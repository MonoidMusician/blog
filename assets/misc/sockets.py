import os
_start_ppid = os.getppid()

import sys
import asyncio
import signal
import contextlib
import inspect
import re
import json
import shlex
import urllib
import random
import socket

# Creates a server on a random free port, returns the port, the passwords used
# for authentication on either end, and an async context manager that waits
# for a caller with the right password, then stops accepting new connections
# and cleans up the connection when the context exits, or cancels it when
# the client exits
async def alloc_server(host='127.0.0.1'):
    # basic handshake protocol
    passwords = (random.randbytes(32).hex(), random.randbytes(32).hex())

    # how to wait for the right client to connect
    right_client: Optional[Tuple[asyncio.StreamReader, asyncio.StreamWriter]] = None
    got_client = asyncio.Event()
    async def check_right_client(reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        nonlocal right_client
        # Check for a client who knows the passcode
        # https://docs.python.org/3/library/asyncio-stream.html#asyncio.StreamWriter.get_extra_info
        # https://docs.python.org/3/library/asyncio-protocol.html#asyncio.BaseTransport.get_extra_info
        # https://docs.python.org/3/library/socket.html#socket.socket.getpeername
        if writer.get_extra_info('peername')[0] == '127.0.0.1':
            # https://docs.python.org/3/library/asyncio-stream.html#asyncio.StreamReader.readexactly
            if await reader.readexactly(len(passwords[0])) == passwords[0].encode():
                # And reassure them that we do too
                # https://docs.python.org/3/library/asyncio-stream.html#asyncio.StreamWriter.write
                writer.write(passwords[1].encode())
                await writer.drain()
                # ... i guess hope that they stay connected here?
                right_client = (reader, writer)
                got_client.set()
                return
        writer.close()
        await writer.wait_closed()

    # https://docs.python.org/3/library/asyncio-stream.html#asyncio.start_server
    # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.loop.create_server
    # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.Server
    # https://docs.python.org/3/library/socket.html#socket.socket
    server = await asyncio.start_server(
        check_right_client,
        host=host, port=None,
        family=socket.AF_INET,
        # dualstack_ipv6=False, # ??
        start_serving=False,
    )
    # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.Server.sockets
    # https://docs.python.org/3/library/socket.html#socket.socket.getsockname
    port = server.sockets[0].getsockname()[1]

    # wait for the right client and then do stuff while it stays open and close it when done
    @contextlib.asynccontextmanager
    async def with_right_client():
        async with server:
            # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.Server.start_serving
            await server.start_serving()
            await got_client.wait()
            # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.Server.close
            server.close() # "leaves existing connections open"
            try:
                yield right_client
            finally:
                right_client[1].close()
                await right_client[1].wait_closed()

    return (port, passwords), with_right_client

# The counterpart to `alloc_server()[2]`: given `alloc_server()[:2]`, it
# constructs the matching client
@contextlib.asynccontextmanager
async def connect_client(port_and_passwords, host='127.0.0.1'):
    port, passwords = port_and_passwords
    # https://docs.python.org/3/library/asyncio-stream.html#asyncio.open_connection
    # https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.loop.create_connection
    # https://docs.python.org/3/library/socket.html#socket.socket
    (reader, writer) = await asyncio.open_connection(
        host=host,
        port=port,
        family=socket.AF_INET,
    )
    writer.write(passwords[0].encode())
    await writer.drain()
    returned = await reader.readexactly(len(passwords[1]))
    assert returned == passwords[1].encode()
    try:
        yield (reader, writer)
    finally:
        writer.close()
        await writer.wait_closed()

async def main_server(with_right_client):
    async with with_right_client() as (reader, writer):
        await writer.drain()
        writer.write('"from server"\n'.encode())
        print('from client', (await reader.readuntil()).decode().strip())

async def main_client(port_and_passwords):
    async with connect_client(port_and_passwords) as (reader, writer):
        writer.write('"from client"\n'.encode())
        print('from server', (await reader.readuntil()).decode().strip())
        await writer.drain()

async def main():
    port_and_passwords, with_right_client = await alloc_server()
    await asyncio.gather(main_server(with_right_client), main_client(port_and_passwords))


class MuxJSON:
    """Handles bidirectional newline-delimited JSON, muxed by singleton key or custom matcher."""
    def __init__(self, reader, writer):
        self.reader = reader
        self.writer = writer

        self.id = 0
        self.needs_reader = asyncio.Event()
        self.readers = dict()
        self.handled = True
        self.backlog = []

        self.is_running = False
        self.stopped = asyncio.Event()
    async def __aenter__(self):
        async def loop():
            try:
                async with until_event(self.stopped):
                    while self.is_running:
                        # Always drain the write queue first
                        await self.writer.drain()
                        # Then wait for a message to come in
                        encoded = await self.reader.readline()
                        if not encoded:
                            # EOF
                            self.is_running = False
                            self.stopped.set()
                            return
                        decoded = json.loads(encoded.decode())
                        while not len(self.readers):
                            await self.needs_reader.wait()
                            self.needs_reader = asyncio.Event()
                        self.handled = False
                        for matcher, reader in list(self.readers.values()):
                            try:
                                matched = matcher(decoded)
                                if matched is not ValueError and not isinstance(matched, ValueError):
                                    reader(matched)
                                    self.handled = True
                            except: continue
                        if not self.handled:
                            self.backlog.append(decoded)
            except asyncio.CancelledError:
                pass
        self.is_running = True
        self.running = asyncio.ensure_future(loop())
        return self
    async def __aexit__(self, exc_type, exc_value, traceback):
        self.is_running = False
        self.stopped.set()
        self.running.cancel()
    def __aiter__(self):
        return self
    async def __anext__(self):
        return await self.read1()
    @staticmethod
    def matcher(mux_name_or_matcher=None, mux_value=None):
        if mux_name_or_matcher is None:
            if mux_value is None:
                return lambda v: v
            else:
                return lambda decoded: decoded if decoded == value else ValueError
        if type(mux_name_or_matcher) is not str:
            assert callable(mux_name_or_matcher)
            assert mux_value is None
            return mux_name_or_matcher
        def match_name(decoded):
            if len(decoded) == 1:
                if type(decoded) is dict and len(decoded) == 1:
                    k, v = list(decoded.items())[0]
                    if k == mux_name_or_matcher:
                        if mux_value is None or v == mux_value:
                            return v
            raise ValueError
        return match_name
    async def read1(self, mux_name_or_matcher=None, mux_value=None):
        matcher = MuxJSON.matcher(mux_name_or_matcher, mux_value)
        # Check the backlog first
        for replay in self.backlog:
            try:
                matched = matcher(replay)
                if matched is ValueError or isinstance(matched, ValueError):
                    raise matched
            except ValueError: continue
            # Pause for a breath, in case another compatible handler is added
            # synchronously
            breath = asyncio.Event()
            asyncio.get_event_loop().call_soon(breath.set)
            await breath.wait()
            # Make sure we were the first to handle it
            for stillpresent in self.backlog:
                # by checking identity not equality
                if stillpresent is replay:
                    self.backlog.remove(replay)
                    break
            return matched
        # Otherwise wait for something new to come in
        try:
            async with until_event(self.stopped) as make_impervious:
                waiting = asyncio.Event()
                fulfilled = None
                i = self.id
                self.id += 1
                def reader(matched):
                    nonlocal fulfilled
                    fulfilled = matched
                    del self.readers[i]
                    make_impervious()
                    waiting.set()
                self.readers[i] = matcher, reader
                self.needs_reader.set()
                await waiting.wait()
                return fulfilled
        except asyncio.CancelledError:
            raise StopAsyncIteration
    async def reading(self, mux_name_or_matcher=None, mux_value=None):
        matcher = MuxJSON.matcher(mux_name_or_matcher, mux_value)
        while self.is_running:
            yield await read1(matcher)
    @staticmethod
    def encode(muxed):
        return json.dumps(muxed).encode()+b'\n'
    def write_and_forget(self, muxed):
        assert self.is_running
        self.writer.write(MuxJSON.encode(muxed))
    def writing(self, mux_name, value=Ellipsis):
        if value is Ellipsis: return lambda value: self.writing(mux_name, value)
        self.writer.write(MuxJSON.encode({ mux_name: value }))
        return self.writer.drain()
    async def __getitem__(self, mux_name):
        # muxed[...] = muxed.read1()
        if mux_name is Ellipsis:
            return await self.read1()
        elif type(mux_name) is slice:
            # muxed[mux_name::mux_value] = muxed.read1(mux_name, mux_value)
            if mux_name.step is not None:
                assert mux_name.stop is None
                return await self.read1(mux_name.start, mux_name.step)
            # muxed[mux_name:mux_value] = muxed.writing(mux_name, mux_value)
            else:
                assert mux_name.step is None
                await self.writing(mux_name.start, mux_name.stop)
                return None
        else:
            # muxed[mux_name] = muxed.read1(mux_name)
            return await self.read1(mux_name)
    def __setitem__(self, mux_name, mux_value):
        self.write_and_forget({ mux_name: mux_value })


async def muxed_server(with_right_client):
    async with with_right_client() as muxed:
        # print the first message, regardless of what JSON it is
        print('from client:', await muxed[...])
        # send two messages (automatically drained)
        muxed["hello"] = "world"
        muxed["hi"] = "you"
        # queue the last message for 0.6 seconds
        await asyncio.sleep(0.6)
        print('from client 2:', await muxed["hello"])
        try:
            print('from client 3:', await muxed[...])
        except StopAsyncIteration:
            print('client had nothing more to say!')

async def muxed_client(port_and_passwords):
    async with connect_mux_client(port_and_passwords) as muxed:
        # send the first message (automatically drained)
        muxed["hello"] = "there"
        # await a specific message: {"hello": "world"}
        print('from server:', await muxed.read1("hello", "world"))
        # send a reply soon
        await asyncio.sleep(0.1)
        muxed["hello"] = "again"
        # finally handle this message that has been waiting since hello=world
        print('from server (backlogged):', await muxed["hi"])
        # print('from server 3', await muxed[...])

async def muxed_main():
    port_and_passwords, with_right_client = await alloc_mux_server()
    await asyncio.gather(muxed_server(with_right_client), muxed_client(port_and_passwords))

async def alloc_mux_server(*args, **kwargs):
    port_and_passwords, with_server = await alloc_server(*args, **kwargs)
    @contextlib.asynccontextmanager
    async def with_mux_server():
        async with with_server() as server:
            async with MuxJSON(*server) as muxed:
                yield muxed
    return port_and_passwords, with_mux_server

@contextlib.asynccontextmanager
async def connect_mux_client(*args, **kwargs):
    async with connect_client(*args, **kwargs) as client:
        async with MuxJSON(*client) as muxed:
            yield muxed


# If we have ssh and ffmpeg running and one terminates, use this event to
# terminate the other process (using `until_event` in `monitor_subprocess`)
please_exit_everything = asyncio.Event()

@contextlib.asynccontextmanager
async def until_awaitable(awaitable, loop=None):
    """Run the context until the awaitable returns: provides a function to, uhm,
    cancel the canceler (exit the context early, essentially)."""
    current_task = asyncio.current_task(loop)

    async def cancel_when_completes():
        try:
            await awaitable
        except asyncio.CancelledError:
            return
        current_task.cancel()
    cancel_task = asyncio.create_task(cancel_when_completes())
    try:
        yield cancel_task.cancel
    finally:
        cancel_task.cancel()

@contextlib.asynccontextmanager
async def until_event(event, loop=None):
    """A context manager to cancel the context when an event fires"""
    if event.is_set(): raise asyncio.CancelledError
    async with until_awaitable(event.wait(), loop) as canceler:
        yield canceler

async def monitor_subprocess(processpromise, *actions, cleanup=None, name=None, exit_everything=True):
    """This monitors a subprocess to see if it goes away, and to make sure it goes away"""
    process = None
    try:
        async with until_event(please_exit_everything):
            process = await processpromise
            if process is None: return # for dry-run
            for action in actions:
                await action(process)
            await process.wait()
            #print("process exited already", name)
            if exit_everything:
                please_exit_everything.set()
    except (KeyboardInterrupt, asyncio.exceptions.CancelledError):
        if process is None: return
        if cleanup is not None:
            await cleanup(process)
        try:
            process.terminate()
            async with asyncio.timeout(1):
                await process.wait()
                #print("exit after terminate()", name)
        except asyncio.TimeoutError:
            process.kill()
            #print("1s timeout failed", name)
            await process.wait()
            #print("exit after kill()", name)
        except ProcessLookupError:
            pass # must already be gone?

if __name__ == '__main__':
    asyncio.run(muxed_main())
    sys.exit(0)
