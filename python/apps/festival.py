import quart
import asyncio
import traceback

from util import *

app = quart.Blueprint('festival', __name__, url_prefix='/festival')

@app.websocket("/")
async def festival_full():
    require_auth(quart.session)
    async def festival_api(websocket):
        while True:
            data = await websocket.receive()
            if not data: continue
            yield data
            await websocket.send_json({"type":"scheme","data":data})
    return await festival_selected_websocket(festival_api)

@app.websocket("/tts")
async def festival_tts():
    async def festival_api(websocket):
        yield f"(Parameter.set 'Wavefiletype 'wav)"
        setup = False
        while True:
            data = await websocket.receive()
            if not setup: yield (setup := True)
            if not data: continue
            yield f"(tts_textall {scheme_string(data)} {scheme_string("nil")})"
            await websocket.send_json({"type":"text","data":data})
    return await festival_selected_websocket(festival_api)

@app.websocket("/rich")
async def festival_rich():
    require_auth(quart.websocket)
    async def festival_api(websocket):
        yield f"(Parameter.set 'Wavefiletype 'wav)"
        setup = False
        while True:
            request = await websocket.receive()
            if not request: continue
            cmd = json.loads(request)
            if not cmd or not cmd['type']: continue
            if not setup: yield (setup := True)
            if cmd['type'] == 'text':
                data = str(cmd['data'])
                yield f"(tts_textall {scheme_string(data)} {scheme_string("nil")})"
                await websocket.send_json({"type":"text","data":data})
            elif cmd['type'] == 'scheme':
                data = str(cmd['data'])
                yield data
                await websocket.send_json({"type":"scheme","data":data})
            else:
                await websocket.send_json({"type":"error","data":"Unrecognized command type"})
    return await festival_selected_websocket(festival_api)

async def festival_selected_websocket(festival_selected_api):
    websocket = quart.websocket

    reached_eof = asyncio.Event()
    (reader, writer) = await asyncio.open_connection('localhost', 1314)
    await websocket.accept()
    setup = False

    async def _receive() -> None:
        while not reader.at_eof():
            try:
                async for message in festival_protocol(reader):
                    if setup:
                        await websocket.send_json(message)
            except Exception as e:
                traceback.print_exc()
                raise
        reached_eof.set()

    try:
        task = asyncio.ensure_future(_receive())
        async with until_event(reached_eof) as pending_eof:
            async for data in festival_selected_api(websocket):
                if not data: continue
                if data is True:
                    setup = True
                    continue
                writer.write(data.encode())
                await writer.drain()
    except Exception as e:
        traceback.print_exc()
    finally:
        task.cancel()
        writer.close()
        reader.feed_eof()
        await websocket.close(1014)
        await task
        await writer.wait_closed()

# Parse the protocol returned by the festival server as an async coroutine of
# messages
async def festival_protocol(reader):
    data = b''
    # Ensure we have n bytes, returning them
    async def ensure(n):
        nonlocal data
        while len(data) < n:
            if reader.at_eof(): return b''
            await read_more(n - len(data))
        ret, data = data[:n], data[n:]
        return ret

    # Buffer up to n more bytes
    async def read_more(n):
        nonlocal data
        data += await reader.read(n)

    # Read data encoded in the custom way
    # https://github.com/festvox/speech_tools/blob/master/utils/filetrans.cc#L54
    async def read_data():
        nonlocal data

        festival_key = b'ft_StUfF_key'
        # This is not a good escaping scheme: `key_escaped` itself is not escaped,
        # so it is not injective (it cannot preserve every message)
        key_escaped = b'ft_StUfF_keXy'

        try:
            async with asyncio.timeout(45):
                while (idx := data.find(festival_key)) == -1:
                    if reader.at_eof(): return
                    await read_more(65535)
                # Chop off the data before the key, replacing its halfheartedly escaped
                # version, and leaving the rest after the key in the buffer
                ret, data = data[:idx].replace(key_escaped, festival_key), data[idx+len(festival_key):]
                return ret
        except TimeoutError:
            traceback.print_exc()
            raise

    # Read all incoming data in a loop
    while not reader.at_eof():
        flag = await ensure(3)
        if reader.at_eof(): return
        if flag == b"OK\n":
            yield {"type":"OK"}
            return
        elif flag == b"ER\n":
            # Error has no data
            yield {"type":"ER"}
            return
        elif flag == b"LP\n":
            # Text data
            yield {"type":"LP","data":(await read_data()).decode()}
        elif flag == b"WV\n":
            # Binary data
            yield {"type":"WV","data":base64.b64encode(await read_data()).decode()}
        else:
            raise ValueError(repr((flag+data)[0:9]))
            return
