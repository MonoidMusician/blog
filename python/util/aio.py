import asyncio
import aiofiles
import aiohttp
import collections
import json
import os
import signal
import shutil
import subprocess
import watchfiles
import re
import traceback
import base64
import contextlib

from .pure import *


def read_file(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return f.read()


def read_json(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        return json.load(f, object_pairs_hook=collections.OrderedDict)


async def fetch(url):
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.text()


@contextlib.asynccontextmanager
async def until_event(event, loop=None):
    current_task = asyncio.current_task(loop)
    async def cancel_when_terminated():
        await event.wait()
        current_task.cancel()
    cancel_task = asyncio.create_task(cancel_when_terminated())
    try:
        yield cancel_task
    finally:
        cancel_task.cancel()



async def race(*args):
    completed, pending = await asyncio.wait(
        args,
        return_when=asyncio.FIRST_COMPLETED
    )
    try:
        done = the(completed)
        for p in pending:
            p.cancel()
        return args.index(done), await done
    except NotUnique:
        raise Exception("AWAWAWA", done, pending)
