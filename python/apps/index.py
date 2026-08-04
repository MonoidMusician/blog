import sanic
from sanic import Sanic, response

import os
import watchfiles

from util.auth import *

app = sanic.Blueprint('index', url_prefix='')
common = sanic.Blueprint('common', url_prefix='')

@common.exception(FileNotFoundError)
def file_error(request, err):
    return sanic.response.HTTPResponse("Not found", status=404)

@app.get("/static/<filename:path>")
async def static(request, filename):
    return await sanic.response.file('./static/'+filename)

@app.get("/")
async def index(request):
    return response.redirect('/static/index.html')

@app.get("/assets/<path:path>")
async def assets(request, path):
    return await sanic.response.file('./assets/'+path)


@common.exception(AuthError)
def auth_error(request, err):
    return sanic.response.HTTPResponse("Not authorized", status=401)

@app.get("/auth")
async def auth(request):
    require_auth(request)
    return sanic.response.HTTPResponse("true")


@app.websocket("/watch/<path:path>")
async def watching(request, websocket, path):
    resolved = os.path.abspath(path)
    #print(path, resolved)
    async for i in watchfiles.awatch('.', path, recursive=False, watch_filter=lambda _, changed: changed==resolved):
        #print(i)
        await websocket.send(read_file(path))
