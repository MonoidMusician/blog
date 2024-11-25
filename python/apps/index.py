import quart
from quart import Quart, websocket
from werkzeug.security import safe_join

import os
import watchfiles

from util.auth import *

app = quart.Blueprint('index', __name__, url_prefix='')

@app.get("static/<path:filename>")
async def static(filename):
    return await quart.send_from_directory("static", filename)

@app.get("/")
async def index():
    return quart.redirect('/static/index.html')

@app.get("/assets/<path:path>")
async def assets(path):
    return await quart.send_from_directory('./assets', path)


@app.errorhandler(AuthError)
def auth_error(err):
    return ("Not authorized", 401)

@app.get("/auth")
async def auth():
    require_auth(quart.request)
    return "true"


@app.websocket("/watch/<path:path>")
async def watching(path):
    await websocket.accept()
    resolved = os.path.abspath(path)
    #print(path, resolved)
    async for i in watchfiles.awatch('.', path, recursive=False, watch_filter=lambda _, changed: changed==resolved):
        #print(i)
        await websocket.send(read_file(path))
