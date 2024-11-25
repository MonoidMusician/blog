import quart
import asyncio
from quart import Quart, websocket, request
import random

from util import *

app = quart.Blueprint('silly', __name__, url_prefix='')

# Echoes messages back to the sender
@app.websocket("/echo")
async def echo():
    try:
        while True:
            data = await websocket.receive()
            await websocket.send(data)
    except asyncio.CancelledError:
        # Handle disconnection here
        raise



broadcast_clients = []

# Broadcasts messages to all connected clients
@app.websocket("/broadcast")
async def broadcast():
    client = asyncio.Queue()
    broadcast_clients.append(client)
    try:
        while True:
            from_user, data = await race(
                asyncio.create_task(client.get()),
                asyncio.create_task(websocket.receive()),
            )
            await websocket.send(data)
            if from_user:
                for other in broadcast_clients:
                    if other == client: continue
                    other.put_nowait(data)
    except asyncio.CancelledError:
        # Handle disconnection here
        raise
    finally:
        broadcast_clients.remove(client)


@app.route('/awawa', defaults={'path': ''})
async def awawa(path: str):
    host_parts = request.host_url.split(".")
    domain = ".".join(host_parts[-2:]) if len(host_parts) > 2 else ".".join(host_parts)
    seed = request.url.replace(domain, "")
    count_a = seed.count("a")
    count_w = seed.count("w")
    count = min(count_a - 1, count_w)
    aww = random.randrange(8) > 0
    if count == 2 or random.randrange(128) == 0:
        count = random.randrange(128)
        aww = random.randrange(32) > 0
    message = ("a"+count*"wa") if aww else ("a"+count*"a"+count*"w")
    resp = await quart.make_response(message)
    resp.headers["X-AWAWA"] = str(count)
    resp.headers["X-Powered-By"] = "catgirls & love"
    return resp

awawapp = quart.Blueprint('awawa', __name__, url_prefix='')
awawapp.add_url_rule('/', None, awawa, defaults={'path': ''})
awawapp.add_url_rule('/<path:path>', None, awawa)
