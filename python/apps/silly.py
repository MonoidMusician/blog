import sanic
import asyncio
import random

from util import *

app = sanic.Blueprint('silly', url_prefix='')

# Echoes messages back to the sender
@app.websocket("/echo")
async def echo(request, websocket):
    print(request.transport.get_websocket_connection())
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
async def broadcast(request, websocket):
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


@app.route('/awawa')
async def awawa(request, path: str = ''):
    host_parts = request.host.split(".")
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
    return sanic.response.HTTPResponse(message, headers={
        "X-AWAWA": str(count),
        "X-Powered-By": "catgirls & love"
    })

awawapp = sanic.Blueprint('awawa', url_prefix='')
awawapp.add_route(awawa, '/', name='awawa_root')
awawapp.add_route(awawa, '/<path:path>')
