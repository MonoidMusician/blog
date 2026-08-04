import sanic
from sanic import Sanic

import re
from urllib.parse import urlparse

import asyncio

import traceback

from apps import silly
from apps import festival
from apps import index
from apps import redirects


# We want some dynamic routes based on the hostname, most specific first
domain_routes = {
    r'^redirect\.': [redirects.app],
    r'^(a(wa)*\.)+': [silly.awawapp],
    # Default routes! index.app includes the static route
    r'': [index.app, silly.app, festival.app],
}

domain_routing = []
for i, (host_re, blueprints) in enumerate(domain_routes.items()):
    # Create a new app for this domain pattern
    app = Sanic(blueprints[0].name)
    app.config.load_environment_vars()
    app.config["MOTD"] = False
    app.config["TOUCHUP"] = False

    # app.config["DEBUG"] = True
    # app.config["NOISY_EXCEPTIONS"] = True
    # app.state.mode = sanic.application.constants.Mode.DEBUG

    # Register the blueprints on it
    app.blueprint(index.common)
    for bp in blueprints:
        app.blueprint(bp)
    domain_routing.append((re.compile(host_re), app))

# Custom middleware to match domains with regexes to determine custom
# routing rules
def match_domains():
    async def modified(scope, receive, send):
        # Broadcast lifespan events to all apps
        if scope["type"] == "lifespan":
            try:
                await multiplex_lifespan(scope, receive, send)
            except asyncio.CancelledError:
                raise
            except Exception as e:
                print(e)
                print(e.args)
                import traceback
                print(traceback.print_tb(e.__traceback__))
                raise
        # Otherwise select the correct app to handle it
        else:
            # Extract the hostname
            hostname = ""
            for name, value in scope["headers"]:
                name = name.decode("latin1")
                # Look for the host header
                if name.lower() == "host":
                    host = value.decode(errors="surrogateescape")
                    # Parse out just the hostname
                    parsed = urlparse(f"http://{host}")
                    hostname = parsed.hostname
                    break

            # Test it against each of the routing tables! (in order)
            for matching, app in domain_routing:
                if matching.match(hostname):
                    # Finally call the normal ASGI logic for Sanic to handle the request
                    await app(scope, receive, send)
                    break

    async def multiplex_lifespan(scope, receive, send):
        del scope["state"]

        first = True

        # Spawn each app concurrently and communicate with a queue
        def spawn(app):
            q = asyncio.Queue()
            async def s(msg):
                nonlocal first
                if msg["type"].endswith(".failed") and first:
                    await send(msg)
                    first = False
                q.task_done()
            thread = app(scope, q.get, s)
            return (q, asyncio.ensure_future(thread))
        spawned = [
            spawn(app)
            for _, app in domain_routing
        ]

        try:
            while True:
                message = await receive()
                first = True
                # Broadcast to all threads
                for q, _ in spawned:
                    await q.put(message)
                await q.join() # Wait for all threads

                # Fill in the completed message
                if first:
                    await send({"type": f"{message["type"]}.complete"})

                # Wait for shutdown
                if message["type"] == "lifespan.shutdown":
                    break
        except CancelledError as e:
            for _, thread in spawned:
                thread.cancel(e)
            raise

        for _, thread in spawned:
            await thread

    return modified

app = match_domains()
