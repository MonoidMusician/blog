import quart
from quart import Quart
from werkzeug.datastructures import Headers
from hypercorn.typing import Scope

import re
from urllib.parse import urlparse

from apps import silly
from apps import festival
from apps import index

# Remove the static route
app = Quart(__name__, static_folder=None)
app.config.from_prefixed_env()

# We want some dynamic routes based on the hostname, most specific first
domain_routes = {
    r'^(a(wa)*\.)+': [silly.awawapp],
    # Default routes! index.app includes the static route
    r'': [index.app, silly.app, festival.app],
}

# Build distinct `url_map`s for each of these route combinations
domain_routing = []
for host_re, blueprints in domain_routes.items():
    # Reset the registered blueprints to avoid errors
    app.blueprints = {}
    # Create a new `url_map`
    app.url_map = app.url_map_class(host_matching=False)
    for bp in blueprints:
        app.register_blueprint(bp)
    domain_routing.append((re.compile(host_re), app.url_map))

# Custom middleware to match domains with regexes to determine custom
# routing rules
def match_domains(asgi_app):
    async def modified(scope: Scope, receive, send):
        if scope['type'] != 'lifespan':
            # Copy over some header handling ...
            headers = Headers()
            for name, value in scope["headers"]:
                headers.add(name.decode("latin1").title(), value.decode("latin1"))
            # Parse out just the hostname
            parsed = urlparse(f"{scope['scheme']}://{headers.get('Host')}")
            hostname = parsed.hostname

            # Test it against each of the routing tables! (in order)
            for matching, url_map in domain_routing:
                if matching.match(hostname):
                    # Swap out the `url_map`
                    app.url_map = url_map
                    break
        # Finally call the normal ASGI logic for Quart to handle the request
        return await asgi_app(scope, receive, send)
    return modified

app.asgi_app = match_domains(app.asgi_app)
