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

class DomainRouter(sanic.router.Router):
    domains = []
    app = None

    def get(self, path: str, method: str, host: str | None):
        for matching, router in self.domains:
            if matching.match(host or ''):
                if self.app is not None:
                    _saved = self.app.router
                    self.app.router = router
                    try:
                        result = router.get(path, method, host)
                    finally:
                        self.app.router = _saved
                    return result
                else:
                    return router.get(path, method, host)
        return super().get(path, method, host)



domain_router = DomainRouter()

app = Sanic("app", router=domain_router)
app.config.load_environment_vars()
app.config["MOTD"] = False
app.config["TOUCHUP"] = False

# app.config["DEBUG"] = True
# app.config["NOISY_EXCEPTIONS"] = True
# app.state.mode = sanic.application.constants.Mode.DEBUG

domain_router.app = app

@app.get("/sorry")
def sorry(req): return sanic.response.HTTPResponse("sorry", status=404)

# app.blueprint(index.common)

for i, (host_re, blueprints) in enumerate(domain_routes.items()):
    _saved = app._future_registry
    app._future_registry = type(app._future_registry)()

    # Create a new router for this domain pattern
    app.router = sanic.router.Router()

    # Register the blueprints on it
    app.blueprint(index.common)
    for bp in blueprints:
        app.blueprint(bp)

    app.finalize()
    domain_router.domains.append((re.compile(host_re), app.router))

    # And on the main router
    app.router = domain_router
    app._future_registry = _saved
    # for bp in blueprints:
    #     del app.blueprints[bp.name]
    #     app.blueprint(bp)
