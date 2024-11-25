import quart

auth_header_name = 'Sec-X-Authed'

def has_auth(req):
    app = quart.current_app
    if app.config['DEBUG']:
        return True
    # https://stackoverflow.com/questions/4361173/http-headers-in-websockets-client-api
    # okay websockets are a bad API and you can't actually set headers on them
    # so i guess this will only come from reverse proxies
    if req.headers.get(auth_header_name, None):
        return True
    return False

class AuthError(Exception):
    pass

def require_auth(req):
    if not has_auth(req):
        raise AuthError()
