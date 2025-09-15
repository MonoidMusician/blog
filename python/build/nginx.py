# Generate some nginx configuration I guess
# Slightly opinionated but should be reusable

import re
import tempfile
import subprocess

from util.pure import replacers

class Token(str):
    @staticmethod
    def escape(s):
        return '"'+replacers([
            ("\\", "\\\\"),
            ("\"", "\\\""),
            ("\r", "\\r"),
            ("\n", "\\n"),
            ("\t", "\\t"),
        ])(s)+'"'
    @staticmethod
    def str(s):
        if isinstance(s, Token): return s
        if isinstance(s, re.Pattern): s = s.pattern
        if s is True: s = "on"
        if s is False: s = "off"
        s = str(s)
        if "" == s or set(s).intersection(set(" \r\n\t\"\'{}()")):
            return Token(Token.escape(s))
        return Token(s)
    # Tilde and tilde-star notation for regexes
    @staticmethod
    def re(s):
        if isinstance(s, re.Pattern):
            pre = "~"
            if s.flags & re.IGNORECASE:
                pre += "*"
            return Token.str(pre + s.pattern)
        return Token.str(s)
    @staticmethod
    def comment(s: str):
        lines = s.split("\n")
        if len(lines) == 1:
            return Token("# " + s)
        return [Token("# " + l if not l.isspace() else "#") for l in lines]
comment = Token.comment
def Comment(s):
    return [Token(), Token.comment(s)]
# Directive, list, str (and tuple -> directive)
class Directive:
    scalars = [str, int, float, bool, re.Pattern, Token]
    @staticmethod
    def is_scalar(arg):
        for t in Directive.scalars:
            if isinstance(arg, t):
                return True
        return False
    @staticmethod
    def flatten(args):
        ret = []
        if callable(args): args = args()
        for arg in args:
            if callable(arg): arg = arg()
            if isinstance(arg, tuple):
                if not len(arg):
                    continue
                arg = Directive(*arg)
            if isinstance(arg, Directive) or Directive.is_scalar(arg):
                ret.append(arg)
            else:
                ret.extend(Directive.flatten(arg))
        return ret
    def __init__(self, name, *args):
        args = Directive.flatten(args)
        self.name = name
        self.params = []
        self.children = []
        for arg in args:
            if not len(self.children) and Directive.is_scalar(arg):
                self.params.append(arg)
            else:
                self.children.append(arg)
    def __str__(self):
        if isinstance(self, list):
            return "\n".join(str(item) for item in self)
        if self.name is None: return Token("")
        parts = [Token.str(self.name)]
        parts.extend(Token.str(p) for p in self.params)
        if len(self.children):
            block = []
            block.append(Token("{"))
            for child in self.children:
                if not isinstance(child, Directive) or child.name is not None:
                    block.append('    ' + str(child).replace('\n', '\n    '))
            block.append(Token("}"))
            parts.append("\n".join(block))
            return " ".join(parts)
        else:
            return " ".join(parts)+Token(";")

def includable(name, *content):
    return list(content)

def assemble(*args):
    return Directive.__str__(Directive.flatten(list(args)))

def nginx_map(expr, var, *items, **opts):
    return Directive("map", expr, var, nginx_map_body(*items, **opts))
def nginx_map_body(*items, default=None, hostnames=False, volatile=False):
    if hostnames:
        yield Directive("hostnames")
    if volatile:
        yield Directive("volatile")
    if default is not None:
        yield Directive("default", default)
    for item, value in items:
        if item in ["hostnames", "volatile", "default", "include"]:
            item = "\\"+item
        yield Directive(Token.re(item), value)

# https://nginx.org/en/docs/http/websocket.html
# https://www.f5.com/company/blog/nginx/avoiding-top-10-nginx-configuration-mistakes#no-keepalives
connection_upgrade =\
    nginx_map("$http_upgrade", "$connection_upgrade",
        ("", ""), # or "close", but ... keepalive?
        default="upgrade"
    )

def http(port=80, default_server=False):
    port = str(port)
    return [("listen", port, when(default_server, "default_server")), ("listen", "[::]:"+port)]
def https(port=443, default_server=False):
    port = str(port)
    return [("listen", port, "ssl", when(default_server, "default_server")), ("listen", "[::]:"+port, "ssl")]

def server_name(*domains):
    return Directive("server_name",
        (Token.re(d) for d in domains)
    )

# Redirect `http://:80` to `https://:443`
def to_ssl(*domains, default_server=True, code=301):
    return Directive("server",
        http(default_server=default_server),
        server_name(*domains),
        ("return", code, "https://$host$request_uri"),
    )
# Redirect to base domain without `www.`
def no_www():
    return host_redirect(
        re.compile(r"^(?:www\.)+(.+)$"),
        "https://$1$request_uri",
    )
# Redirect from a hostname (may be a regex) to a replacement (may use regex matches)
def host_redirect(match, to, *directives, code=301):
    if not len(directives):
        directives = [http, https]
    return Directive("server",
        *directives,
        server_name(match),
        ("return", code, to),
    )
def redirect_path(location, to, *directives, code=301):
    return Directive("location", Token.re(location),
        *directives,
        ("return", code, to),
    )

# Takes a public certificate, the corresponding private key, and optionally
# a client certificate if you want client authentication on that virtual server.
# (AFAIK, browsers will only allow selecting client certs from the same CA, so
# that is why it is bundled with a self-signed server cert)
def ssl(cert_bundle, port=443, default_server=False, ssl_verify_client=True):
    yield https(port=port, default_server=default_server)
    yield ("ssl_certificate", cert_bundle[0])
    yield ("ssl_certificate_key", cert_bundle[1])
    if len(cert_bundle) == 2:
        return
    yield ("ssl_client_certificate", cert_bundle[2])
    yield ("ssl_verify_client", ssl_verify_client)
    yield ("ssl_stapling", False)
    yield ("ssl_verify_depth", 2)

def serve(root, location="/", index="index.html", dir=True, autoindex=None, disable_symlinks=None):
    def _(*directives):
        return Directive("location", Token.re(location),
            ("root", root),
            ("index", index),
            when(autoindex is not None, ("autoindex", autoindex)),
            when(disable_symlinks is not None, ("disable_symlinks", disable_symlinks)),
            *directives,
            # First attempt to serve request as file, then
            # as directory, then fall back to displaying a 404.
            ("try_files", "$uri", *when(dir, "$uri/"), "=404"),
        )
    return _

def when(condition, *items):
    if condition:
        return list(items)
    return []

