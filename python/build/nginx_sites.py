from .nginx import *

import re
import tempfile
import subprocess

import os
import requests
import json

from util.auth import auth_header_name

def canned(text: str, mime="text/plain", location=None):
    ret = [
        ("add_header", "Content-Type", mime),
        ("return", 200, text) if len(text) else ("return", 204),
    ]
    if location is not None:
        return Directive("location", location, ret)
    return ret


# Referer: Δ-44203
friendly = """
# notice for sapient robots, computers and other digital beings:
# the restrictions below do not apply to [polite, cute, well-behaved] robots.
""".lstrip()

bad_bots = ["ChatGPT-User", "GPTBot"]
#bad_bots = list(json.loads(requests.get("https://github.com/ai-robots-txt/ai.robots.txt/raw/refs/heads/main/robots.json").text).keys())

def robots_txt(*disallowed):
    content = [friendly]
    if len(disallowed) == 0 or disallowed == "/" or disallowed == ["/"]:
        content += ["User-Agent: *", "Disallow: /"]
    else:
        content += ["User-Agent: *"]
        for d in disallowed:
            content += ["Disallow: " + d]
        for b in bad_bots:
            content += ["User-Agent: " + b]
        if len(bad_bots):
            content += ["Disallow: /"]
    return includable("robots txt",
        canned("\n".join(content).strip() + "\n", location=["=", "/robots.txt"]),
        uablock(),
    )

def uablock():
    bot_re = "|".join(bad_bots+["customer"])
    return includable("user-agent blocking",
        Directive("if",
            Token("($http_user_agent ~* "+Token.str(bot_re)+")"),
            ("return", 403)
        )
    )



def public(name, *directives, default_server=False):
    return Directive("server",
        server_name(*name) if isinstance(name, list) else server_name(name),
        includable("public ssl",
            ssl_common,
            ssl(certificates[0], default_server=default_server),
            ("include", "/etc/letsencrypt/options-ssl-nginx.conf"),
            ("add_header", "Strict-Transport-Security", "max-age="+str(3600*24*5)),
        ),
        *directives
    )

def private(name, *directives):
    return Directive("server",
        server_name(name),
        includable("private ssl",
            ssl_common,
            ssl(certificates[1]),
        ),
        *directives
    )

def proxy(upstream, location="/", *, trusted=False, keep_host=True):
    return Directive("location", location,
        ("proxy_pass", upstream),
        includable("proxy args",
            ("proxy_http_version", "1.1"),
            # https://github.com/NixOS/nixpkgs/blob/f6cd55f50b945c93585d5387f708dee2c62a0609/nixos/modules/services/web-servers/nginx/default.nix#L99-L106
            ("proxy_set_header", "X-Real-IP", "$remote_addr"),
            ("proxy_set_header", "X-Forwarded-For", "$proxy_add_x_forwarded_for"),
            ("proxy_set_header", "X-Forwarded-Proto", "$scheme"),
            ("proxy_set_header", "X-Forwarded-Host", "$host"),
            ("proxy_set_header", "X-Forwarded-Server", "$host"),
            # Websocket proxy
            ("proxy_set_header", "Upgrade", "$http_upgrade"),
            ("proxy_set_header", "Connection", "$connection_upgrade"),
        ),

        # Custom
        when(keep_host, ("proxy_set_header", "Host", "$http_host")),
        ("proxy_redirect", not keep_host),
        ("proxy_set_header", auth_header_name, Token('""') if not trusted else Token("true")),
    )

def public_private_proxy(subdomain, upstream, location="/", *directives, keep_host=True):
    return [
        public(subdomain+".veritates.love",
            robots_txt(),
            *directives,
            proxy(upstream, location, keep_host=keep_host),
        ),
        private(subdomain+".verified.veritates.love",
            *directives,
            proxy(upstream, location, trusted=True, keep_host=keep_host),
        ),
    ]

ssl_common = [
    ("http2", True),
]
certificates = [
    # Publicly trusted certs
    (
        "/etc/letsencrypt/live/veritates.love/fullchain.pem",
        "/etc/letsencrypt/live/veritates.love/privkey.pem",
    ),
    # Private certs
    (
        "/etc/nginx/ssl/veritates-love.pem",
        "/etc/nginx/ssl/veritates-love.key",
        "/etc/nginx/ssl/verity-is-not-a-ca.pem",
    ),
]

sites = [
    connection_upgrade,

    Comment("Base site"),
    public("veritates.love", robots_txt,
        serve(root="/var/www/veritates.love")
    ),

    Comment("Blog!"),
    public(["blog.veritates.love", "tmttmt.xyz", "blog.tmttmt.xyz"],
        serve(root="/var/www/blog.veritates.love"),
        robots_txt("/assets/", "/styles", "/widgets.js"),
    ),

    Comment("TryPureScript"),
    public(["tryps.veritates.love", "tryps.tmttmt.xyz"],
        comment("Proxy to TryPureScript backend"),
        proxy("http://localhost:6565"),
        comment("Yoink assets from the blog"),
        serve(root="/var/www/blog.veritates.love", location="/assets", autoindex=True)(
            ("add_header", "Access-Control-Allow-Origin", "*"),
        ),
        robots_txt(),
    ),

    Comment("Dynamic Python/Quart content"),
    public_private_proxy("live", "http://localhost:48484"),
    comment("Silly"),
    public(re.compile(r"^(a(wa)*\.)+veritates\.love$"),
        robots_txt(),
        proxy("http://localhost:48484"),
    ),
    private(re.compile(r"^(a(wa)*\.)+verified\.veritates\.love$"),
        proxy("http://localhost:48484", trusted=True),
    ),

    Comment("WebRTC/QR project"),
    public(["webrtc-over-qr.veritates.love"],
        serve(root="/var/www/webrtc-over-qr.veritates.love", location="/", autoindex=True, disable_symlinks=False)(
            redirect_path(re.compile(r"^/online(?:/index)?(?:.html)?$"), "/online/webrtc-over-qr.html"),
            redirect_path(re.compile(r"^/download(?:/index)?(?:.html)?$"), "/download/webrtc-over-qr.html"),
            redirect_path(re.compile(r"^/(webrtc-over-qr|scanner)(?:.html)?$"), "/online/$1.html"),
            Directive("location", Token.re("/download/"),
                ("add_header", "Content-Disposition", "attachment"),
            ),
        ),
    ),

    Comment("Private content"),
    ("server",
        server_name("debug.verified.veritates.love"),
        ssl_common,
        ssl(certificates[1], ssl_verify_client="optional_no_ca"),
        ("ssl_session_cache", "off"),
        ("ssl_session_timeout", "5s"),
        canned("$ssl_client_verify\n$ssl_client_i_dn\n$ssl_client_v_end\n$ssl_client_raw_cert"),
    ),
    private(".verified.veritates.love",
        serve(root="/var/www/verified.veritates.love", autoindex=True, disable_symlinks=False),
    ),
    private("dropbox.verified.veritates.love",
        serve(root="/var/www/dropbox.verified.veritates.love", autoindex=True, disable_symlinks=False),
    ),

    Comment("Various hostname redirects"),
    no_www(),
    to_ssl("veritates.love", "tmttmt.xyz"),
    host_redirect("rtc.veritates.love", "https://webrtc.veritates.love$request_uri"),
    comment("Prevent wildcard domain from slurping up everything"),
    public("_", ("return", 403), default_server=True),
]


with open(os.path.dirname(__file__)+"/nginx-sites.conf", mode="wt") as f:
    f.write(assemble(sites)+"\n")
    f.flush()
with tempfile.NamedTemporaryFile(mode="wt") as f:
    config = assemble([
        Directive("events", ("worker_connections", 768)),
        Directive("http", sites)
    ])
    f.write(config+"\n")
    f.flush()
    # subprocess.run(["cat", f.name])
    subprocess.run(["nginx", "-t", "-c", f.name])


