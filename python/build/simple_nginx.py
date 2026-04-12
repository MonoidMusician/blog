from .nginx import *

import re
from tempfile import TemporaryDirectory
import subprocess

import os
import requests
import json

port = "8998"
run = True

with TemporaryDirectory(suffix=f".nginx_{port}") as realtempdir:
    temp = (lambda base: lambda sub: base+sub)(realtempdir+"/" if run else f"/tmp/nginx_{port}_")

    CORS_headers = {
        'Access-Control-Allow-Origin': [Token.var("http_origin"), Token.always],
        #'Access-Control-Allow-Private-Network': 'true',
        'Access-Control-Allow-Methods': 'GET,HEAD,OPTIONS',
        'Access-Control-Allow-Headers': 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range',
        'Access-Control-Expose-Headers': ['Content-Length,Content-Range', Token.always],
    }
    allowCORS = [
        comment("Always allow CORS requests"),
        by_request_method({
            'OPTIONS': [
                add_headers({
                    **CORS_headers,
                    # valid for 20 seconds
                    'Access-Control-Max-Age': 20,
                }),
                canned(""),
            ],
            'HEAD': add_headers(CORS_headers),
            'GET': add_headers(CORS_headers),
        }),
    ]

    config = assemble([
        comment(f"""
        Simple nginx configuration for serving static files from
        the specified directory onto port {port}.

        Run with:
            nginx -c "$(realpath simple_nginx.conf)" -p "$PWD" -e stderr
        to serve files from the current directory ($PWD)
        """[1:].rstrip()),

        "",
        ("worker_processes", 1),
        "",
        ("error_log", os.devnull, "emerg"),
        ("error_log", "stderr", comment("comment out this line if you do not want error logging")),

        "",
        comment("do not detach the process from the terminal,\nwhich means it stops on ctrl-c"),
        ("daemon", False),
        comment("thus a PID file is unnecessary"),
        ("pid", os.devnull),

        "",
        Directive("events", Body()),
        "",
        Directive("http", [
            Comment("turn off logging"),
            ("access_log", False),

            Comment("temporary files"),
            ("client_body_temp_path", temp("client_body")+"/"),
            ("fastcgi_temp_path", temp("fastcgi")+"/"),
            ("proxy_temp_path", temp("proxy")+"/"),
            ("scgi_temp_path", temp("scgi")+"/"),
            ("uwsgi_temp_path", temp("uwsgi")+"/"),

            "",
            ("sendfile",            True),
            ("tcp_nopush",          True),
            ("keepalive_timeout",   65),
            ("types_hash_max_size", 4096),

            Comment("mapping of file types to MIME types", "(use [.] globbing to allow the file to not exist)"),
            ("include", "/etc/nginx/mime[.]types"),
            ("include", "/usr/local/etc/nginx/mime[.]types"),
            ("default_type", "application/octet-stream", comment("(default to binary data)")),

            "",
            Directive("server", [
                comment("port number"),
                ("listen", f"{port}"),
                ("listen", f"[::]:{port}"),
                "",
                ("http2", True),
                ("server_name", "localhost"),
                "",
                Directive("location", "/", [
                    comment("use whatever the value of -p is"),
                    ("root", "./"),
                    comment("serve local files or return 404"),
                    ("try_files", "$uri/index.html", "$uri", "=404"),
                    comment("return a listing of files (optional)"),
                    ("autoindex", True),
                    "",
                    allowCORS,
                ]),
            ]),
        ]),
    ])

    print(config+"\n")
    with open(realtempdir + "/nginx.conf", mode="wt") as f:
        f.write(config+"\n")
        f.flush()
        subprocess.run(["nginx", "-e", "stderr", "-t", "-c", f.name])

