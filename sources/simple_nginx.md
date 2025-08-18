---
title: Simple Local Nginx Config
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/08/04
---

I made this as a drop-in replacement for `python3 -m http.server 8998`{.sh} or the older Python 2 `python -m SimpleHTTPServer 8998`{.sh}, because I needed a file server that supported range requests and [CORS]{t=}.

That is, this is a simple nginx config that serves static files from the current directory `$PWD`{.sh}, with all the goodies that it supports out of the box: high performance, range requests, caching/ETags, directory indexes, error logging, and so on.
This config includes wide-open [CORS]{t=} config since I often need that in these circumstances, but you should take it and customize it to your needs.
You can easily add upstreams to be reverse proxied, or whatever other features you want.

You need to run it with `nginx -c "$(realpath nginx.conf)" -p "$PWD" -e stderr`{.sh} (of course, substituting the path to `nginx.conf` if needed, or choosing another location than `$PWD`{.sh}, and so on).

It does **not** run in daemon mode, thus it exits on <kbd>Ctrl</kbd>+<kbd>C</kbd>.
This, in addition to the locality of running from any directory, is the main difference from conventional nginx configs, which usually are meant to be run persistently by the system.

[[Raw]](/assets/files/simple_nginx.conf) / [[Download]](/assets/files/simple_nginx.conf){download=""}

```nginx {.wrap save-to=assets/files/simple_nginx.conf}
# Simple nginx configuration for serving static files from
# the specified directory onto port 8998.
#
# Run with:
#     nginx -c "$(realpath simple_nginx.conf)" -p "$PWD" -e stderr
# to serve files from the current directory ($PWD)
#
# By MonoidMusician, CC0/Public Domain

worker_processes 1;

error_log /dev/null emerg;
error_log stderr; # comment out this line if you do not want error logging

# do not detach the process from the terminal,
# which means it stops on ctrl-c
daemon off;
# thus a PID file is unnecessary
pid /dev/null;

events {
}

http {
    # turn off logging
    access_log off;

    # temporary files
    client_body_temp_path /tmp/nginx_8998_client_body/;
    fastcgi_temp_path /tmp/nginx_8998_fastcgi/;
    proxy_temp_path /tmp/nginx_8998_proxy/;
    scgi_temp_path /tmp/nginx_8998_scgi/;
    uwsgi_temp_path /tmp/nginx_8998_uwsgi/;

    sendfile            on;
    tcp_nopush          on;
    keepalive_timeout   65;
    types_hash_max_size 4096;

    # mapping of file types to MIME types
    # (use [.] globbing to allow the file to not exist)
    include             /etc/nginx/mime[.]types;
    include             /usr/local/etc/nginx/mime[.]types;
    # (default to binary data)
    default_type        application/octet-stream;

    server {
        # port number(s)
        listen       8998;
        listen       [::]:8998;

        http2        on;
        server_name  localhost;

        location / {
            # use whatever the value of -p is
            root ./;
            # serve local files or return 404
            try_files $uri/index.html $uri =404;
            # return a listing of files (optional)
            autoindex on;

            # Always allow CORS requests
            if ($request_method = 'OPTIONS') {
                add_header 'Access-Control-Allow-Origin' $http_origin always;
                add_header 'Access-Control-Allow-Private-Network' 'true';
                add_header 'Access-Control-Allow-Methods' 'GET, HEAD, OPTIONS';
                #
                # Custom headers and headers various browsers *should* be OK with but aren't
                #
                add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range';
                #
                # Tell client that this pre-flight info is valid for 20 seconds (was: days)
                #
                add_header 'Access-Control-Max-Age' 20; #1728000;
                add_header 'Content-Type' 'text/plain; charset=utf-8';
                add_header 'Content-Length' 0;
                return 204;
            }
            if ($request_method = 'HEAD') {
                add_header 'Access-Control-Allow-Origin' $http_origin always;
                add_header 'Access-Control-Allow-Private-Network' 'true' always;
                add_header 'Access-Control-Allow-Methods' 'GET, HEAD, OPTIONS' always;
                add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range' always;
                add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range' always;
            }
            if ($request_method = 'GET') {
                add_header 'Access-Control-Allow-Origin' $http_origin always;
                add_header 'Access-Control-Allow-Private-Network' 'true' always;
                add_header 'Access-Control-Allow-Methods' 'GET, HEAD, OPTIONS' always;
                add_header 'Access-Control-Allow-Headers' 'DNT,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Range' always;
                add_header 'Access-Control-Expose-Headers' 'Content-Length,Content-Range' always;
            }
        }
    }
}
```
