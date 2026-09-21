from .nginx import *

import re
from tempfile import TemporaryDirectory
import subprocess

import os
from os import path
import argparse
import requests
import json
import sys
import shutil
import json
import tomllib as toml
import typing
import http
import urllib

parser = argparse.ArgumentParser(
    prog='simple_nginx',
    # https://codeberg.org/cgranade/icdl/src/branch/main/README.md
    description='Simple CLI/JSON/ICDL~TOML config/runner for (daemonless) nginx',
)

class syntax:
    @staticmethod
    def flag(s: str):
        value = None
        mod = ""
        kv = s.split("=", 1)
        if len(kv) > 1:
            value = kv[1]
            s = kv[0]
        while s[-1] in "+-":
            mod = s[-1] + mod
            s = s[:-1]
        parts = s.split(".")
        return parts, mod, value

# 'tls': typing.Union[bool, "auto", "none", typing.IO, typing.List[typing.Union[bool, "auto", "none", typing.IO]]],

will_configuration = lambda: CfgRecord(children={
    'config': CfgList(children=CfgFile()),
    'workers': CfgInt(default=1, min=0, max=1024),
    'log': CfgRecord(children={
        'error': CfgFile(default=sys.stderr),
        'access': CfgFile(default=None),
    }),

    'mime': CfgDict(key=CfgStr, val=CfgStr, default={
        'default': 'application/octet-stream',
    }),

    'http': CfgRecord(children={}),

    'server': CfgKeyed(key=('listen.*.port', 'listen.*.bind', 'host.*'), alias='listen', children={
        'listen': CfgList(children=CfgRecord(_parse=parse.listen, default={'port': 8998}, children={
            'port': CfgInt(default=None),
            'bind': CfgStr(),
            # default_server
            # ssl
            # http2? quic?
            # options
            # accept/deny
        })),
        'port': CfgAlias(alias='listen.port'),
        'bind': CfgAlias(alias='listen.bind'),

        'host': CfgList(children=CfgStr()),

        'loopback': CfgBool(default=False),
        'cors': CfgBool(default=True),
        'nocache': CfgBool(default=True),

        'location': CfgKeyed(key=('path.*',), alias='path', children={
            'path': CfgSet(children=CfgStr()),
            'proxy': CfgRecord(alias='upstream', children={
                'upstream': str,
                'header': CfgHeaders(),
            }),
            'header': CfgHeaders(),

            'files': CfgList(children=CfgPath()),

            'fixed': CfgText(),
            'redirect': CfgStr(),
            'status': CfgInt(enum=http.HTTPStatus),
        }),
    }),
})


class parse:
    @staticmethod
    def listen(s: str, *_):
        try:
            return {'port': int(s), 'bind': ''}
        except ValueError:
            try:
                h, p = s.split(':', 1)
                return {'port': int(p), 'bind': h}
            except ValueError:
                return {'bind': s}

class Cfg[T]:
    def __init__(desc, **kwargs):
        desc.__dict__.update(kwargs)
    def _default(desc, value: Optional[T] = None):
        try:
            if value is None: return desc.default
        except AttributeError: pass
        return value
    def _parse(desc, value: str):
        return value
    def _flag(desc, subname: list[str], mod: str, value: Optional[str] = None):
        pass
    def _merge(desc, old: T, new: T):
        return new

class CfgRecord(Cfg[dict]):
    children: dict
    def _default(desc, value: Optional[dict] = None):
        if value is None: value = {}
        return {
            # Keep value in order, where possible
            k: desc.children[k]._default(value[k]) if k in desc.children else value[k]
            for k in value.keys()
        } | {
            # Add missing keys
            k: desc.children[k]._default(None)
            for k in desc.children.keys() - value.keys()
            if not isinstance(desc.children[k], CfgAlias)
        }

class CfgStr(Cfg):
    pass

class CfgText(CfgStr):
    pass

class CfgInt(CfgStr):
    pass

class CfgEnum(CfgStr):
    pass

class CfgBool(CfgEnum):
    pass

class CfgPath(CfgStr):
    pass

class CfgFile(CfgPath):
    pass

class CfgDict(Cfg[dict]):
    pass

class CfgList(Cfg[list]):
    def _default(desc, value = None):
        if value is None: return []
        return value

class CfgSet(CfgList):
    pass

class CfgKeyed(CfgList):
    def _merge(desc, old, new):
        pass

class CfgAlias(Cfg):
    pass

class CfgHeaders(CfgList):
    pass

configuration = will_configuration()

def generate(root):
    return assemble(convert(root))

def convert(root):
    run = False
    temp = (lambda base: lambda sub: base+sub)(realtempdir+"/" if run else f"/tmp/nginx_")

    return [
        "",
        ("worker_processes", root['workers'] or "auto"),
        "",
        ("error_log", os.devnull, "emerg"),
        ("error_log", "stderr", comment("comment out this line if you do not want error logging")),

    ]

def convert_server(root, server):
    _
