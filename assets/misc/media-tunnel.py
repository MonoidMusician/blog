#!/usr/bin/env python3
"""
media-tunnel.py: transports audio from one ffmpeg to another (port 2958 by default)
  *doesn't really support video yet, but it could!

Select the mode first:
    ( [[--ssh] | --ssh-tx | --ssh-rx] [user@]hostname[:ssh_port] | --as-tx | --as-rx | --local )
Flags/options for media-tunnel.py (anywhere; --help to list):
    --tx-(opt) --rx-(opt) --(opt)
Add options for ffmpeg/ffplay (rx defaults to ffplay):
    [ffmpeg_tx...] [-- ffplay_rx... [-- ffmpeg_shared...]]
"""

usage = __doc__

version = "0.1.0"

examples = ["""
media-tunnel.py --ssh myothercomputer -f avfoundation -i :default -- -nodisp

    Tunnel the default audio input (microphone) from macOS to any Unix computer with ffplay (no visualization) with LATM (AAC codec) over UDP.
    Equivalent to running these commands together (RX+TX)
        ssh -x -4 myothercomputer exec ffplay -nodisp -loglevel warning -fflags nobuffer -flags low_delay -avioflags direct -f loas 'udp://0.0.0.0:2958?listen'
        ffmpeg -loglevel warning -fflags flush_packets -flush_packets 1 -flags low_delay -avioflags direct -f avfoundation -i :default -ar 48000 -f latm udp://myothercomputer:2958
    List avfoundation input devices (microphones) with
        ffmpeg -f avfoundation -list_devices true -i - 2>| grep AVFoundation
    Replace `-nodisp` with `--env DISPLAY=:0` to use the default X11 display
""", """
media-tunnel.py --via=tcp --tx-sleep=1 --ssh linuxcomputer -re -i MyFile.mp3 -- --no-ffplay -f pulse default

    Stream a MP3 file in realtime ('-re') onto linuxcomputer over TCP, playing through PulseAudio default speakers.
    Equivalent to running these commands together (RX+TX)
        ssh -x -4 linuxcomputer exec ffmpeg -f pulse default -loglevel warning -fflags nobuffer -flags low_delay -avioflags direct -f loas -i 'tcp://0.0.0.0:2958?listen'
        ffmpeg -loglevel warning -fflags flush_packets -flush_packets 1 -flags low_delay -avioflags direct -re -i MyFile.mp3 -ar 48000 -f latm tcp://linuxcomputer:2958
    List pulse output devices (speakers) with
        pactl list short sinks
"""]

import os
_start_ppid = os.getppid()

import sys
import asyncio
import signal
import contextlib
import inspect
import re
import json
import shlex

# FIXME!! TCP does not really work, since it does not wait for the socket to open
# FIXME: lost UDP packets
# TODO: tmux/screen support (needs to call itself, to avoid piping stdin through)
# TODO: DTLS and TLS
# TODO: SDP, via python-to-python UDP socket?

################################################################################
## Utilities                                                                  ##
################################################################################

class Blank: # blank class
    def __repr__(self): return repr(self.__dict__)

class Help:
    def __init__(self, aspect, helptext, example=None):
        self.aspect = aspect
        self.helptext = helptext
        self.example = example

## Pretty output

def dedent(s):
    indent = min(len(l) - len(l.lstrip()) for l in s.split('\n') if l.strip())
    return '\n'.join(l[indent:] for l in s.split('\n'))
def columns(rows):
    # Save iterators
    rows = list(tuple(row) if type(row) != str else row for row in rows)
    # Pad out
    ncols = max(len(row) for row in rows if type(row) != str)
    rows = list(row + ('',)*(ncols - len(row)) if type(row) != str else row for row in rows)
    # Longest of each column
    sizes = list(max(len(c) for c in col) for col in zip(*(row for row in rows if type(row) != str)))
    return '\n'.join(
        ' '.join(
            str(cell).ljust(sz)
            for cell, sz in zip(row, sizes)
        ).rstrip() if type(row) != str else row
        for row in rows
    )

## Argument generation

def opt(name, value):
    """Optional commandline argument"""
    if value is None: return []
    return [name, str(value)]
def df(value, default):
    """Default value"""
    if value is None: return default
    return value
def when(include, *opts):
    """Conditional array"""
    if not include: return []
    return opts

def to_args(stuff, *args, **kwarg) -> list[str]:
    """Convert lists, functions, scalars to an argument list"""
    while callable(stuff):
        stuff = stuff(*args, **kwarg)
    if stuff is None: return []
    if type(stuff) in [str, int, float]:
        return [str(stuff)]
    return [arg for thing in stuff for arg in to_args(thing, *args, **kwarg)]

################################################################################
## Details of commands to run (ffmpeg, ssh, python3)                          ##
################################################################################

# Command for initiating this script over SSH
ssh_exec_cmd = [
    'ssh',
        '-x', # no X server forwarding
        '-4', # IPv4 speeds up connection times for me
        lambda: args.destination, # user@hostname, FIXME port
        'exec', # leave the shell immediately
]
ssh_python_cmd = ssh_exec_cmd + [
    'python3',
        '-u', # do not buffer output
        '-', # pipe this script itself over stdin
]

# Some recommended(ish) flags for ffmpeg
recommended_flags_list = [
    ['txrx', '-loglevel', 'warning'],
    ['txrx', '-hide_banner'], # redundant for log levels below info, but good to know about

    # Attempt to get lowest latency?
    [  'rx', '-fflags', 'nobuffer'],
    ['tx'  , '-fflags', 'flush_packets'],
    ['tx'  , '-flush_packets', '1'], # redundant?
    ['txrx', '-flags', 'low_delay'], # maybe only affects video?
    # ['txrx', '-avioflags', 'direct'], # bad for mpegts?
    [  'rx', '-probesize', 4000], # bytes, needed for loas
    [  'rx', '-analyzeduration', 1000], # microseconds! defaults to 5 seconds
]
recommended_flags = Blank()
recommended_flags.tx = [flag[1:] for flag in recommended_flags_list if 'tx' in flag[0]]
recommended_flags.rx = [flag[1:] for flag in recommended_flags_list if 'rx' in flag[0]]

lowprobe = [ [ '-probesize', 32 ], [ '-analyzeduration', 1000 ] ]

def url_tx():
    """Default URL scheme for transmitting"""
    return ''.join(to_args([
        args.protocol or 'udp',
        '://',
        args.hostname,
        ':', args.tx.tunnel,
    ]))
def url_rx():
    """Default URL scheme for receiving"""
    return ''.join(to_args([
        args.protocol or 'udp',
        '://',
        df(args.rx.bind, '0.0.0.0'),
        ':', args.rx.tunnel,
        '?listen',
        when((args.protocol or 'udp') in ['udp', 'rtp'], opt('&sources=', args.rx.accept)),
    ]))
def audio_options():
    return [
        opt('-ar', args.tx.sample_rate),
    ]
def audio_format():
    return [
        '-ar', args.tx.sample_rate or 48000,
        # '-channels', audio_layouts[args.tx.channels or 'stereo'],
        # '-channel_layout', args.tx.channels or 'stereo',
    ]

helptexts = {
    'Mode:': ('--ssh', '--ssh-tx', '--ssh-rx', '--as-tx', '--as-rx', '--local'),
    # Also see methods of `class Args` for documentation
}

# Strategies for transporting media
#
# Not all strategies may be available on all ffmpeg builds!
#
# Note: sometimes options like sample-rate ('-ar') need to be specified on
# both sides of the stream, for raw formats; other times, it is an error to
# specify it on the receiving side.
strategies = {
    # LOAS/LATM is Low Overhead Audio Stream / Low Overhead Audio Transport Multiplex
    # Basically it wraps the AAC with a header, including metadata
    'loas': {
        'tx': [ audio_options, ['-f', 'latm'], url_tx ],
        'rx': [ ['-f', 'loas'], url_rx ],
        'help': "Low Overhead Audio (AAC) Stream",
    },
    # ADTS also includes a header with metadata, but is even more simplistic than LOAS/LATM
    'adts': {
        'tx': [ audio_options, ['-f', 'adts'], url_tx ],
        'rx': [ ['-f', 'aac'], url_rx ],
    },
    'f32le': {
        # FIXME: needs work
        'tx': [ ['-f', 'f32le'], '-channel_layout', 'mono', audio_format, url_tx ],
        'rx': [ ['-f', 'f32le'], lowprobe, audio_format, url_rx ],
        'help': "[FIXME] Raw samples (32-bit floating point, little endian, interleaved)",
    },
    'mpegts': {
        'tx': [ audio_options, ['-f', 'mpegts'], url_tx ],
        'rx': [ ['-f', 'mpegts'], lowprobe, url_rx ],
        'help': "[FIXME] Transport Stream (.ts) container/file format",
    },
}
for name, strat in strategies.items():
    try:
        if '--'+name not in helptexts:
            helptexts['--'+name] = strat['help']
    except: pass
for alias, of in {'latm':'loas', 'aac':'adts', 'raw':'f32le', 'ts':'mpegts'}.items():
    strategies[alias] = strategies[of]
    helptexts['--'+alias] = "Alias for --"+of

################################################################################
## Argument parsing                                                           ##
################################################################################

global_options = {
    'usage': False,
    'help': False,
    'examples': False,
    'version': False,
    # 'argparse': False, # handled in `class Args`

    # https://ffmpeg.org/ffmpeg-formats.html
    'format': Help(list(strategies.keys()), "The data format (muxer/demuxer) to represent the audio"),
    # https://ffmpeg.org/ffmpeg-protocols.html
    'protocol': Help(['udp', 'tcp'], "Force a transport protocol"),
}

audio_layouts = {
    'stereo': 2,
    'mono': 1,
}
txrx_options = {
    'dry-run': Help(False, "Print commands but do not run them"),
    'echo': Help(False, "Echo commands as they are run"),
    'recommended': Help(True, "Recommended ffmpeg flags (for latency, etc.)"),
    'outlive': Help(False, "Let it outlive its parent process (e.g. ssh disconnects) [Not Recommended]"),
    'ffplay': Help(True, "Receive with ffplay, else use ffmpeg"),

    'tunnel': Help(2958, "Port for media RX/TX"), # --tunnel=2958
    'bind': Help(str, "IP address to bind receiver, set by default from $SSH_CONNECTION, use 0.0.0.0 for all interfaces"),
    'accept': Help(str, "Accept connections from these IPs (UDP only), set by default from $SSH_CONNECTION"),

    'sleep': Help(float, "Wait seconds before starting, e.g. --tx-sleep=0.5"), # --tx-sleep=0.5

    'exec': Help(str, "Path to ffmpeg (or bin directory)"), # --tx-exec="$(which ffmpeg)"
    'sample-rate': Help(48000, "(Higher may reduce latency, typically up to 96000Hz)"), # --sample-rate=96000
    'channels': list(audio_layouts.keys()), # --stereo

    'env': Help(list, "Set environment variables, e.g. --rx-env DISPLAY=:0"),

    # TODO: --recommended, but as individual flags
    # TODO: --list
}
for optionset in [txrx_options, global_options]:
    for name, aspect in optionset.items():
        if type(aspect) == Help:
            optionset[name] = aspect.aspect
            helptexts['--'+name.replace('_','-')] = aspect.helptext

class OptParser:
    """Custom argument parsing library"""
    # (should really be a module, shhh)

    @staticmethod
    def init(options) -> Blank:
        """Initialize the data for `options`"""
        data = Blank()
        for flagname, aspect in options.items():
            attrname = flagname.replace('-', '_')
            init = aspect
            if aspect == list: init = []
            elif type(aspect) in [list, set, tuple]: init = None
            elif callable(aspect): init = None
            setattr(data, attrname, init)
        return data

    @staticmethod
    def prefilter(dataoptionsprefixes, arg):
        """Preprocess prefixed arguments to apply to certain datas."""
        matched_prefix = ''
        optionsandtheirdatas = [] # (options, [*datas])
        tongue = arg[2:] if arg.startswith('--') else ''
        for (data, options, prefixes) in dataoptionsprefixes:
            for prefix in set(prefixes):
                if not tongue.startswith(prefix): continue
                if len(matched_prefix) > len(prefix): continue
                if len(matched_prefix) < len(prefix):
                    matched_prefix = prefix
                    optionsandtheirdatas = []
                for optiondatas in optionsandtheirdatas:
                    if optiondatas[0] == options:
                        optiondatas[1].append(data); break
                else: optionsandtheirdatas.append((options, [data]))
        if not matched_prefix:
            return optionsandtheirdatas, arg, matched_prefix
        return optionsandtheirdatas, '--'+tongue[len(matched_prefix):], matched_prefix

    @staticmethod
    def recognize(optionsandtheirdatas, arg, argv):
        """
        [Only mutates argv if necessary]
        Parse a single option (pulling arguments off of argv as needed)
        Returns a tuple(len, attrname, modify, value, datas) or None, where
        len is the total number of arguments from [arg, *argv] that were consumed

        Only parses double-dash style arguments

        - True: boolean flag with default value true
        - False: boolean flag with default value false
        (can only be given an argument with --[no-]flag[=true|false|0|1])
        - str/int/float: default value None, parse as that datatype
        - str(...)/int(...)/float(...): given default value, parse as the same datatype
        - list: maintain a list of all --arg x and --args="y z" (['x', 'y', 'z'])
        - list(...): an enum, whose values are also flags of their own
        - tuple(...): an enum, whose values are not flags of their own
        """
        if not arg.startswith('--'): return []
        _len0 = len(argv)
        # Isolate the argument name
        argname = arg.split('=', 1)[0]
        # Immediate value if given
        value = arg.split('=', 1)[1] if '=' in arg else None
        # Default action: overwrite the value
        modify = lambda _: value
        # Try to find it
        for options, datas in optionsandtheirdatas:
            broke = False
            for flagname, aspect in options.items():
                attrname = flagname.replace('-', '_')
                flagname = '--'+flagname
                if type(aspect) in [str, int, float]:
                    aspect = type(aspect)
                if type(aspect) == bool:
                    if flagname == argname:
                        if value is None: value = True
                        else: value = value.lower() not in ['false', '0']
                        broke=True;break
                    elif argname == '--no-'+flagname[2:]:
                        if value is None: value = False
                        else: value = value.lower() in ['false', '0']
                        broke=True;break
                elif aspect == list and argname in [flagname, flagname+'s']:
                    if value is None: value = argv.pop(0)
                    if argname == flagname+'s': value = value.split(' ')
                    else: value = [value]
                    modify = lambda existing: existing + value
                    broke=True;break
                elif type(aspect) == list and argname[2:] in aspect and argname[:2] == '--' and value is None:
                    value = argname[2:]
                    broke=True;break
                elif flagname == argname:
                    if type(aspect) is inspect.Signature:
                        # Use the function signature to grab some args
                        argargs = [] if value is None else [value]
                        params = list(aspect.parameters.values())
                        if params[0].name == 'self': params.pop(0)
                        if argargs: params.pop(0)
                        for param in params:
                            if param.default != inspect.Parameter.empty:
                                if not argv or argv[0].startswith('--'): break
                            argargs.append(argv.pop(0))
                        modify = lambda fn: [fn(*argargs), fn][1]
                        broke=True;break
                    if value is None: value = argv.pop(0)
                    if callable(aspect):
                        value = aspect(value)
                    elif value not in aspect:
                        raise ValueError(repr(value) + ' not in ' + repr(aspect) + ' for flag ' + flagname)
                    broke=True;break
            if broke: break
        else:
            return None
        return (1 + len(argv) - _len0, attrname, modify, value, datas)

    @staticmethod
    def applyresult(stuffs):
        """[Mutates datas inside stuffs]"""
        if not stuffs: return None
        (_len, attrname, modify, value, datas) = stuffs
        for data in datas:
            setattr(data, attrname, modify(getattr(data, attrname)))
        return stuffs

    @staticmethod
    def apply(dataoptionsprefixes, arg, argv):
        """[Mutates datas and argv] Parse and apply an option."""
        optionsandtheirdatas, arg, prefix = OptParser.prefilter(dataoptionsprefixes, arg)
        parsed = OptParser.recognize(optionsandtheirdatas, arg, argv)
        if prefix and not parsed:
            raise ValueError(repr('--'+prefix+arg[2:]) + ' had special prefix ' + repr('--'+prefix) + ' but was not recognized')
        return OptParser.applyresult(parsed)

    @staticmethod
    def parseall(dataoptionsprefixes_or_keyoptionsprefixes, argv, fallback=None):
        if isinstance(dataoptionsprefixes_or_keyoptionsprefixes, dict):
            keyoptionsprefixes = dataoptionsprefixes_or_keyoptionsprefixes
            slots, dataoptionsprefixes = zip(*[
                ((k, data), (data, options, prefixes))
                for k, (options, prefixes) in keyoptionsprefixes.items()
                for data in [OptParser.init()]
            ])
            slots = dict(slots)
        else:
            dataoptionsprefixes = dataoptionsprefixes_or_keyoptionsprefixes
            slots = [data for (data, _, __) in dataoptionsprefixes]
        while argv:
            arg = argv.pop(0)
            if not OptParser.apply(dataoptionsprefixes, arg, argv):
                fallback(slots, arg, argv)
        return slots

class Args:
    """Arguments for the program live here"""
    # Methods without leading underscores act as argument parsers and are copied
    # to `global_options` and `helptexts`

    def __repr__(self):
        return repr({
            k:v for k,v in self.__dict__.items()
            if not k.startswith('_') and not callable(v)
        })
    def __init__(self, argv):
        for k,v in OptParser.init(global_options).__dict__.items():
            if type(v) == inspect.Signature: continue
            setattr(self, k, v)

        self.argv = argv

        self._mode_args = [] # keep track of these so we can discard them

        self.destination = None # [user@]hostname[:ssh_port]

        # Dedicate options for TX/RX
        self.tx = OptParser.init(txrx_options)
        self.rx = OptParser.init(txrx_options)

        # User options to ffmpeg/ffplay
        self.tx_ffmpeg = []
        self.rx_ffmpeg = []
        self.layer = 0 # tx, rx, txrx

        prefixes_for_both = [pq+'-' for (p,q) in [('rx', 'tx'), ('tx', 'rx')] for pq in [p+q, p+'-'+q]]
        OptParser.parseall([
            (self, global_options, ['']),
            (self.tx, txrx_options, ['', 'tx-', *prefixes_for_both]),
            (self.rx, txrx_options, ['', 'rx-', *prefixes_for_both]),
        ], argv, fallback=lambda _, arg, __: self(arg))
    def __call__(self, arg):
        layers = ['--tx:', '--rx:']
        if self.mode is None and not arg.startswith('-'):
            self.mode = 'ssh'
            self.destination = arg
            self._mode_args.append(arg)
        elif arg == '--':
            self.layer += 1
        elif arg in layers:
            self.layer = layers.index(arg)
        else:
            for tgt in [[self.tx_ffmpeg], [self.rx_ffmpeg], [self.tx_ffmpeg, self.rx_ffmpeg]][self.layer]:
                tgt.append(arg)
    mode = None
    def ssh(self, destination):
        """Run TX locally, start RX over SSH (non-interactive auth)"""
        self.mode = 'ssh'
        self.destination = destination
        self._mode_args.append('--ssh')
        self._mode_args.append(destination)
    def ssh_tx(self, destination):
        """Start TX over SSH (non-interactive auth)"""
        self.mode = 'ssh_tx'
        self.destination = destination
        self._mode_args.append('--ssh-tx')
        self._mode_args.append(destination)
    def ssh_rx(self, destination):
        """Start RX over SSH (non-interactive auth)"""
        self.mode = 'ssh_rx'
        self.destination = destination
        self._mode_args.append('--ssh-rx')
        self._mode_args.append(destination)
    def as_tx(self):
        """Run TX locally"""
        self.mode = 'as_tx'
        self._mode_args.append('--as-tx')
    def as_rx(self):
        """Run RX locally"""
        self.mode = 'as_rx'
        self._mode_args.append('--as-rx')
    def local(self):
        """Run TX and RX locally"""
        self.mode = 'local'
        self._mode_args.append('--local')

    def via(self, method):
        """Set format/protocol/transport, e.g. --via=loas/udp"""
        for item in method.split('/'):
            handled = False
            if item in global_options['format']:
                handled = True; self.format = item
            if item in global_options['protocol']:
                handled = True; self.protocol = item
            if not handled:
                raise ValueError(repr(item) + ' not in ' + repr(global_options['format']) + ' or ' + repr(global_options['protocol']))
    wants_argparse = None
    def argparse(self, format='json'):
        self.wants_argparse = format

    def _revise_mode(self, *mode):
        args = []
        remove = list(self._mode_args)
        # FIXME: don't use sys.argv
        for arg in sys.argv[1:]:
            if remove and arg == remove[0]:
                remove.pop(0)
            else:
                args.append(arg)
        return list(mode)+args

    @property
    def _infomode(self):
        return any([
            self.usage,
            self.help,
            self.examples,
            self.version,
            self.wants_argparse,
        ])
    _destination_regex = re.compile(r'(?:([-_.\w]+)@)?([-_.\w]+)(?:[:](\d+))?')
    @property
    def username(self):
        try: return self._destination_regex.fullmatch(self.destination)[1]
        except: return None
    @property
    def hostname(self):
        try: return self._destination_regex.fullmatch(self.destination)[2]
        except: return None
    @property
    def ssh_port(self):
        try: return self._destination_regex.fullmatch(self.destination)[3]
        except: return None
# Copy over to `global_options` and `helptexts`
for attr, value in Args.__dict__.items():
    if attr.startswith('_'): continue
    try:
        global_options[attr.replace('_', '-')] = inspect.signature(value)
        flagname = '--'+attr.replace('_', '-')
        helptexts[flagname] = helptexts.get(flagname) or inspect.getdoc(value)
    except: pass
# print(Args.__dict__)

args = Args(['--help'])
def parse_sys_argv():
    global args
    args = Args(sys.argv[1:])

    try:
        (client_ip, _out_port, server_ip, _in_port) = os.environ['SSH_CONNECTION'].split(' ')
        if args.rx.bind is None:
            args.rx.bind = server_ip
        if args.rx.accept is None:
            args.rx.accept = client_ip
    except KeyError: pass

    # Now we assemble the arguments into commands to run
    for name in ['tx', 'rx']:
        other = {'tx':'rx','rx':'tx'}[name]
        side = getattr(args, name)
        side._ffmpeg_strat = to_args(strategies[args.format or list(strategies.keys())[0]][name])
        if name == 'rx' and not side.ffplay:
            side._ffmpeg_strat = side._ffmpeg_strat[:-1] + ['-i', side._ffmpeg_strat[-1]]
        side._ffmpeg_flags = to_args(getattr(recommended_flags, name) if side.recommended else [])
        if name == 'tx':
            side._ffmpeg_args = side._ffmpeg_flags + getattr(args, name+'_ffmpeg') + side._ffmpeg_strat
        else:
            side._ffmpeg_args = getattr(args, name+'_ffmpeg') + side._ffmpeg_flags + side._ffmpeg_strat
        # Binary name to search for and call
        side._ffmpeg_bin = 'ffplay' if side.ffplay and name == 'rx' else 'ffmpeg'
        side._ffmpeg_path = side.exec or side._ffmpeg_bin
        side._ffmpeg_cmd = [side._ffmpeg_path] + side._ffmpeg_args
        for env_arg in side.env:
            assert '=' in env_arg
        side._env_cmd = (['env'] + side.env if side.env else []) + side._ffmpeg_cmd
        side._cmd = side._env_cmd
        side._stdin = None
        side._active = (other not in args.mode)
        if (args.mode == 'ssh' and name == 'rx') or args.mode == 'ssh_'+name:
            if not side.dry_run:
                side._cmd = to_args(ssh_python_cmd) + [
                    shlex.quote(arg) for arg in args._revise_mode('--as-'+name)
                ]
                side._stdin = open(__file__, mode='rb')
            else:
                side._cmd = to_args(ssh_exec_cmd) + side._cmd
            side._active = 'ssh'

        # for k,v in side.__dict__.items():
        #     if k.startswith('_'):
        #         print(name, k, v)
        # print()

# Arg parsing can fail if we are asking for help
if '--help' in sys.argv or '--usage' in sys.argv:
    try: parse_sys_argv()
    except Exception: args = Args(['--help'])
else: parse_sys_argv()

# Handle informative flags
if args.wants_argparse: # argparse beats every other flag
    print(json.dumps(args, default=lambda x: x.__dict__, indent=2))
    sys.exit(0)
if args.help:
    print(dedent(usage).strip())
    print()
    rows = []
    grouped = {name:[] for name, subs in helptexts.items() if type(subs) == tuple}
    group_by = {by:name for name, subs in helptexts.items() if type(subs) == tuple for by in subs}
    for name, aspect in [*global_options.items(), (None, None), *txrx_options.items()]:
        if name is None:
            rows.append('')
            rows.append('TX/RX Options:')
            continue
        flagname = name.replace('_', '-')
        helptext = helptexts.get('--'+flagname, "")
        if helptext is None: continue
        if type(helptext) == list:
            rows.extend(helptext)
        elif aspect is True:
            grouped.get(group_by.get('--'+flagname), rows).append([
                '--no-'+flagname,
                '--'+flagname,
                "true",
                helptext,
            ])
        elif aspect is False:
            grouped.get(group_by.get('--'+flagname), rows).append([
                '--'+flagname,
                '--no-'+flagname,
                "true",
                helptext,
            ])
        elif aspect == list:
            grouped.get(group_by.get('--'+flagname), rows).append([
                '--'+flagname,
                '--'+flagname+'s',
                "[]",
                helptext,
            ])
        else:
            desc = repr(aspect) if type(aspect) is not type else f'<{aspect.__name__}>'
            if type(aspect) == inspect.Signature:
                desc = re.fullmatch(r'<Signature \(self(?:, (.+))?\)>', desc)[1] or ''
                desc = ', '.join('<'+p+'>' for p in desc.split(', ')) if desc else ''
            if len(desc) > 25: desc = '...'
            grouped.get(group_by.get('--'+flagname), rows).append([
                *(
                    [ '--'+flagname, '' ]
                    if '--'+flagname not in group_by else
                    [ '', '--'+flagname ]
                ),
                desc,
                helptext or "",
            ])
            if type(aspect) == list:
                for flagname in aspect:
                    helptext = helptexts.get('--'+flagname, "")
                    if helptext is None: continue
                    grouped.get(group_by.get('--'+flagname), rows).append([
                        '',
                        '--'+flagname,
                        '',
                        helptext or "",
                    ])
    rows = [item for name, subrows in grouped.items() for item in [name, *subrows]] + [''] + rows
    print(columns(rows))
    sys.exit(0)
if args.usage:
    print(dedent(usage).strip())
    sys.exit(0)
if args.version:
    print(version)
    sys.exit(0)
if args._infomode:
    print('Oops')
    sys.exit(1)

################################################################################
## Asyncio runtime implementation                                             ##
################################################################################

# If we have ssh and ffmpeg running and one terminates, use this event to
# terminate the other process (using `until_event` in `monitor_subprocess`)
please_exit_everything = asyncio.Event()

@contextlib.asynccontextmanager
async def until_event(event, loop=None):
    """A context manager to event when an event fires"""
    current_task = asyncio.current_task(loop)

    async def cancel_when_completes():
        await event.wait()
        current_task.cancel()
    cancel_task = asyncio.create_task(cancel_when_completes())
    try:  yield cancel_task
    finally:    cancel_task.cancel()

async def monitor_subprocess(processpromise, *actions, cleanup=None, name=None, exit_everything=True):
    """This monitors a subprocess to see if it goes away, and to make sure it goes away"""
    process = None
    try:
        async with until_event(please_exit_everything):
            process = await processpromise
            if process is None: return # for dry-run
            for action in actions:
                await action(process)
            await process.wait()
            #print("process exited already", name)
            if exit_everything:
                please_exit_everything.set()
    except (KeyboardInterrupt, asyncio.exceptions.CancelledError):
        if process is None: return
        if cleanup is not None:
            await cleanup(process)
        try:
            process.terminate()
            async with asyncio.timeout(1):
                await process.wait()
                #print("exit after terminate()", name)
        except asyncio.TimeoutError:
            process.kill()
            #print("1s timeout failed", name)
            await process.wait()
            #print("exit after kill()", name)
        except ProcessLookupError:
            pass # must already be gone?

async def dry_or_run(dry, cmd, *argv, sleep=None, **kwargs):
    if dry:
        print(shlex.join([cmd, *argv]))
    else:
        if sleep is not None:
            await asyncio.sleep(sleep)
        return await asyncio.create_subprocess_exec(cmd, *argv, **kwargs)

async def command(args, dry_run=None, stdin=None, process_group=True, name=None, exit_everything=True, sleep=None):
    if process_group is True:
        process_group = os.getpgid(0)
    await monitor_subprocess(dry_or_run(
        dry_run,
        *args,
        sleep=sleep,
        stdin=stdin,
        process_group=process_group
    ), name=name, exit_everything=exit_everything)

async def main():
    """Start the RX/TX commands that were assembled by the argument parser"""
    try:
        commands = []
        # Start RX before TX (ish)
        for name, side in {'rx':args.rx, 'tx':args.tx}.items():
            if not side._active: continue
            if side.echo: print((name if side._active == True else side._active)+':', shlex.join(side._cmd))
            commands.append(command(
                side._cmd,
                dry_run=side.dry_run,
                sleep=side.sleep if side._active != 'ssh' else None,
                stdin=side._stdin,
                name=name
            ))
        await asyncio.gather(*commands)
    except (KeyboardInterrupt, asyncio.exceptions.CancelledError):
        print("\nAudio tunnel closed (hopefully)")

if __name__ == '__main__':
    # https://stackoverflow.com/questions/48562893/how-to-gracefully-terminate-an-asyncio-script-with-ctrl-c
    # https://stackoverflow.com/questions/78452284/keyboardinterrupt-in-asyncio-taskgroup
    # TODO: cleanup
    loop = asyncio.new_event_loop()
    signaled = None

    def canceler(signame):
        """Handle signals by canceling the main task"""
        global signaled
        signaled = signame
        main_task.cancel()
        please_exit_everything.set() # just in case
    async def check_parent():
        """Poll *os.getppid()* to make sure the parent is still around"""
        if (args.tx.outlive and args.tx._active) or (args.rx.outlive and args.rx._active): return
        try:
            while os.getppid() == _start_ppid:
                await asyncio.sleep(0.04)
                if please_exit_everything.is_set(): return
            canceler(signal.SIGHUP)
        except asyncio.exceptions.CancelledError: pass
        finally: please_exit_everything.set() # just in case
    for signame in [signal.SIGINT, signal.SIGTERM, signal.SIGHUP]:
        loop.add_signal_handler(signame, canceler, signame)
    checking_parent = asyncio.ensure_future(check_parent(), loop=loop)
    try:
        main_task = asyncio.ensure_future(main(), loop=loop)
        loop.run_until_complete(main_task)
        checking_parent.cancel()
        loop.run_until_complete(checking_parent)
    finally:
        loop.close()

"""
ffmpeg -loglevel info -f audiotoolbox -list_devices true -i - 2>| grep AudioToolbox
ffmpeg -loglevel info -f avfoundation -list_devices true -i - 2>| grep AVFoundation
pactl --format=json list sinks | jq .
aplay -l
aplay -L
pactl --format=json list sources | jq .
arecord -l
arecord -L
"""
