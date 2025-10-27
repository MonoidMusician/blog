from util import *
from arglib import *
from runtime import *

# Layers/steps:
# Optional wrapper: tmux/screen
#   Call `python3 __main__.__file__ --$mode $argv` to re-enter
# Set up local resources (listening socket for sidechannel tunneled over ssh)
# Run one or both sides (asyncio)
#   Local TX, Remote RX:
#     Start remote RX
#     Wait for remote resources
#     Start local TX
#   Remote TX, Local RX:
#     Start local RX
#     Start remote TX
# Monitor and wait for things to close (uhh ... how does that work in tmux?)
# Cleanup steps (grab tmux buffers)

class Steps:
    def run(self):
        return []
    pass

class LRXRRXSteps:
    def run(self):
        return [self.run_rx]

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
        lambda: opt('-p', args.side.ssh_port),
        lambda: when(args.side.aux_local is not None, '-R', ':'.join(to_args([
            args.side.aux_port or (args.side.tunnel + 1),
            '127.0.0.1',
            args.side.aux_local,
        ]))),
        lambda: args.side.ssh_opt,
        lambda: ''.join(to_args([
            args.username+'@' if args.username else '',
            args.hostname
        ])), # user@hostname,
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
