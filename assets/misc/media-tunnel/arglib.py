################################################################################
## Custom argument parsing library                                            ##
################################################################################

from util import *

class Help:
    def __init__(self, aspect, helptext, example=None):
        self.aspect = aspect
        self.helptext = helptext
        self.example = example

helptexts = {
    'Mode:': ('--ssh', '--ssh-tx', '--ssh-rx', '--as-tx', '--as-rx', '--local'),
    # Also see methods of `class Args` for documentation
}

global_options = {
    'usage': False,
    'help': False,
    'examples': False,
    # 'example': int, # print a particular or random example (handled in `class Args`)
    'version': False,
    # 'argparse': False, # handled in `class Args`
    'bundle': False, # output bundled script

    # https://ffmpeg.org/ffmpeg-formats.html
    'format': Help(list(), "The data format (muxer/demuxer) to represent the audio"),
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

    'bind': Help(str, "IP address to bind receiver, set by default from $SSH_CONNECTION (use 0.0.0.0 for all interfaces)"),
    'accept': Help(str, "Accept connections from these IPs (UDP only), set by default from $SSH_CONNECTION"),
    'tunnel': Help(2958, "Port for media RX/TX (not tunneled through SSH)"), # --tunnel=2958
    'ssh-port': Help(int, "Port for SSH (usually 22 by default)"), # --ssh-port=22
    'aux-port': Help(int, "Auxiliary port (tunneled through SSH, $tunnel+1 by default)"), # --aux-port=2959
    'aux-local': Help(int, "Local auxiliary port (random free port)"), # --aux-local=0

    'sleep': Help(float, "Wait seconds before starting, e.g. --tx-sleep=0.5"), # --tx-sleep=0.5

    'exec': Help(str, "Path to ffmpeg (or bin directory)"), # --tx-exec="$(which ffmpeg)"
    'sample-rate': Help(48000, "(Higher may reduce latency, typically up to 96000Hz)"), # --sample-rate=96000
    'channels': list(audio_layouts.keys()), # --stereo

    'env': Help(list, "Set environment variables, e.g. --rx-env DISPLAY=:0"),
    'ssh-opt': Help(list, "Options for ssh"),

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
                    if argname == flagname+'s': value = value.split()
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

args = None
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
        global args
        args = self
        for k,v in OptParser.init(global_options).__dict__.items():
            if type(v) == inspect.Signature: continue
            setattr(self, k, v)

        self.argv = argv

        self._mode_args = [] # keep track of these so we can discard them

        self.destination = None # [user@]hostname[:ssh_port]
        self.username = None
        self.hostname = None

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
    @property
    def destination(self):
        destination = self.hostname or '0.0.0.0'
        if self.username:
            destination = self.username + '@' + destination
        if self.ssh_port:
            destination = destination + ':' + self.ssh_port
        return destination
    _destination_regex = re.compile(r'(?:([-_.\w]+)@)?([-_.\w]+)(?:[:](\d+))?')
    @destination.setter
    def destination(self, value):
        try:
            matched = self._destination_regex.fullmatch(value)
            self.hostname = matched[2]
            if matched[2]: self.username = matched[1]
            if matched[3]: self.ssh_port = matched[3]
        except:
            pass

    specific_example = None
    def example(self, nr=0):
        self.specific_example = int(nr)
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

# Arg parsing can fail if we are asking for help
if '--help' in sys.argv or '--usage' in sys.argv:
    try: parse_sys_argv()
    except Exception: args = Args(['--help'])
else: parse_sys_argv()
