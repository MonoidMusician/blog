#!/usr/bin/env python3
"""
media-tunnel.py: python helper to transport audio from one ffmpeg to another
  e.g. from microphones on one machine to speakers on another
  (doesn't really support video yet, but it could!)

The media goes directly over port 2958 by default (TCP or UDP depending on settings),
*not* tunneled over SSH, while the next sequential port (thus 2959 by default) *is*
tunneled if needed for additional communication (encryption parameters, SDP, etc.)

Select the mode first:
    ( [[--ssh] | --ssh-tx | --ssh-rx] [user@]hostname[:ssh_port] | --as-tx | --as-rx | --local )
Flags/options for media-tunnel.py (anywhere; --help to list):
    --tx-(opt) --rx-(opt) --(opt)
Add options for ffmpeg/ffplay (rx defaults to ffplay):
    [ffmpeg_tx...] [-- ffplay_rx... [-- ffmpeg_shared...]]
"""

from util import *
from arglib import *
from plan import *
from runtime import *

usage = __doc__ # printed with --usage

version = "0.1.1"

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


def after_parse():
    try:
        (client_ip, _out_port, server_ip, _in_port) = os.environ['SSH_CONNECTION'].split()
        if args.rx.bind is None:
            args.rx.bind = server_ip
        if args.rx.accept is None:
            args.rx.accept = client_ip
    except KeyError: pass

    # Now we assemble the arguments into commands to run
    for name in ['tx', 'rx']:
        other = {'tx':'rx','rx':'tx'}[name]
        side = getattr(args, name)
        args.side = side
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
        del args.side

        # for k,v in side.__dict__.items():
        #     if k.startswith('_'):
        #         print(name, k, v)
        # print()

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
                "false",
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
if args.examples:
    for i,ex in enumerate(examples):
        print(ex.rstrip())
    sys.exit(0)
if args.specific_example is not None:
    if 0 < args.specific_example <= len(examples):
        print(examples[args.specific_example - 1].rstrip())
    else:
        print(random.choice(examples).rstrip())
    sys.exit(0)
if args.bundle:
    print(bundle())
    sys.exit(0)
if args._infomode:
    print('Oops')
    sys.exit(1)


after_parse()



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
    run_main_thingy(main, (args.tx.outlive and args.tx._active) or (args.rx.outlive and args.rx._active))
