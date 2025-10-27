################################################################################
## Utilities                                                                  ##
################################################################################

import os
start_ppid = os.getppid()

import sys
import asyncio
import signal
import contextlib
import inspect
import re
import json
import shlex
import urllib
import random
import socket

class Blank: # blank class
    def __repr__(self): return repr(self.__dict__)


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


## Reformat

# https://stackoverflow.com/questions/14693701/how-can-i-remove-the-ansi-escape-sequences-from-a-string-in-python
re_graphics = re.compile(R'''
    ( # graphics commands
        \x1B    # ESC
        \[      # CSI
        [0-?]*  # Parameter bytes
        [ -/]*  # Intermediate bytes
        m       # Final byte
    )+
    (.*?) # content
    ( # graphics reset
        \x1B
        \[
        0*
        m
    )
''', re.VERBOSE)

def no_graphics(string):
    return re_graphics.sub(lambda m: m[2], string)

def reformat(string):
    """Reformat into an equal length column where ANSI graphics commands do not
    linger over newlines"""
    assert '\r' not in string
    def restore_graphics(graphics):
        start = graphics[1]
        content = graphics[2]
        reset = graphics[3]
        return start + content.replace('\n', reset+'\n'+start) + reset
    string = re_graphics.sub(restore_graphics, string)
    lines = string.split('\n')
    linelength = max(len(no_graphics(l)) for l in lines)
    string = '\n'.join(l.ljust(linelength) for l in lines)
    return string

def side_by_side(*strings, sep='|', align='end'):
    """Print tmux scrollback buffers side by side"""
    strings = [re.sub(r'\n\s+$', '', string) for string in strings]
    linecount = max(string.count('\n') for string in strings)
    pads = ['\n' * (linecount - string.count('\n')) for string in strings]
    strings = [pad + string if align == 'end' else string + pad for pad, string in zip(pads, strings)]
    strings = [reformat(string) for string in strings]
    lines = [sep.join(line) for line in zip(*strings)]
    return '\n'.join(lines)
