---
title: Computering
subtitle: Consoles and terminals oh my
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Consoles are so funny because Iʼve always thought they were pretty bad interfaces.
(I grew up with web browsers and GUIs, although I am no stranger to terminal emulators.)

But Iʼve been realizing lately how ubiquitous they are, and they probably arenʼt going away!

For example, low level UEFI and GRUB bootloader stuff is based on console interfaces, or something.
(I donʼt actually know.)

## Aspects of using consoles

For those who **donʼt** already live and breathe it.

Iʼd like to walk through the areas with some small examples.
(Examples that are recent for me.)

### Ambient background knowledge

Stuff that you need to know.
Like how to `cd`{.bash} around directories.
When to use `sudo`{.bash} (and when not to!).

Of course, itʼs only ambient if you already know it …

### Realm of possibilities, space of actions you can take

Yeah.

symlinks vs bind mounts.
Probably doesnʼt matter for a lot of use cases.
Probably want symlinks.
Unless youʼre doing sysadmin stuff, like I was.

### How to verify

Important!

### How to make permanent

See, this is the funny part.
A lot of the joy of using a console is that you can just do thing.

But making is repeatable is hard.
Needlessly hard.
“Oh, I just execute the same commands again.”
It is rarely that simple …

If you want the `mount`{.bash} command to re-apply every time the system boots, you need to add it to `/etc/fstab`{.file}, which has different syntax!

## Why consoles are nice, sometimes

Consoles are perfect when you just have a keyboard.

Especially for the low-level bootloader stuff, this is, yeah.

Can get away with simple graphics.
Can even get away with simple interactions.

Space separated words just “makes sense”.
Thereʼs no reason to actually parse it.

Like, thereʼs a place for it, and there probably will always be a place for it.

But it is a different thing, with specific use cases.
