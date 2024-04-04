---
title: Symlinks, hardlinks, (bind) mounts, oh my
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Iʼve always been a bit scared of symlinks and hardlinks.
Bind mounts also accomplish similar things, but through a completely different mechanism.
It gets confusing!

They are all variations on copying without copying: none of them take appreciably more disk space, and they all will propagate changes in some way, but they are implemented differently and that has various implications for how you can use them.

Two of the reasons that they are interesting:

- You can do some cool things with them: make things more convenient for you, save harddrive space, work with some tools and abstractions.
- By learning about what they do and how they work, you learn more about file systems and operating systems ^^

:::Note
These features require support from both the operating system and the filesystem.
The older filesystems of FAT32 and exFAT do not support symlinks, for example.^[Ask me [how I learned that](https://github.com/npm/cli/issues/1232).]
(Which is a shame, since they have the best cross-platform support: I can read/write them on macOS, Linuxes, and Windows, without any effort.)
:::

:::Warning
This is Unix focused, since I donʼt know a whole lot about Windows dev and I donʼt really wanna learn :3

I think similar concepts generally apply, especially on newer Windows with the NTFS file system, but the commands will be different.

I also donʼt really know the difference between Unix flavors or Linux distros, so you might need to find different commands besides the basic ones (`ln`{.sh}, `mount`{.sh}, `realpath`{.sh} should all be universal, but [e.g.]{t=} `sudo systemctl daemon-reload`{.sh} is not universal).
:::

## Overview
<!-- positionality -->

The big distinction between links and mounts is that links live on the filesystem while mounts are created by the operating system as it runs.

This means that mounts are actually temporary: they donʼt persist across reboots, for example, unless the operating system knows how to recreate them, [e.g.]{t=} by detecting available drives or by reading special configuration files like [`/etc/fstab`{.path}](https://wiki.archlinux.org/title/fstab).

Since symlinks and hardlinks are written to the filesystem, they are naturally persistent.

:::{.Key_Idea box-name="Key takeaway"}
My first piece of advice: you almost always want symlinks (`ln -s`{.sh}) as opposed to hardlinks (`ln`{.sh} without the `-s`{.sh}).
:::

I only used hardlinks for the first time last week, to save a little disk space of `.mp3`s since Iʼm not sure whether I will want to keep their original location or their new location in the future.
(Using hardlinks means it doesnʼt matter which one I delete: the other will still exist.)

:::Warning
Most usages of symlinks, hardlinks, bind mounts make certain assumptions about what is changing, and what is not changing.

Sometimes subtle differences can make a difference, or at least be observable.
But depending on how static you expect your files and directories to be, the answer could be that it simply does not matter in the end!

Like, most of the time I donʼt care whether a symlink is relative or absolute, because it is not going to move, and nor is the thing it is pointing to.

On the other hand, in my shared drives that I want to mount from multiple Linux OSes, my only option is to symlink some locations, since the paths they are pointing to do not exist on the original filesystem.

Can you hardlink symlinks? I donʼt know! Maybe??
:::

## Copying (not linking)

If you donʼt want to make a link, `cp`{.sh} (for files) and `cp -r`{.sh} (for directories: `-r` for ***r***ecursive) got you covered.

Changes to one are not reflected in the other, deleting one does nothing to the other, and disk usage is (probably) doubled.^[New filesystems like btrfs could avoid adding disk space.]

The main reason I want to bring this up is to establish this convention:

:::{.Key_Idea box-name="Convention"}
In the commands that create these things, `ln -s`{.sh} (symlink), `ln`{.sh} (hardlink), `mount`{.sh} (mount), `mount --bind`{.sh} (bind mount), the last argument is always the path that will be created or overriden.

That is, the argument order is similar to `cp`{.sh} or `cp -r`{.sh}.

The “to”/“from” terminology just gets really confusing, since the link points **back *to*** the original file that it acts like it was copied **from** …
:::

## Symlinks

Symlinks are pretty simple: they are plain text files marked with a special tag to interpret the file contents as a relative link.
You generally create them with `ln -s $contents_of_link $new_symlink`{.sh}.
This command stands for:

- Create a ***l***i[***n***]{}k (`ln`{.sh})
- of the ***s***ymbolic variety (`-s`{.sh})
- where the link points to a location `$contents_of_link`{.sh}, hopefully one that exists
- and the link lives at the new location `$new_symlink`{.sh} that doesnʼt exist yet

### Why

Symlinks are a good default.
They are generally what I reach for.

More specifically: If you have an original file or directory, and you also want it to appear somewhere else for convenience, use a symlink.

I use them a lot to organize files for convenience, to create shortcuts to get to directories quicker, all that good stuff.

:::{.Example box-name=Examples}
Various examples from my shell history:

```sh
# Easy access to remember where `nginx` configuration lives:
ln -s /usr/local/etc/nginx ~/Documents/Hacking/nginx
ln -s /usr/local/etc/nginx/nginx.conf ~/Documents/Hacking/nginx.conf
```

```sh
# Put all the FlightGear stuff in one directory:
git clone ... flightgear
git clone ... simgear
git clone ... fgdata
ln -s ~/.fgfs fghome
ls # fgdata fghome flightgear simgear
ls -l fghome # fghome -> /home/monoidmusician/.fgfs
```

```sh
# Oops I guess NodeJS wants a `.mjs` file for ESM modules:
ln -s index.js index.mjs
# (Editing both files is identical, etc.)
```
:::

Note that symbolic links are not at all symmetrical!
If the original file goes away, the symlink becomes broken.
When the original file comes back, the symlink works again.

Remember: itʼs literally just a special piece of text on disk, saying “hey, look at this other path instead”.

### Transparency

These links are relatively transparent: if you want to read a file path, it will resolve symlinks (recursively), and read whatever it encounters there.
The `realpath`{.sh} command is the standard way to do this file path resolving.
(Note that it also changes relative paths to absolute paths from the filesystem root.)

If you want to list the files in a directory, it will also resolve symlinks.

### Observability

Symlinks, despite being rather transparent, are still very observable/inspectable.
One way to see them is with `ls -l`{.sh}

### Gotchas

#### Relative paths

This tripped me up for a while: `ln -s ../path/to/existing/file ./path/to/new/symlink`{.sh}.

The first argument is what will be written to the symlink! Directly!

Relative paths are interpreted *relative to the new symlink*, so they will not show up.

Often I get around this by using `realpath`{.sh}: `ln -s $(realpath ../path/to/existing/file) ./path/to/new/symlink`{.bash}.
Then I donʼt have to worry about it.

If I want a relative link, I will usually `cd`{.sh}/`pushd`{.sh} into the directory of the new symlink, to make sure I am getting it correct.

#### Slashes

Sometimes whether you include a slash on the end of a path matters when dealing with symlinks and not regular directories:

For example, regular `ls -l symlink_dir`{.sh} and `ls -l symlink_dir/`{.sh} with a trailing slash will do different things: the first will show the symlink itself `symlink_dir -> its_origin/`{.path} and the second will show the contents of `its_origin/`{.path}.

#### Resolving

When resolving a symlink, note that relative paths apply *before* symlinks get resolved.

So if `TODO`.

## Hardlinks

Hardlinks mean that two files share the same space on disk.

Hardlinks are interesting because they operate at a slightly deeper level of the filesystem: {TODO: directory listings and same disk location}.

### Why

Good for reducing disk usage.
Particularly easy to reason about when the contents are not going to change ([e.g.]{t=} `/nix/store`{.path}).

It seems pretty rare to me that you actually expect to modify the file and see those modifications in both places (TODO).

Good if you donʼt know which path you consider “canonical”.

### Transparency

yeah.

### Observability

Itʼs a lot trickier to observe hardlinks: the files donʼt directly point to each other.

This means that the filesystem needs to keep track of how many paths point to the same {TODO: disk location}, and .

`ls -l`{.sh} also helps here, like for symlinks: although this time, it just indicates that a {TODO: disk location} is shared in the second column, if it is greater than `1`{.dv}.

## Mounts

Regular mounts are from raw disk devices or plain files even (like ISOs) which have the binary filesystem laid out on them.

There are many other types of mounts!

### Why

Your root filesystem at `/`{.path} is mounted during boot.

Partitions from other disks and drives (like thumbdrives) usually get mounted automatically by the OS, either during boot or when they are inserted and detected.
Again: they just have raw memory on them, and the filesystem is a convention for how that memory should be laid out.
It is then interpreted by the OS (or by software like FUSE) to produce a usable directory tree out of it, and then it is made accessible from the root filesystem.

:::Details
`/dev/`{.path} holds.

You can mount things anywhere.
:::

In addition to external storage, you can also take regular files.

### Gotcha

The mount point already must exist on the filesystem.
(Tbh I donʼt know why: so it gets permissions? so the filesystem knows there is something there and doesnʼt scream?)

Too often I create a mount in `/etc/fstab`{.path}, reload it with `mount -a`{.sh} and `sudo systemctl daemon-reload`{.sh} and then I find that I forgot to `mkdir`{.sh} the mount point first.

The flip side is that any contents that are already at the mount point get shadowed until the bind mount disappears.
(Can you save them? Probably! Definitely not with symlinks, but maybe with hardlinks, maybe with bind mounts … I really do not know.)

## Bind Mounts

Bind mounts are cool, itʼs nice that Linux makes them so easy.

Bind mounts act like regular mounts, they just create a new directory tree somewhere, but instead of linking to like an external drive or some other raw filesystem, it links to part of an existing filesystem.

:::Note
Bind mounts are commonly used with virtualization software like Docker to link resources to and from containers.
:::

https://unix.stackexchange.com/questions/198590/what-is-a-bind-mount#answer-198591

### Transparency

Pretty darn transparent.
You wouldnʼt even know that youʼre looking at a bind mount.

### Observability

Difficult.
You canʼt just inspect the location it was mounted to and see that it is special.

You can look through the output of `findmnt`{.sh}, but it doesnʼt directly tell you what the `mount --bind`{.sh} command was, exactly …

See: https://unix.stackexchange.com/questions/18048/list-only-bind-mounts

### Creation

`mount --bind /existing_target /shadow_asdsf`{.sh}

`mount -a`{.sh}

## Extras

### MonoidMusician

Iʼve started keeping my important partitions in a new root directory `/~/`{.path} and then mount the partitions there normally: `/~/Backup`{.path}, `/~/Media`{.path}, `/~/Shared`{.path}, `/~/Universal`{.path}, and so on.
Then I do bind mounts from those into my home directory: `mount --bind /~/Shared/PureScript $HOME/PureScript`{.sh}, etc.
I store these in [`/etc/fstab`{.path}](https://wiki.archlinux.org/title/fstab) so they get set up automatically:

```fstab
/~/Shared/PureScript /home/monoidmusician/PureScript none defaults,bind 0 0
/~/Shared/FlightGear /home/monoidmusician/FlightGear none defaults,bind 0 0
/~/Universal/TerraSync /home/monoidmusician/TerraSync none defaults,bind 0 0
```

But first it needs some setup:

```fish
# Create the location on the mounted disk
mkdir /~/Shared/PureScript
# Clone the purescript/purescript repo to `/~/Shared/PureScript/purescript`
pushd /~/Shared/PureScript; git clone (ghremote purescript/purescript) purescript; popd
# Create the location we will mount to
mkdir ~/PureScript
# Add the above to `/etc/fstab`
sudo vim /etc/fstab
# Refresh mounts
sudo mount -a
# Do `systemd` stuff based on `/etc/fstab`
# (`mount -a` suggests this, at least on Fedora)
sudo systemctl daemon-reload
# Now verify that it exists
ls ~/PureScript/purescript
```

### `/dev/`{.path}

stands for “devices” not “development”.

https://unix.stackexchange.com/questions/188886/what-is-in-dev-proc-and-sys

### `chroot`{.sh}

`chroot`{.sh} is deep magic that lets you create an alternate universe where your filesystem root is different.

One of its primary uses is debugging operating systems from other operating systems: rather than launch a new kernel, you can just `chroot`{.sh} and pretend you *are* the other computer/whatnot.
Virtualization without virtualization :P

Before you go into the `chroot`{.sh} you can still establish links to outside stuff.
So itʼs not airtight security, but it can help with it.

### See whatʼs going on.

for files, directories, links, mounts, disks, partitions, ...

- `ls -alh`{.sh}
- `findmnt`{.sh}
- `du`{.sh}
- `df`{.sh}

- Disk Inventory X (Mac)
- GNOME Disk Utility
- KInfoCenter
