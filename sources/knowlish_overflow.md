---
title: Knowlish (Overflow)
subtitle: "Stuff that I barely understand that I came across that I want to keep around"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Basically a record of information Iʼve come across.
Provided without warranty or commentary.
Just some pointers to things that worked on my systems.
If you want to try these things out, youʼll have to follow the pointers to understand where they could apply and what they do and what are the risks, &c.
(Tbh most of the risks are pretty low.
But itʼs good to know how to roll it back in any case!)

## SysAdmin

Wake on LAN

:   Enable in BIOS/UEFI.
:   Enable in Fedora(?):

    `sudo nmcli connection show`{.bash}

    `sudo nmcli connection show "Wired connection 1"`{.bash}

    `sudo nmcli connection modify "Wired connection 1" 802-3-ethernet.wake-on-lan magic`{.bash}

    [[source]](https://www.reddit.com/r/Fedora/comments/uji6dw/anyone_have_a_guide_on_how_to_activate_wake_on/) [[source]](https://www.reddit.com/r/Fedora/comments/fbd8ny/cannot_setup_wake_on_lan_on_fedora_31/)
    [[docs]](https://developer-old.gnome.org/NetworkManager/stable/settings-802-3-ethernet.html)

    `sudo nmcli connection modify $YOURSSID 802-11-wireless.wake-on-wlan magic`{.bash} (did not work for me)

    [[docs]](https://developer-old.gnome.org/NetworkManager/stable/settings-802-11-wireless.html)
:   `ifconfig`{.bash}
:   `wakeonlan`{.bash}
    (maybe `brew install wakeonlan`{.bash})
:   `wakeonlan xx:xx:xx:xx:xx:xx`{.bash}

Dismiss (GNOME?) login screen

:   ```bash
    # Lock
    dbus-send --session --dest=org.gnome.ScreenSaver --type=method_call --print-reply --reply-timeout=20000 /org/gnome/ScreenSaver org.gnome.ScreenSaver.SetActive boolean:true

    # Unlock
    dbus-send --session --dest=org.gnome.ScreenSaver --type=method_call --print-reply --reply-timeout=20000 /org/gnome/ScreenSaver org.gnome.ScreenSaver.SetActive boolean:false
    ```

    [[source]](https://lists.fedoraproject.org/archives/list/users@lists.fedoraproject.org/thread/P2QR5N763BU56VN4MWLL7ABBVJBTBXJB/)
    [[source]](https://notes.zerodogg.org/GNOME/lock-unlock-cli/)

mDNS (`.local` addresses)

:   Add `search company.local` to `/etc/resolv.conf` to get subdomains `app.company.local` [[source]](https://www.reddit.com/r/Fedora/comments/19eeeo6/enabling_mdns_on_fedora_39_still_doesnt_work/)
:   https://serverfault.com/questions/211982/adding-more-than-one-local-address-using-osx-bonjour
:   https://andrewdupont.net/2022/01/27/using-mdns-aliases-within-your-home-network/

bind mounts

:   `/etc/fstab`{.filepath} and `sudo mount -a`{.sh}

:   https://apple.stackexchange.com/questions/197029/how-do-you-mount-bind-a-local-directory

    > Disadvantage of fuse is that it do not support inotify events
:   https://jcol.me/2019/12/20/bind-mounting-on-macos/

Samba

:   https://docs.fedoraproject.org/en-US/quick-docs/samba/

    Note: `[share]` in `/etc/samba/smb.conf`{.filepath} is the name of the share, visible over the network.

SSH

:   [Keepalive](https://superuser.com/questions/699676/how-to-prevent-ssh-from-disconnecting-if-its-been-idle-for-a-while):
    - `ssh -o "ServerAliveInterval 60" -o "ServerAliveCountMax 120" yourserver`{.sh} to override for a single session
    - Or add to `~/.ssh/config`{.filepath}

## Misc. links

- https://github.com/paopre/Spontini (LilyPond, CodeMirror)

