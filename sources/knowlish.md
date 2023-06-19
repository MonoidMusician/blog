---
title: Knowlish
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## CLI

MacOS SIGINFO: Ctrl+T, https://stuff-things.net/2016/04/06/that-one-stupid-dd-trick-and-the-ballad-of-siginfo/

### Git

- [`git clone --filter=blob:none <url>`{.bash}](https://github.blog/2020-12-21-get-up-to-speed-with-partial-clone-and-shallow-clone/) is a new better alternative to `git clone --depth=1 <url>` (with `git clone --filter=tree:0 <url>` skipping downloading trees but not recommended for dev work since it will redownload trees more often than necessary)
- `git push --set-upstream origin $(git branch --show-current)`{.bash}
- View history of file (commits + patches): `git log -p -- foo.sh`{.bash}
- Local/private version of `.gitignore`: `.git/info/exclude`

## Applications

### VSCode

- [Right click to add file in same parent folder](https://github.com/microsoft/vscode/issues/83693#issuecomment-782810618)

### ffplay

- `p`{.key}/`Space`{.key} to pause
- `s`{.key} to step to next frame

## Formats

- https://gchq.github.io/CyberChef/
- https://www.onlinemp4parser.com/

