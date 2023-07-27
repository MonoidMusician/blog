---
title: Knowlish
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## CLI

- MacOS SIGINFO: Ctrl+T, https://stuff-things.net/2016/04/06/that-one-stupid-dd-trick-and-the-ballad-of-siginfo/
- Print side-by-side: `pr -mtw $(tput cols)`{.bash}
  - not a great way to do it
  - will cut off long lines
  - does not automatically size to content, just console widith

### Git

- [`git clone --filter=blob:none <url>`{.bash}](https://github.blog/2020-12-21-get-up-to-speed-with-partial-clone-and-shallow-clone/) is a new better alternative to `git clone --depth=1 <url>` (with `git clone --filter=tree:0 <url>` skipping downloading trees but not recommended for dev work since it will redownload trees more often than necessary)
- `git push --set-upstream origin $(git branch --show-current)`{.bash}
- View history of file (commits + patches): `git log -p -- foo.sh`{.bash}
- Local/private version of `.gitignore`: `.git/info/exclude`
- Remove untracked and/or ignored files via [`git clean`{.bash}](https://git-scm.com/docs/git-clean):
  - Untracked: `git clean -f`{.bash}, dry-run: `git clean -n`{.bash}
  - Ignored: `git clean -fX`{.bash}, dry-run: `git clean -nX`{.bash}
  - Both: `git clean -fx`{.bash}, dry-run: `git clean -nx`{.bash}

## GUI

### VSCode

- [Right click to add file in same parent folder](https://github.com/microsoft/vscode/issues/83693#issuecomment-782810618) (extension)
- Alt+up/down to reorder lines
- Alt+click for multiselect

### ffplay

- `p`{.key}/`Space`{.key} to pause
- `s`{.key} to step to next frame

## Data Formats/Parsers

- https://gchq.github.io/CyberChef/
- https://www.onlinemp4parser.com/

