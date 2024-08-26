---
title: Knowlish
subtitle: "Little bits of tips ʼnʼ tricks I want to keep around.<br/>You may find it useful too sometimes \\^.\\^"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

_[Overflow](knowlish_overflow.html)_

## CLI

- Print side-by-side: `pr -mtw $(tput cols)`{.bash}
  - not a great way to do it
  - will cut off long lines
  - does not automatically size to content, just console width
- Type decimal numbers, echo and copy hexadecimal escapes:
  ```fish
  cat (echo "obase=16" | psub) - | bc | xargs -I % -n 1 bash -c "echo -n \\\\x% | pbcopy; echo \\\\x%"
  ```
- [`watchexec`{.sh}](https://github.com/watchexec/watchexec/tree/main/crates/cli#usage-examples) to watch files & directories and then rerun `make`{.sh} or whatnot. better than `entr`{.sh}
- [`date '+%Y-%m-%d-%H-%M-%S'`{.bash}](https://stackoverflow.com/questions/1401482/yyyy-mm-dd-format-date-in-shell-script#answer-1401495)
- [`grep -Fxq "One specific line exists in" many_lines.txt`{.bash}](https://stackoverflow.com/questions/4749330/how-to-test-if-string-exists-in-file-with-bash#answer-4749368)
- pipe into `sponge filename.txt`{.bash} if `filename.txt` is used earlier in the command (since output redirection erases the file)

### MacOS

- SIGINFO: Ctrl+T, https://stuff-things.net/2016/04/06/that-one-stupid-dd-trick-and-the-ballad-of-siginfo/
- Clipboard: `pbpaste`{.bash}/`pbcopy`{.bash} (stdout/stdin only)
- Network name (SSID): [`/System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -I  | awk -F' SSID: '  '/ SSID: / {print $2}'`{.bash}](https://stackoverflow.com/questions/4481005/get-wireless-ssid-through-shell-script-on-mac-os-x#answer-4481019)

### Ubuntu (GNOME?)

- MouseKeys fast: `xkbset ma 60 10 10 5 2`{.bash}

### Git

- [`git clone --filter=blob:none <url>`{.bash}](https://github.blog/2020-12-21-get-up-to-speed-with-partial-clone-and-shallow-clone/) is a new better alternative to `git clone --depth=1 <url>` (with `git clone --filter=tree:0 <url>` skipping downloading trees but not recommended for dev work since it will redownload trees more often than necessary)
- `git push --set-upstream origin $(git branch --show-current)`{.bash}
- View history of file (commits + patches): `git log -p -- foo.sh`{.bash}
- Local/private version of `.gitignore`: `.git/info/exclude`
- Remove untracked and/or ignored files via [`git clean`{.bash}](https://git-scm.com/docs/git-clean):
  - Untracked: `git clean -f`{.bash}, dry-run: `git clean -n`{.bash}
  - Ignored: `git clean -fX`{.bash}, dry-run: `git clean -nX`{.bash}
  - Both: `git clean -fx`{.bash}, dry-run: `git clean -nx`{.bash}
- Current branch name: `git branch --show-current`{.bash}
- Upstream for current branch (pretty name): `git rev-parse --abbrev-ref --symbolic-full-name @{upstream}`{.bash}
- See what changes you are about to push: `git diff @{u} HEAD`{.bash}

  <details class="Details">

  <summary>Explanation</summary>

  First it [finds the upstream `@{u}`/`@{upstream}`](https://git-scm.com/docs/git-rev-parse#Documentation/git-rev-parse.txt-emltbranchnamegtupstreamemegemmasterupstreamememuem) for the current branch.

  `HEAD` means to compare against the currently committed changes, not whatever files are in your working tree.

  </details>

### tmux

- Detach: `ctrl+b` then `d`.
- Reattach: `tmux a`{.sh}.
- Switch: `ctrl+b` then `s`.

## Runtimes & Debugging

### Browser JS

- `new URLSearchParams(window.location.search).{get,getAll,has,entries,...}(...)`{.js}
  - `.entries` returns an iterator??
- POST JSON to server:

  ```js
  fetch('localhost', {
      method: 'POST', body: JSON.stringify(yourData),
      headers: { "Content-Type": "application/json" },
  }).then(r => r.json());
  ```

### NodeJS

- Full stack trace: `NODE_OPTIONS='--stack-trace-limit=10000'`{.bash}
- `util.inspect.defaultOptions.depth = null;`{.js} (no commandline option :sad:)

### Erlang

- `io:format(user, <<"~p~n">>, [Object])`{.erl} in tests and `rp(Object).`{.erl} in the REPL
- `f(VarName)`{.erl} to unbind a variable in the REPL, `f()`{.erl} to unbind them all.

### Bash

- [`set -euxo pipefail`{.bash}](https://gist.github.com/mohanpedala/1e2ff5661761d3abd0385e8223e16425)
- `cd -- "$(dirname $0)"`{.bash}
- Simple argument handling:
  ```bash
  case "$1" in
    "--no-build")
      shift 1;;
    *)
      make;;
  esac
  ```
- Conditionals, flags like `-e`, `-f`: https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html

### Fish

- `cd (dirname (status -f))`{.fish} (or `status --current-filename`{.fish})

  (do I need quotes? did we fix that in fish?)

- Compare directory listings, excluding hidden files

  ```fish
  git diff --no-index (cd $ROOT1; find . -not -path '*/\.*' -type f | sort | psub) (cd $ROOT2; find . -not -path '*/\.*' -type f | sort | psub)
  ```

### Nix

- Nix test installation:

  `nix-shell -p nix-info --run "nix-info -m"`{.sh}

## GUI

### VSCodium / VSCode

- Language-specific settings: see https://code.visualstudio.com/docs/getstarted/settings.
  - Settings search: `@lang:markdown`{.awk}.
    This will apply those settings to that language specifically!
    - You can discover this by clicking the filter icon at the far right of the search bar.
  - Json Settings: add a key with the language name in brackets, `"[markdown]": { ... your settings ... }`{.js}.
  - Can I just say how I miss Atomʼs settings menu?
    It was so clean, in comparison.
- Extensions live in these places:
  - `~/.vscode-oss/extensions`
  - `~/.vscode/extensions`
  - `/Applications/VSCodium.app/Contents/Resources/app/extensions`
  - `/Applications/VSCode.app/Contents/Resources/app/extensions`
  - other OSes??
- `codium --list-extensions`{.bash} & `codium --install-extension`{.bash}

#### Shortcuts

- Keybinds for ctrl+(shift)+tab to just switch tabs, no “most recently used” switcher:

  <details class="Details">

  <summary>Details</summary>

  From [“Is there a quick change tabs function in Visual Studio Code?”](https://stackoverflow.com/questions/38957302/is-there-a-quick-change-tabs-function-in-visual-studio-code/38978993#38978993):

  ```json
  [
    {
        "key": "ctrl+tab",
        "command": "workbench.action.nextEditorInGroup"
    },
    {
        "key": "ctrl+shift+tab",
        "command": "workbench.action.previousEditorInGroup"
    },
  ]
  ```

  (or `workbench.action.{next,previous}Editor`{.bash} to have it apply across windows instead of wrapping around)

  > By default, Ctrl+Tab in Visual Studio Code cycles through tabs in order of most recently used. This is confusing because it depends on hidden state.
  >
  > Web browsers cycle through tabs in visible order. This is much more intuitive.
  </details>
- [Right click to add file in same parent folder](https://github.com/microsoft/vscode/issues/83693#issuecomment-782810618) (extension)
- Alt+up/down to reorder lines
- Alt+click for multiselect

#### Highlighting

- Weird bug where typing `re` in a markdown makes syntax highlighting wonky: it turns out to be caused by the extension file `ocamllabs.ocaml-platform-1.*.*-universal/syntaxes/reason-markdown-codeblock.json`{.bash}, so you can just delete that whole file (you will have to do this at every update).

### ffplay

- `p`{.key}/`Space`{.key} to pause
- `s`{.key} to step to next frame

## Data Formats/Parsers

- [Common MIME types](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types)
- [List of file signatures](https://en.wikipedia.org/wiki/List_of_file_signatures)
- [List of TCP and UDP port numbers](https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers)
- https://gchq.github.io/CyberChef/
- https://www.onlinemp4parser.com/
- https://explainshell.com/
- https://regex101.com/
- https://ijmacd.github.io/rfc3339-iso8601/ (DateTime formats)

### Scripts

- Sum durations of A/V media (`.mp3`s in this case):

  ```fish
  for f in *.mp3; ffprobe -show_entries format=duration -v quiet -i $f | grep -oE '[[:digit:].]+'; end | awk '{s+=$1} END {print s}'
  ```

## Misc

- https://www.keycaps.info/

### MacOS nonsense

- > [“VSCodium.app” can’t be opened because Apple cannot check it for malicious software.](https://github.com/VSCodium/vscodium/issues/228#issuecomment-510788465)

  **Alt** + right click > open the app once, the security dialog will now have an “Open” button, and then it will be fine.
