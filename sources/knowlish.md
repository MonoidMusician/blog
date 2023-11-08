---
title: Knowlish
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

## CLI

- MacOS SIGINFO: Ctrl+T, https://stuff-things.net/2016/04/06/that-one-stupid-dd-trick-and-the-ballad-of-siginfo/
- Mac `pbpaste`{.bash}/`pbcopy`{.bash} (stdout/stdin only)
- Print side-by-side: `pr -mtw $(tput cols)`{.bash}
  - not a great way to do it
  - will cut off long lines
  - does not automatically size to content, just console widith
- Type decimal numbers, echo and copy hexadecimal escapes: `cat (echo "obase=16" | psub) - | bc | xargs -I % -n 1 bash -c "echo -n \\\\x% | pbcopy; echo \\\\x%"`{.fish}
- [`entr`](http://eradman.com/entrproject/) is pretty useful, a little tricky to use ... maybe `watchexec` is better? never used it

### Git

- [`git clone --filter=blob:none <url>`{.bash}](https://github.blog/2020-12-21-get-up-to-speed-with-partial-clone-and-shallow-clone/) is a new better alternative to `git clone --depth=1 <url>` (with `git clone --filter=tree:0 <url>` skipping downloading trees but not recommended for dev work since it will redownload trees more often than necessary)
- `git push --set-upstream origin $(git branch --show-current)`{.bash}
- View history of file (commits + patches): `git log -p -- foo.sh`{.bash}
- Local/private version of `.gitignore`: `.git/info/exclude`
- Remove untracked and/or ignored files via [`git clean`{.bash}](https://git-scm.com/docs/git-clean):
  - Untracked: `git clean -f`{.bash}, dry-run: `git clean -n`{.bash}
  - Ignored: `git clean -fX`{.bash}, dry-run: `git clean -nX`{.bash}
  - Both: `git clean -fx`{.bash}, dry-run: `git clean -nx`{.bash}

### Runtimes & Debugging

#### Browser JS

- `new URLSearchParams(window.location.search).{get,getAll,has,entries,...}(...)`{.js}
  - `.entries` returns an iterator??

#### NodeJS

- Full stack trace: `NODE_OPTIONS='--stack-trace-limit=10000'`{.bash}
- `util.inspect.defaultOptions.depth = null;`{.js} (no commandline option :sad:)

#### Erlang
- `io:format(user, <<"~p~n">>, [Object])`{.erl} in tests and `rp(Object).`{.erl} in the REPL

#### Bash

- [`set -euxo pipefail`{.bash}](https://gist.github.com/mohanpedala/1e2ff5661761d3abd0385e8223e16425)

## GUI

### VSCode

- Language-specific settings: see https://code.visualstudio.com/docs/getstarted/settings.
  - Settings search: `@lang:markdown`{.awk}.
    This will apply those settings to that language specifically!
    - You can discover this by clicking the filter icon at the far right of the search bar.
  - Json Settings: add a key with the language name in brackets, `"[markdown]": { ... your settings ... }`{.js}.
  - Can I just say how I miss Atomʼs settings menu?
    It was so clean, in comparison.
    (Okay, not _that_ clean.)

#### Shortcuts

- Keybinds for ctrl+(shift)+tab to just switch tabs, no “most recently used” switcher:

  <details class="Details">
  From [“Is there a quick change tabs function in Visual Studio Code?”](https://stackoverflow.com/questions/38957302/is-there-a-quick-change-tabs-function-in-visual-studio-code/38978993#38978993):

  ```json
  [
    {
        "key": "ctrl+tab",
        "command": "workbench.action.nextEditor"
    },
    {
        "key": "ctrl+shift+tab",
        "command": "workbench.action.previousEditor"
    },
  ]
  ```

  (or `{next,previous}EditorInGroup`{.bash})

  > By default, Ctrl+Tab in Visual Studio Code cycles through tabs in order of most recently used. This is confusing because it depends on hidden state.
  >
  > Web browsers cycle through tabs in visible order. This is much more intuitive.
  </details>
- [Right click to add file in same parent folder](https://github.com/microsoft/vscode/issues/83693#issuecomment-782810618) (extension)
- Alt+up/down to reorder lines
- Alt+click for multiselect

#### Highlighting

- Weird bug where typing `re` in a markdown makes syntax highlighting wonky: it turns out to be caused by the extension file `ocamllabs.ocaml-platform-1.13.4-universal/syntaxes/reason-markdown-codeblock.json`{.bash}, so you can just delete stuff from there until it goes away.

### ffplay

- `p`{.key}/`Space`{.key} to pause
- `s`{.key} to step to next frame

## OSes

### MacOS nonsense

- > [“VSCodium.app” can’t be opened because Apple cannot check it for malicious software.](https://github.com/VSCodium/vscodium/issues/228#issuecomment-510788465)

  Alt + right click > open the app once, the security dialog will now have an “Open” button, and then it will be fine.


## Data Formats/Parsers

- [Common MIME types](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types)
- https://gchq.github.io/CyberChef/
- https://www.onlinemp4parser.com/

