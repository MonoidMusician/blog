import quart
from quart import Quart, websocket
from werkzeug.security import safe_join

import sys, os
import re

app = quart.Blueprint('redirect', __name__, url_prefix='')

# Lift a function to work on pairs of paths
def bi(f: Callable[[str], Optional[str]]):
    def inner(p: str, q: str) -> Tuple[str, str]:
        if p is q or p == q:
            r = f(p)
            return (r, r)
        return (f(p), f(q))
    return inner

# String without suffix, or None
def trim_suffix(s: str, ext: str) -> Optional[str]:
    if s.endswith(ext):
        return s[:-len(ext)]
    return None

# Strip a solitary .html suffix, not .md.html for example
def implicit_html(p: str, q: str):
    if p != q: return
    if clean := trim_suffix(p, ".html"):
        if "." not in clean: return (p, clean)

def implicit_index(p: str, q: str):
    if p != q: return
    if p == "index" or p == "index.html":
        return "/"
    if (clean := (trim_suffix(p, "/index") or trim_suffix(p, "/index.html"))) is not None:
        return clean + "/"

identity = lambda p, q: (p, q)

re_sep = re.compile(r"[-_\s]+")

# Modifications to try
mods = [
    [
        implicit_index,
        identity,
    ],
    [
        bi(lambda p: re.sub(re_sep, "-", p)),
    ],
    [
        bi(lambda p: p + ".html")
    ],
    [
        implicit_html,
        identity,
    ],
]

# Try applying an option. Handles types and value errors.
def try_option(path: str, option):
    (file_path, user_path) = path
    try:
        new_path = option(file_path, user_path)

        if new_path is not None and new_path != (None, None):
            if type(new_path) is str:
                return new_path, new_path
            if type(new_path) is tuple and len(new_path) == 2 and type(new_path[0]) is str and type(new_path[1]) is str:
                return new_path
            raise TypeError(new_path)
    except ValueError:
        return None

# Apply all of the logic to determine if a redirect exists. It maintains a
# dictionary of file path to user path (URL).
def logic(original_path: str, cwd: Optional[str] = None):
    if ".." in original_path.split("/"):
        return None

    relative = lambda rel: os.path.join(cwd, rel) if cwd is not None else rel

    if not implicit_index(original_path, original_path) and not implicit_html(original_path, original_path):
        if os.path.exists(relative(original_path)):
            return None

    paths: dict[str, str] = {original_path: original_path}

    for options in mods:
        seen = set()
        # Try every combination of path and option
        new_paths = (
            result
            for old_path in paths.items()
            for option in (options if identity in options else [identity, *options])
            # filter map
            if (result := try_option(old_path, option)) is not None
            # deduplicate
            if result[0] not in seen
            if seen.add(result[0]) or True
        )
        paths = dict(new_paths)

    # print(original_path, paths)

    checked = set() # only check a file path once
    for (file_path, user_path) in paths.items():
        if file_path in checked: continue

        # If the path exists, return the original path
        present = os.path.exists(relative(file_path))
        if present:
            if user_path == original_path:
                return None
            return user_path
        checked.add(file_path)
    return None


@app.get("<path:path>")
async def redirect(path):
    try:
        new_path = logic(path, '../static/')
        if new_path is not None:
            return quart.redirect(new_path, 301)
    except Exception as e:
        print(e)
    return ("Not Found", 404)

if __name__ == '__main__':
    args = sys.argv[1:]
    if len(args) == 1:
        a = args[0]
        result = logic(a)
        if result is not None: print(result)
    else:
        print([logic(a) for a in args])
