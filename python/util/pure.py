def replacers(replace_pairs):
    def the_replacer(s):
        for (search, replace) in replace_pairs:
            s = s.replace(search, replace)
        return s
    return the_replacer

def scheme_string(s):
    return '"'+replacers([
        ("\\", "\\\\"),
        ("\"", "\\\""),
        ("\r", "\\r"),
        ("\n", "\\n"),
        ("\t", "\\t"),
    ])(s)+'"'


class NotUnique(Exception):
    def __new__(self, values):
        self.values = values
    def __repr__(self):
        if not len(self.values):
            return "Received no value at all"
        return "Was not unique, had length {}".format(len(self.values))

def the(values):
    vs = list(values)
    if len(vs) == 1:
        return vs[0]
    raise NotUnique(vs)
