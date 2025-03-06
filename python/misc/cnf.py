# watchexec -w misc/cnf.py -r -c --shell=bash 'mypy --strict --disallow-any-unimported misc/cnf.py && python3 misc/cnf.py'

# Polynomial time verifier for Conjunctive Normal Form

# Configuration variables
debug = False
max_size = 25 # max number of variables, conjunctions, and disjunctions

# This is the denotation of what we are going for: each clause is a conjunction
# of disjunctions of possibly negated variables.
def verify(vars: list[bool], clauses: list[list[tuple[int, bool]]]) -> bool:
    return all(
        any(
            vars[var] != negated
            for var, negated in clause
        )
        for clause in clauses
    )

# It uses a Turing Machine tape but is not quite faithful to a Turing machine's
# evaluation model yet. Eventually it would be nice to translate it into
# exact Turing machine instructions and prove asymptotics for each subroutine.

# This Python code ensures the tape is “typed”: each variable has a well defined
# role that we check each time we access it, and we only return the value from
# it, not the role. That is, the description of the slot on the tape is purely
# metadata, it is not used by the algorithm. This typing discipline makes bugs
# much easier to detect and also means that the tape is legible when printed.

# Initialize the data layout on the tape. The data layout is very careful,
# so that we can scan back and forth through arbitrary lengths of data. It is
# actually pretty elegant that such a simple scheme works for this problem,
# but it is already clear that it does not scale well to more complicated setups.
def init_tape(vars: list[bool], clauses: list[list[tuple[int, bool]]]) -> list[tuple[str, bool]]:
    nvar: int = len(vars)
    Clen: int = len(clauses)
    Dlen: list[int] = [len(clause) for clause in clauses]

    # A representation of a variable, according to the solution we are checking
    def tape_var(i: int, value: bool) -> list[tuple[str, bool]]:
        # mut mark is used for matching the variables stored in the header with the
        # variable indicated in the CNF term: each time you need to look up the
        # value of the variable, you have to scan back and forth, marking each
        # variable off, before you arrive at the end of the nat. Then you can read
        # the next *unmarked* variable, and unmark the variables on your way back.
        return [
            ("notSIGIL", False), # <notSIGIL=0>
            ("mut mark", False), # <mut mark=0>
            ("const value", value), # <const value>
        ]
    # All of the variables represented on the stack, with SIGIL as a separator
    def tape_vars() -> list[tuple[str, bool]]:
        # nvar variables, 3 bits each, in reverse order, since SIGIL is the neutral
        # point of the tape (essentially), and variables are read in relation to it.
        return [
            bit
            for i, value in enumerate(reversed(vars))
            for bit in tape_var(i, value)
        ] + [
            ("SIGIL", True), # SIGIL, demarcates the variables from the CNF
        ]
    # A representation of an index for a variable. They are constant size unary
    # numerals, to ease asymptotic analysis. They are disposable: they are read
    # once, upon which all of the bits are latched high, and then the term is
    # deleted. Although it would be possible to restore it, while unmarking
    # variables, but it just is not necessary.
    def tape_nat(value: int) -> list[tuple[str, bool]]:
        assert 0 <= value < nvar
        return (nvar - value) * [
            ("notEndOfNat", False), # notEndOfNat
            ("accountedFor", True), # accountedFor
        ] + value * [
            ("notEndOfNat", False), # notEndOfNat
            ("latch accountedFor", False), # latch accountedFor
        ] + [
            ("endOfNat", True), # endOfNat=1
        ]
    # The representation of the clauses, until EOF. The clauses of
    # Conjunctive Normal Form are joined with conjunction: all must be true
    # to verify the solution.
    def tape_clauses() -> list[tuple[str, bool]]:
        return [
            bit
            for clause in clauses
            for bit in tape_clause(clause) + [
                ("noEOF", False), # noEOF=0
                ("C", False), # C=0
            ]
        ] + [
            ("EOF", True), # EOF
        ]
    # An individual clause of Conjunctive Normal Form is a *disjunction*
    # of terms
    def tape_clause(clause: list[tuple[int, bool]]) -> list[tuple[str, bool]]:
        return [
            bit
            for term in clause
            for bit in tape_term(term)
        ]
    # A term is a variable that may be negated
    def tape_term(term: tuple[int, bool]) -> list[tuple[str, bool]]:
        var, negated = term
        return [
            ("noEOF", False), # noEOF=0
            ("D", True), # D=1
            ("negated", negated), # negated
        ] + tape_nat(var) + [
            ("notSIGIL", False), # notSIGIL=0
        ]
    return tape_vars() + tape_clauses()

# Global variables for the state of the Turing(-ish) Machine
tape: list[tuple[str, bool]] = []
steps = 0
ptr: int = 0
result: bool | None = None

def prev() -> None:
    global ptr, steps
    ptr -= 1
    assert 0 <= ptr < len(tape)
    steps += 1
def next() -> None:
    global ptr, steps
    ptr += 1
    assert 0 <= ptr < len(tape)
    steps
def finish(r: bool) -> None:
    global result
    result = r

# Cheaty
def seekTo(*desbits: tuple[str, bool]) -> None:
    while tape[ptr] not in desbits:
        next()
# Cheaty
def backTo(*desbits: tuple[str, bool]) -> None:
    while tape[ptr] not in desbits:
        prev()
# Cheating
def delete(*desbits: tuple[str, bool | None]) -> None:
    global steps
    peek(*desbits)
    tape[ptr:ptr+1] = []
    steps += 1

# Peek at the current pointer
def peek(*desbits: tuple[str, bool | None]) -> bool:
    val = tape[ptr]
    for desc, bit in desbits:
        if bit is None:
            if val[0] == desc: return val[1]
        if val == (desc, bit): return val[1]
    raise ValueError("Got", val, "expected", *desbits, "at", ptr)
# Peek and advance
def read(*desbits: tuple[str, bool | None]) -> bool:
    val = peek(*desbits)
    next()
    return val
# Verify and advance
def skip(*desbits: tuple[str, bool | None]) -> None:
    read(*desbits)
# Peek and reverse
def back(*desbits: tuple[str, bool | None]) -> bool:
    val = peek(*desbits)
    prev()
    return val
# Assign the current pointer
def asgn(desbit: tuple[str, bool]) -> None:
    tape[ptr] = desbit

# Run the machine on the current `tape`
def main() -> tuple[bool, int]:
    global ptr, result, steps
    ptr, steps, result = 0, 0, None
    return loop(), steps

# The main loop
def loop() -> bool:
    global result
    # Start from the well known spot SIGIL
    seekTo(("SIGIL", True))
    while True:
        # Lookup the value of the variable of the first term we see.
        # This oscillates back and forth around SIGIL
        value = lookup()
        if debug: print(f"lookup {value} {result}")
        # Check if we have a result
        if result is not None:
            if debug: print_tape(prefix="fin.")
            return result
        # If not, we need to process the variable and advance through a term or
        # clause, depending on whether the term was true or false. This puts the
        # next term of interest right next to SIGIL, so then we are ready to loop.
        compress(value)
# Look up the variable referenced by the head term
def lookup() -> bool:
    """
    01. precondition: all variables are unmarked
    02. start at SIGIL
    03. read <EOF=1|noEOF=0>
    04. if EOF: exit(1) # verified true, no conjunctive clause was falsified
    05. read <D=1|C=0>
    06. if C: exit(0) # verified false, false disjunctive clause among conjuctive clauses
    07. skip <negated>
    """
    skip(("SIGIL", True))
    EOF = peek(("EOF", True), ("noEOF", False))
    if EOF:
        finish(True)
        return True
    skip(("noEOF", False))
    D = read(("D", True), ("C", False))
    if not D:
        finish(False)
        return False
    skip(("negated", None))
    """
    08. for each successor in <var:nat…> (i.e. each pair of bits in the encoding),
            we want to mark the second bit, then turn around and mark one more var
            on the other side (the last unmarked one), in O(nvar^2)
    09. when we’ve marked all bits, seek back to the last unmarked var, in O(nvar)
    10. read its <value>
    11. read forward to SIGIL, unmarking variables as you go, in O(nvar)
    """
    peek(("notEndOfNat", False))
    backTo(("SIGIL", True))
    # Start at SIGIL and advance to the next successor in the <var:nat…> that
    # needs to be marked off. Returns True if they have all been accounted for.
    def fromSigilToNextSucc() -> bool:
        skip(("SIGIL", True))
        skip(("noEOF", False))
        skip(("D", True))
        skip(("negated", None))
        while True:
            EON = read(("notEndOfNat", False), ("endOfNat", True))
            if EON:
                backTo(("SIGIL", True))
                return False
            accountedFor = peek(("accountedFor", True), ("latch accountedFor", None))
            if not accountedFor:
                peek(("latch accountedFor", False))
                asgn(("latch accountedFor", True))
                backTo(("SIGIL", True))
                return True
            read(("accountedFor", True), ("latch accountedFor", True))
    # Walk backwards from SIGIL to mark off another variable, corresponding to
    # a successor term.
    def markLastVarBeforeSigil() -> None:
        back(("SIGIL", True))
        while True:
            back(("const value", None))
            mark = back(("mut mark", None))
            if not mark:
                skip(("notSIGIL", False))
                asgn(("mut mark", True))
                seekTo(("SIGIL", True))
                return
            back(("notSIGIL", False))
    # Once every successor has been accounted for, we look at how many variables
    # have been marked and choose the next one, and unmark them all.
    def getUnmarkedVarBeforeSigil() -> bool:
        back(("SIGIL", True))
        # Skip marked variables
        while True:
            value = back(("const value", None))
            mark = peek(("mut mark", None))
            if not mark:
                skip(("mut mark", False))
                skip(("const value", value))
                break
            back(("mut mark", mark))
            back(("notSIGIL", False))
        # Unmark as we go
        while not peek(("SIGIL", True), ("notSIGIL", False)):
            skip(("notSIGIL", False))
            peek(("mut mark", True))
            asgn(("mut mark", False))
            skip(("mut mark", False))
            skip(("const value", None))
        return value
    peek(("SIGIL", True))
    while fromSigilToNextSucc():
        markLastVarBeforeSigil()
    return getUnmarkedVarBeforeSigil()
# If the term is false, delete just the term and continue processing the
# other terms in the disjunction. If the term is true, the disjunction succeeded
# so we can delete the adjacent terms, as it just remains to check the other
# conjunctive *clauses* (with their own disjunctive terms).
def compress(value: bool) -> None:
    """
    1. start at SIGIL
    2. skip <noEOF=0><D=1> # we were just processing an atom
    3. read <not>
    4. result := value XOR not
    """
    skip(("SIGIL", True))
    skip(("noEOF", False))
    skip(("D", True))
    negated = back(("negated", None))
    result = value != negated
    if debug: print(f"{result=} = {value=} != {negated=}")

    def deleteNat() -> None:
        while not peek(("endOfNat", True), ("notEndOfNat", False)):
            delete(("notEndOfNat", False))
            delete(("accountedFor", True), ("latch accountedFor", None))
        delete(("endOfNat", True))

    def deleteDisj() -> None:
        delete(("D", True))
        delete(("negated", None))
        deleteNat()
        delete(("notSIGIL", False))
        delete(("noEOF", False))

    """
    5. delete this atom, in O(nvar^2 * len)
    6. if result: delete the other atoms in this disjunctive clause, in O(nvar^2 * len^2)
    """
    deleteDisj()
    if result:
        while peek(("C", False), ("D", True)):
            deleteDisj()
        delete(("C", False))
        back(("noEOF", False), ("EOF", True))
        delete(("noEOF", False))
        back(("noEOF", False), ("EOF", True))
    else:
        back(("C", False), ("D", True))
        back(("noEOF", False))

    if debug: print_tape(prefix="del.")
    if debug: print()
    peek(("SIGIL", True))

def print_tape(aTape: list[tuple[str, bool]] | None = None, *, prefix: str | None = None) -> None:
    for i, (desc, bit) in enumerate(aTape or tape):
        star = "" if aTape is not None else "*" if ptr==i else " "
        print(f"{prefix or ""}{star}<{desc}={int(bit)}>")

# Simulate the Turing machine and check that it gives the correct result
def test(vars: list[bool], clauses: list[list[tuple[int, bool]]]) -> tuple[bool, bool, int]:
    global tape
    init = init_tape(vars, clauses)
    tape = list(init)
    result, nsteps = main()
    verified = verify(vars, clauses)
    if debug: print(result, verified)
    if not debug:
        assert result == verified, (f"{result=} != {verified=}", vars, clauses)
    return result, verified, nsteps

# Generate a random CNF of reasonable size
def generate() -> tuple[list[bool], list[list[tuple[int, bool]]]]:
    import random as r
    nvar = r.randint(1, 5 if debug else max_size)
    vars = [r.choice([True,False]) for _ in range(nvar)]
    Clen = r.randint(0, 3 if debug else max_size)
    Dlen = [r.randint(0, 3 if debug else max_size) for _ in range(Clen)]
    clauses = [
        [
            (r.randrange(0, nvar), r.choice([True,False]))
            for _ in range(Dlen[i])
        ]
        for i in range(Clen)
    ]
    return vars, clauses

try:
    if debug:
        print_tape(init_tape([True, False], [
            [(0, False), (1, True)],
            [(1, True), (0, False)],
        ]))
    results = []
    for v0 in [True, False]:
        for v1 in [True, False]:
            if debug:
                print()
                print("------------")
                print()
            r = test([v0, v1], [
                [(0, False), (1, True)],
                [(1, False)],
            ])
            results.append(r)
    # print(results)
    if not debug:
        # import random
        # random.seed(157842)
        step_counts = []
        for _ in range(0, 100):
            _, _, nsteps = test(*generate())
            step_counts.append(nsteps)
        print(" ".join(map(str, step_counts)))
        print("Average:", int(sum(step_counts) / len(step_counts)), "steps")
except Exception as e:
    print(e)
    print_tape()
    raise
