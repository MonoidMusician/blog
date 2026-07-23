There are three main types of computation in a pure functional programming language:

- machine-word computation (ints and floats)
- case analysis on constructors, including recursors and elminators (induction)
- function application

The first is not essential, but it is useful to include it to navigate the spectrum from pen-and-paper formalized math to machine-code implementation.

(Lazy thunks are important too, but I will consider them part of the framework for computation, not a member of the types of computation.)

Functions and datatype constructors have a weird relationship.

With datatypes, we expect `not False`{.haskell} to compute first to an actual constructor `True`{.haskell}, and *then* the case analysis `if (not False) then _ else _`{.haskell} gets to run.

With pure combinators, we have the definitions `True t f = t`{.haskell} and `False t f = f`{.haskell}, and you know that `(not False) t f`{.haskell} eventually reduces to `t`{.haskell}, but that does not mean that `not False`{.haskell} reduces to `True`{.haskell}.
You can force this reduction with `asBool b = b True False`{.haskell} to compute an expression to a canonical constructor.

So, do we get rid of constructors?
Probably not.
Constructors are a good place to hang your hat at the end of the day.
They serve other purposes than raw computation.

On the machine side, they have known storage requirements, known performance.
