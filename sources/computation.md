:::{.centered}
_This post is meditation synthesizing different ideas of computation within FP and why lambdas could take over but constructors still persist._
:::
<!-- ʼ -->

There are three main types of computation in a pure functional programming language:

- machine-word computation (ints and floats)
- case analysis on constructors, including recursors and elminators (induction)
- function application and combinators

The first is not essential, but it is useful to include it to navigate the spectrum from pen-and-paper formalized math to machine-code implementation.

(Lazy thunks are important too, but I will consider them part of the framework for computation, not a member of the types of computation.)

## Functions vs Constructors: Who Would Win?

Functions and datatype constructors have a weird relationship.
They are constantly posturing to make each other redundant.

Why have a fully-featured language with [ADT]{t=}s and case expressions, when it is all equivalent to a combinator calculus – even the minimal SK combinator calculus?

Why not go the other way, defunctionalize everything, make everything data?

Their usage and usefulness depend on context, so, spoiler, they probably are not going to make each other redundant.

### Different computational behavior

With datatypes, we expect `not False`{.haskell} to compute first to an actual constructor `True`{.haskell}, and *then* the case analysis `if (not False) then _ else _`{.haskell} gets to run.
The conception is that the function is a guide for computation that makes decisions based on concrete data it sees, and the concrete data is at most one thunk away.

With pure combinators, we have the definitions `True t f = t`{.haskell} and `False t f = f`{.haskell} for Church-encoding `Bool`{.haskell} as binary functions `forall r. r -> r -> r`{.haskell}, and you know that `(not False) t f`{.haskell} eventually reduces to `t`{.haskell}, but that does not mean that `not False`{.haskell} reduces to `True`{.haskell}.
You can force this reduction with `asBool b = b True False`{.haskell} to compute an expression to a canonical constructor: `asBool (not False) = True`{.haskell}.^[You might expect this to be like forcing a thunk, but it is not really. It only shares work through `asBool b`{.haskell}, not with any other instance of `b`{.haskell}.]

The difference in evaluation is this: with datatypes, you are often paused *inside* a function, waiting for *data*.
(Potentially snaking your way through various `case`{.haskell} elements of the function, if the representation does not split functions apart so they only have one `case`{.haskell}.)
With pure combinators, youʼre always just looking at what function is at the top of the satck and executing it, almost instantly.


### What If: Functions Take Over

Combinators are not functions making decisions based on other data: they *are* the decision, recorded as a function.
Of course, other functions can and do encode decisions.
But combinators are restricted to only doing that; they can only select control flow and route data.

So, do we get rid of constructors?
Probably not.
Constructors are a good place to hang your hat at the end of the day.
They serve other purposes than raw computation.

On the machine side, they have known data representation, known performance.
You do not have to analyze your whole program to know how a data constructor will behave.

**Constructors:** On the type theory side, constructors are the home of _definitional injectivity_.
When the compiler is trying to unify `Just x = Just y`{.haskell}, it _knows_ it can unify `x = y`{.haskell}: there are no other solutions.
(This includes type constructors.)

**Casing:** Definition by cases on a concrete type, `b : Bool`{.haskell} where `data Bool = False | True`{.haskell}, is much easier than on a Church-encoded type `forall r. r -> r -> r`{.haskell}, which requires parametricity to show it only has two inhabitants.
No proof assistant I know of incorporates parametricity in this way.

### What If: Constructors Take Over

Defunctionalization is the process of letting constructors take over.

You still need one procedure to do evaluation, otherwise you are left with a pile of data and nothing to do with it, but every other function is converted to a datatype that is passed around on the stack with closures reified as records.

The downside is that this model of computation is closed.
You defunctionalize a whole program and cannot really extend it.

The versatility of functions is that they represent an open model of computation, easily extensible.

## Continuations & Gotos

Compiling with Continuations has a long history as a functional alternative to A-Normal Form (Administrative Normal Form or Atomic Normal Form (ANF)), the imperative^[disputed] representation of choice.
Continuations are higher-order functions that take arguments and have self-contained behavior, while ANF is generally thought of as first-order imperative blocks with gotos to jump between them.

We have [_Compiling with Continuations_](https://www.cambridge.org/us/universitypress/subjects/computer-science/programming-languages-and-applied-logic/compiling-continuations), [“Compiling with Continuations, Continued”](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf), [“The Essence of Compiling with Continuations”](https://dl.acm.org/doi/10.1145/173262.155113), [“The Logical Essence of Compiling with Continuations”](https://arxiv.org/abs/2304.14752), and so on.

A continuation represents a decision: the choice of continuation and what data to pass to it is what moves program evaluation forward.
Pared down to its core, you get combinator calculi.
In the intermediate phase, you might look at the fact that each `case`{.haskell} expression may be thought of as taking a continuation for each of its branches, but still operating on the concrete data that we know and love.
Or maybe you think of it as pushing the bound variables onto a stack and jumping to a new location in the program code.

:::Key_Idea
The key idea I want to bring to the forefront is this:
whether you use continuations or gotos, the data from an [ADT]{t=} is always __parsed__ into your programʼs instruction counter: briefly reflected there for every piece of data that is unpacked.
The only way around this is to use data that your CPU supports directly: integers and floating point numbers.
Otherwise, a record of your programʼs instruction counter is enough to record what data your program came across and found interesting.
:::



## Glossary

ADT: Algebraic Data Type

:   A data type that is specified as a sum of products: a bunch of constructors (sum type / “tagged union”) that each contain some data fields (product type).
    This abstract representation is enough to specify all forms of pure data, it comes with perfect pattern matching semantics (namely, exhaustivity checking).

    Read more about what makes it _algebraic_ from one of my favorite posts: [The algebra (and calculus!) of algebraic data types](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types).
    (The short answer is that the sum and product behavior can seen by counting inhabitants of data types, but this goes much, much deeper. Study category theory to learn more!)

    ```haskell
    data Bool = False | True

    data Either x y = Left x | Right y

    data Tuple x y = Tuple x y

    data These x y = This x | That y | Both x y

    data List t = Nil | Cons t (List t)

    data Tree t = Leaf t | Branch (Tree t) (Tree t)
    ```

Continuation monad / Continuation Passing Style (CPS)

:   In the monadic bind operator `c0 >>= k`{.haskell}, the left side `c0 :: m i`{.haskell} is a computation in a monad, and the right side `k :: i -> m o`{.haskell} is a _continuation_ that specifies what to do with `i`{.haskell} when/if it is obtained (possibly multiple times).
    The continuation monad `ContT r m a = ContT ((a -> m r) -> m r)`{.haskell} makes the continuation a first-class part of the monad, allowing it to be manipulated and transformed in any way you can think of.

    The continuation monad can be used to model a lot of other monads, it is [“The Mother of all Monads”](https://www.schoolofhaskell.com/user/dpiponi/the-mother-of-all-monads), and this is sometimes helpful for performance reasons too.
    It also is the basis of constructing an asynchronous effect monad from a synchronous effect monad: this is why JavaScript uses callbacks for asynchronous processing.^[Be careful, because `ContT`{.haskell} says nothing about the finer details: it does not enforce call-once callbacks, it says nothing about exception handling, asynchronous cancellation, resource management, scheduling, or a lot of other details.]

Combinator calculus ([e.g.]{t=} SK/SKI calculus)

:   A minimal model of computation where everything is a combinator: a function that, when applied to a number of arguments, returns those arguments applied to each other.
    That is, the right-hand-side of a combinator contains only variables, no lambdas or named combinators.

    The SK calculus is the most minimal of these^[There are other two-combinator bases, but all one-combinator bases have to use improper combinators.]: everything is done with the two combinators `S x y z = xz(yz)`{.haskell} (which I call the sharing combinator) and `K x y = x`{.haskell} (the constant combinator – konstant in German).
    You can build up a stack of these, evaluate them (ideally, left to right, with sharing and laziness), and compute any program.
    Every lambda calculus term can be written in terms of just `S`{.haskell} and `K`{.haskell}!^[There are some details here about beta-equivalence and eta-equivalence differing, but the point is that lambda calculus and combinator calculus have equivalent expressive power.]

Defunctionalization

:   Enumerating some/all functions/closures/procedures and turning them into datatypes, so they can be manipulated as regular data in the program.
    All of the executable code is consolidated into one interpreter procedure (or one procedure per function type) that handles detecting what function to apply and referencing variables in the closure.
