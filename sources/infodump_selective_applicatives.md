---
title: Infodumping About Selective Applicative Functors
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

:::{.centered}
*Originally posted at <https://tech.lgbt/@monoidmusician/112250458187094582>*
:::

tonightʼs infodump on selective applicative functors:

we know about monads, which have dynamic control flow: based on the
result of a previous computation, you can invent a whole new computation
to run in the chain

```purescript
(>>=) :: m x -> (x -> m y) -> m y
```

this is the most flexible you can be, and it necessitates a linear order
of evaluation

applicatives have static control flow: the set of computations to run is
fixed up front, and then you get to combine their results at the end

applicatives can evaluate however they like: sequentially, in parallel
… they can even evaluate right-to-left (like monoids, they can be
flipped via `^op`)

```purescript
(<**>) :: m x -> m (x -> y) -> m y
```

alternatives model nondeterminism: you donʼt write into the program
what branch to choose, but however you interpret the functor has to
decide

this is useful for parsers: the parser will return results from branches
that match, discarding branches that fail

```purescript
(<|>) :: m x -> m x -> m x
```

------------------------------------------------------------------------

selective applicative functors lie in between monads and applicatives in
terms of their power, and show similarities with alternatives too

unfortunately, selective applicatives are tricky to formulate correctly,
but the basic flavor is given by `select`{.purescript} and `branch`{.purescript}:

```purescript
select :: m (Either u v) -> m (u -> v) -> m v
branch :: m (Either x y) -> m (x -> z) -> m (y -> z) -> m z
```

one way to think of it from a high level is that selective applicative
functors let you have *determined* choice:

the computation itself gets to determine what branch to take, by whether
it returns a `Left`{.purescript} or a `Right`{.purescript}

it is no longer nondeterministic like `<|>`{.purescript}

but it also isnʼt completely dynamic like monads: you have to declare
all the branches up front, make some dynamic choices along the way, and
use all the information to come up with a final result

------------------------------------------------------------------------

for several months Iʼve been thinking about selective applicative
functors in the context of parsers specifically

I realized that thereʼs a reason that `<|>`{.purescript} and `branch`{.purescript} feel similar
in terms of both being branching structures of control flow, and the
implications that has for scoping and syntax and such:

selective parsers are just parsers with dynamic failure:
`try :: m x -> (x -> Maybe y) -> m y`{.purescript}, where you get to inspect a result
but choose to make the parser fail anyways

parsers with (finite!) case matching built in, and just the intuitive
notion of scope that that brings

I posted some notes about why this is on cohost:

<https://cohost.org/monoidmusician/post/2588944-more-more-more-seman>

and my notes on selective functors more generally:

<https://cohost.org/monoidmusician/tagged/selective%20applicative%20functor>

and Iʼve been developing a framework for parser combinators that will
hopefully execute selective applicative structures with [LR(1)]{t=} or [GLR]{t=}
tables

(uhhh I havenʼt figured out if it corresponds to a known class of
grammars yet)

------------------------------------------------------------------------

so why is it so tricky to formulate?

```purescript
select :: m (Either u v) -> m (u -> v) -> m v
branch :: m (Either x y) -> m (x -> z) -> m (y -> z) -> m z
```

`select`{.purescript} is weird: `Either`{.purescript} is a tensor, but `->`{.purescript} sure as heck isnʼt.

an alternative formulation goes like

```purescript
m u -> m (Either v (u -> v)) -> m v
```

where you have a “maybe constant function” (`Either v (u -> v)`{.purescript}), but
this still doesnʼt fit into category theory nicely at all

`branch`{.purescript} is even weirder: it looks like it has the right shape, but it
doesnʼt compose!!

the 2-ary `branch`{.purescript} does not easily give rise to 3-ary `branch`{.purescript}, you
basically have to reimplement `select`{.purescript} in terms of `branch`{.purescript} and then
apply it multiple times, it is so annoying

my solution and notes on why this is is here:

<https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Selective.purs>

basically you have to treat it like the control flow it is: break out
the profunctors/arrows, specifically have a profunctor for “finite case
branching on some type”, and then use that as your building block for a
N-ary `branch`{.purescript}

------------------------------------------------------------------------

so the formulation I like says, “follow a computation with a finite
case tree on its result, whose result is the result of the whole
computation”:

```purescript
caseTreeOn :: forall i r. f i -> CaseTree i f r -> f r
```

(`CaseTree`{.purescript} is tricky to define, with existential quantification ... I
think thatʼs part of why nobody has stumbled across it to my knowledge)

these are strong functors, so you can pass down any information you
learned in the first computation into the branches of the second ...
basically how you would expect variable scoping to work (in non-linear
type theories) :)

(yes you can shut up about how monads need to be strong to be
applicative functors: everything is strong here.)

------------------------------------------------------------------------

this is a strengthening of the usual definition of selective functors
(which just use `select`{.purescript}): it encodes *exclusive determined choice*

now, itʼs a bit tricky to answer what this gives you, what any of this
gives you,

but exclusive determined choice is important for parsers, at least: for
the purposes of static analysis, you would like to know that two
branches are exclusive, to know that ambiguities between them will
always be resolved

you simply do not get this knowledge with `select`{.purescript}: if you look at how
to encode `branch`{.purescript} via `select`{.purescript}, youʼll realize that you have to
inspect the control flow of (potentially) arbitrary functions to make
that determination, which is absolutely forbidden

(basically it works by saying that each branch “could” be run, and
shuffling around data to ensure that the right branch gets chosen or
skipped each time … it is really annoying and inelegant)

------------------------------------------------------------------------

now would be a great time to clarify this:

**nothing in the formalism says that branches have to be skipped, and
this is a feature not a bug**

I will repeat

**branches can be executed even if they were not chosen, and this is on
purpose**

in the case of `select`{.purescript}, it means that `select`{.purescript} can be implemented in
terms of `<*>`{.purescript} which always runs both branches! (and just discard the
result of the one that was not necessary)

I will get more into this when I ask “what does this formalism get us,
anyway?”

but for now, observe that the *beauty* (yes, beauty) of selective
applicative functors is that they are more plentiful:

whereas types mostly have one monad instance, and maybe a couple
different applicative instances, they will have a proliferation of
selective instances:

lists have their usual monad structure which gives an applicative
structure (Cartesian product), but they also have a ziplist structure,
and then their flipped/`^op` versions

all these give different selective structures, and more!

------------------------------------------------------------------------

another example: the `Const`{.purescript} functor has no monad structure, but it has
a standard applicative structure given by monoid concatenation (and its
opposite order, which we do not care about)

but as a selective functor we can work some magic: `Under`{.purescript} with only
capture the effect executed unconditionally ([i.e.]{t=} the first arguments of
`select`{.purescript}, ignoring the second arguments) and `Over`{.purescript} will take it all,
regardless of if it would be ignored during runtime (this is the
selective structure given by the applicative)

this is in The Paper, read more there (section 2.2):
<https://dl.acm.org/doi/10.1145/3341694>

but it starts to answer our question: why is this useful?

itʼs useful because you can approximate the effects you are about to
run, in advance: they all exist statically, the control flow *can* be
dynamic, but itʼs tailor made so you can *ignore* the control flow and
know everything that might pop up

------------------------------------------------------------------------

itʼs tempting to impose a law that branches of control flow that are
never taken are always ignored

but there is a huge problem with that: to obey the law, an
implementation either needs to (1) inspect arbitrary functions to
determine if the next computation could possibly run, or (2) actually
execute the one or many possibilities and keep track of the results …
[i.e.]{t=} run/simulate the computation.

both of those are non-starters for static analysis, it simply ruins the
abstraction we are going for

you might ask, “why not formulate static analysis as a separate
abstraction, and leave selective functors for the well-behaved ones?”,

The Paper tells you why!!

through the example of Haxl (a library mostly meant for DB queries)
shows that you want it to live in a single abstraction so that you can
have *static analysis interleaved with dynamic execution*

how cool is that??

basically batches DB queries to get all required data and maybe
conditionally required data, Iʼm fuzzy on the details still

------------------------------------------------------------------------

so yeah, if you let it all live under one abstraction, you get nice
things

like, you can take the product of two selective applicative functors,

so you can just tuple up your over-approximation, your
under-approximation, and your actual execution context into one functor,
simply because they all worked on their own …

… or you can find ways to interleave them like Haxl did

the main problem with Haxl, which is not its fault but rather
Haskellʼs, is that Haskell typeclasses require that instances agree
with each other:

if a type implements Monad, it needs to implement Applicative in a
closely compatible way

and the same choice was made with Selective: must agree with Monad
instance if it exists

I think this is mostly good design, itʼs important that things behave
predictably

itʼs just so tempting to cut corners and implement more interesting
instances for Applicative and Selective, mostly because no language nor
library features make dealing with it easier

------------------------------------------------------------------------

Haxl in particular got in hot water for having an Applicative instance
that didnʼt agree with its Monad instance, since the Applicative wants
to do static analysis and batch everything, while Monad simply does not
allow batching

we should support this somehow! thereʼs a really interesting story to
be told there! we do want to prefer static analysis when possible!

itʼs just hard to express currently (it requires making newtypes for
each behavior you think of), and I really think we need to make this
easier at the language+library level (itʼs something that I want to
address in tmTTmt!)

there is
[`Parallel`{.purescript}](https://pursuit.purescript.org/packages/purescript-parallel/7.0.0/docs/Control.Parallel.Class)
in PureScript, which gives `parTraverse`{.purescript} and other goodies, but idk
Iʼve never enjoyed making new types for it……

------------------------------------------------------------------------

okay so as the final word in what selective applicative functors get us
in terms of static analysis:

my case tree abstraction makes it more clear

the whole deal with regular applicative functors is that their static
analysis is just a monoid, given by the `Const`{.purescript} instance

in fact, I would argue thatʼs all that static analysis is: an
interpretation of the functor `f a -> c`{.purescript} that is lawful as a morphism of
the appropriate structure of type `f a -> Const c a`{.purescript}.

with `caseTreeOn`{.purescript}, we donʼt just have a monoid anymore: we still
combine effects sequentially with a monoid, but we combine the
alternatives with a (potentially differently behaved) semigroup

(we can ignore empty case trees: 0-ary branch just evaluates its first
argument)

thereʼs probably some laws you want to impose between the monoid and
semigroup

in fact, having a lattice or semilattice [is particularly
nice](semilattice_semiring.html)

and semilattices crawl all over static analysis, you wouldnʼt believe
it

(well, maybe you would \^\^)

------------------------------------------------------------------------

and as a final message that should have been a first message for
intuition:

what type of `do`{.purescript} syntax would we give to selective applicatives?

for monads we have regular `do`{.purescript} syntax:

```purescript
do
  x <- f
  y <- g x
  case x + y of
    42 -> return True
    r -> loop r
```

for applicatives we have much stricter `ado`{.purescript} notation, where each
binding only is visible in the final `in`{.purescript} clause (return statement):

```purescript
ado
  x1 <- c1
  x2 <- c2
  in x1 + x2
```

`selective`{.purescript} will allow case branches, where the bound variables are
visible in `case`{.purescript} and `in`{.purescript} but still not the right sides of binds:

```purescript
ado
  x1 <- c1
  x2 <- c2
  case x1 + x2 of
    42 -> pure True
    r -> ado
      -- x1, x2, and r are still not in scope
      x3 <- c3
      -- until `in`
      in x1 + x2 + x3 == 42
```

unfortunately the desugaring will be a lot more complicated: the
compiler essentially needs to pick a strategy for pattern matching and
expose it during desugaring

thereʼs also details in branching in the middle vs at the end, yeah …
