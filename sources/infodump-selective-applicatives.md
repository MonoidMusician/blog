---
title: Infodumping About Selective Applicative Functors
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2024/04/10
toc: false
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
flipped via \({-}^{op}\))

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

I posted some notes about why this is on [cohost](#appendix-chosts),
along my notes on selective functors more generally.

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
skipped each time&nbsp;… it is really annoying and inelegant)

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
and then their flipped/\({-}^{op}\) versions

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
execute the one or many possibilities and keep track of the results&nbsp;…
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
simply because they all worked on their own&nbsp;…

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

thereʼs also details in branching in the middle vs at the end, yeah&nbsp;…

<hr class="full-width" />
# Appendix: Chosts
<hr class="full-width" />

[2023/08/06]{.dated}

thinking a lot about selective applicative functors these days

theyʼre very delicate objects because theyʼre all about “liberties constrain, constraints liberate” and navigating the interplay of producer vs consumer, interface vs implementation

in particular, they make the opposite tradeoff of most contracts: implementations get a lot of freedom, while users of the interface donʼt get to assume as much about behavior of the implementations

this also means that for a given type, there are likely to be many more selective instances you would consider, than monad or applicative instances!

anyways my main things are:

- I want to think about what selective parsers look like (theyʼre not context-free, but their choices are limited – can they be integrated with LR parsing techniques? I think so!)
- I want to make compiler implementations using the language of selective applicatives, in particular with compiler state being a semilattice
- enhanced API to encode exclusive branching, not just optional branching

<hr class="full-width" />

[2023/08/06]{.dated}

background on selective applicative functors:

- applicative functors have an operation `apply :: f (a -> b) -> f a -> f b`{.purescript}
- selective applicatives have an operation `select :: f (Either a b) -> f (a -> b) -> f b`{.purescript}

this allows short-circuiting the computation, skipping the effects of the second operand if the first returns a `Right`{.purescript} value; this is what happens if you implement it in terms of a monadʼs `bind`{.purescript}/`join`{.purescript} in the obvious way

However, it does not mandate skipping effects! In fact, you can implement `select`{.purescript} in terms of `apply`{.purescript} to not skip effects.

the main reason this is done is to allow forms of static analysis that consider all potential cases, thus being an overapproximation of the potential behavior of the computation

(static analysis is impossible for monads, where you can only underapproximate by considering the first action, since the remaining actions can depend completely arbitrarily on the result of the first)

the paper on this has a nice case study with the Haxl functor

link to the paper (ACM open access):

https://dl.acm.org/doi/10.1145/3341694

and the hackage docs:

https://hackage.haskell.org/package/selective-0.7/docs/Control-Selective.html

<hr class="full-width" />

[2023/08/09]{.dated}

`multiSelect`{.purescript} and `<|>`{.purescript} both have similar challenges for control flow

can `multiSelect`{.purescript} be phrased in terms of `<|>`{.purescript} and `select`{.purescript}? idk it kind of seems like a bad idea, outside the land of parsers

youʼd probably have trouble arguing it if you have nontrivial effects on the left side of `select`; it would rely on factoring those out

so it would need to be idempotent …?

_sleepy_

(unrelated thought: I wonder if the order of alternatives could be semantically significant in LR or Earley parsing; can you maintain it consistently in state and use it to disambiguate?)

<hr/>

what type signature would I give `multiSelect`{.purescript} in PureScript?

```
forall f ri rfo o.
  Selective f =>
  … forall l, rfio[l] = f (ri[l] -> o) …
f (Variant ri) -> Record rfio -> f o
```

Itʼs like [`match`{.purescript}](https://pursuit.purescript.org/packages/purescript-variant/8.0.0/docs/Data.Variant#v:match) but each case has a statically known effect `f`{.purescript} around its function (the key part being you can enumerate these ahead of time – although if you care about order, youʼre stuck with string sorting order)

<hr/>

okay letʼs get a Menhir/OCaml-inspired syntax going

_looks up what Menhir looks like again_

```bnf
int:
  | d = digit
    { d }
  | v = int; d = digit
    { 10*v + d }

// a higher order parser
// parses an integer, and then that many of the given parser
// (this makes it context-sensitive)
length_prefixed(parser):
  // I need to verify that passing { amt } makes sense semantically
  // (that seems necessary to have unbounded repetition here)
  | amt = int; r = length_prefixed_aux(parser){ amt }
    { r }

// a higher order parser
// that also takes a value too?
length_prefixed_aux(parser){ amt }
  match amt with
    // if amt = 0, we return an empty list
    | 0 ->
      { [] }
    // otherwise we parse one, and recurse
    // (decrementing amt)
    | (n+1) -> head = parser; more = length_prefixed_aux(parser){ n }
      { head :: more }
```

<hr class="full-width" />

[2023/08/09]{.dated}

okay so I think if you have a *good*, *well-behaved* parsing framework, you donʼt need a `select`{.purescript} primitive, just a [`compact :: f (Maybe a) -> f a`{.purescript}](https://pursuit.purescript.org/packages/purescript-filterable/5.0.0/docs/Data.Compactable#v:compact) primitive (in addition to `Applicative`{.purescript} and [`Plus`{.purescript}](https://pursuit.purescript.org/packages/purescript-control/6.0.0/docs/Control.Plus#t:Plus)), so then

```purescript
select l r =
  compact (blush <$> l) <|> ((#) <$> compact (hush <$> l) <*> r)

hush :: Either a b -> Maybe b
blush :: Either a b -> Maybe a
```

and the syntax in my last post can look like this: no explicit match statement, just pattern matching on a pure value:

```bnf
length_prefixed_aux(parser){ amt }
  // guard on amt = 0, then return an empty list
  | 0 = { amt }
    { [] }
  // otherwise we parse one, and recurse
  // (decrementing amt)
  | (n+1) = { amt }; head = parser; more = length_prefixed_aux(parser){ n }
    { head :: more }
```

---

in fact you might argue that this formulation of `select`{.purescript} is equivalent to something with `compact`{.purescript} on the outside, but again, that requires you to have a good parsing framework

what does that mean?

well the parsing framework should be able to handle backtracking and ambiguity that gets resolved on the fly

and it also is able to cope with losing the notion of what paths are exclusive

maybe in non-backtracking parsers, you can add `try`{.purescript} on the left alternative ... but youʼll still do the work twice, naïvely

(i.e. if youʼre interpreting in a monad and not doing static analysis)

and because of how you have to re-associate applicatives to make `select`{.purescript} work, thatʼs often a significant, multi-step parser

but if you use the static analysis to read ahead or something (Brzozowski derivative, LR parsing, stuff like that), then you can maybe share the work between the branches anyways

<hr class="full-width" />

[2023/08/09]{.dated}

did some more thinking about the laws and what I said above

> in fact you might argue that this formulation of `select`{.purescript} is equivalent to something with `compact`{.purescript} on the outside, but again, that requires you to have a good parsing framework

```purescript
select l r =
  compact (blush <$> l) <|> ((#) <$> compact (hush <$> l) <*> r)
```

there are two laws we need:

---

`compact`{.purescript} + `apply`{.purescript}:
```purescript
compact l <*> r = compact (lift2 (\mf a -> mf <@> a) l r)
```
This law says that `r`{.purescript} must have relatively benign effects in that they must be able to be reverted when the combined computation fails on account of the first failing.

this connects with what I was saying about LR parsers in another post, because there it is about being able to predictively try rules in states, and roll back if they donʼt work out

`compact`{.purescript} + `alt`{.purescript}:
```purescript
(compact l <|> compact r) = compact (l <|> r)
```
This law says that ... uhh ...

no I guess this law doesnʼt make sense for parsers, since if `l`{.purescript} succeeds with `Nothing`{.purescript} it will override whatever `r`{.purescript} would do to recover. (It does make sense for lists, incidentally.)

okay I guess we just need something other than `<|>`{.purescript} in there

this is halfway to what we need:

```purescript
(compact l <|> compact r) = compact (map Just (compact l) <|> r)
```

this makes sense for both lists and parsers, not too insightful

we could also do it the other way
```purescript
(compact l <|> compact r) = compact (l <|> r) <|> compact r
```

this doesnʼt make sense for lists anymore, since it is duplicating `r`{.purescript}! and arguably it would introduce ambiguity for parsers too, if they donʼt have idempotence like `r <|> r = r`{.purescript} (LR parsing gets really unhappy if you have duplication like that)

I donʼt know, I will have to think if there are ways to pull it out so we have ``compact (l `someOp` r)``{.purescript} on the right hand side.

<hr class="full-width" />


it occurred to me that you could go the other way, and encode `<|>`{.purescript} in terms of `select`{.purescript} and `try :: f a -> f (Maybe a)`{.purescript}.

```purescript
alt :: f a -> f a -> f a
alt l r = select (note unit <$> try l) (const <$> r)
```

again, this seems to make sense for well-behaved parsing frameworks, but I havenʼt put too much thought into laws

and again, we see the theme that we want the effects to be reversible, which is now encapsulated in `try`{.purescript}.

however, I think this is the less useful direction to go? it seems hard to make sense of the exclusivity inherent in `alt`{.purescript} in this presentation

<hr class="full-width" />

[2023/08/09]{.dated}

Iʼm pretty sure itʼs possible to interpret this selective applicative parsing within the style of LR parsing

(in fact, it may be the most well-behaved setting)

I wrote a bunch about how LR(1) parsing works:
[Interactive Parser Explanations](parser.html)
(as always, thereʼs a bunch more to write)

quick overview is that it builds a graph of state nodes, where each state is a partial parse of some rules and the lookahead that one would expect after parsing that rule (based on how you got to the state)

each step looks at the current token in the stream, and decides to *shift* the token onto the stack and continue parsing the rules, or *reduce* a finished rule (if you see its lookahead) and get its arguments off of the stack

conflicts occur when there are multiple actions that one could take from a state on a token: shift/reduce conflicts or reduce/reduce conflicts

Iʼm pretty sure that the only modification this needs to account for selective parsing is that the actions needed to be pruned based on the logic written into the `select`s

but thereʼs some technical issues that come up (and functional programming to the rescue!)

the first is that there are multiple rules for each state: each state is juggling several rules (sometimes different parts of the same rule!), and one only whens when the others are filtered out by shifting tokens that they donʼt have, or when a reduction happens and the lookahead unambiguously points to one rule

the second issue is related: the rules in a state are all hypothetical, they arenʼt finalized until the reduce happens and the information is popped off of the stack

so if there are applicative and selective operations happening, with their own local variables and logic, we need to be able to simulate that for each rule independently

this would happen by peeking at the stack, running the logic for the partial parses based on the stack, and using it to prune the actions to take to advance the parser

so we really need the logic to be pure! we need to be able to keep a firm grasp on whatever global and local state is floating around, so we can try it out and roll it back and memoize it

<hr class="full-width" />

[2023/08/22]{.dated}

what if you were able to match “any token that doesnʼt already have specific meaning in this context”? wouldnʼt that be cool?

it particular, it would make throwing together parsers for semi-structured data much easier (think parsing logs or other ill-specified formats)

this should definitely be possible for any parser with static analyzability

it gets a little weird with lookahead like LR(1), but I donʼt think thatʼs a technical challenge, just a conceptual one

I havenʼt seen this done in the wild, have you?

---

you could even lean into this more, and have a more elaborate system to match tokens, with multiple levels of specificity to resolve any ambiguities that may arise, like CSS selectors

(itʼs very analogous to the problem of CSS selectors, actually)

you need some structure on token selectors anyways: if youʼre building an LR(1) parsing table, you definitely want to know ahead of time which token selectors are disjoint

---

basically there are three sensible ways of disambiguating parses:
1. order (regexes tend to do this, preferring leftmost parse; CSS does this of course)
2. precedence operators, which specify the precedence and associativity of individual tokens (very common in LR parser generators)
3. specificity (what I proposed above; also seen in CSS; this subsumes 2.)

<hr class="full-width" />

[2023/09/06]{.dated}

![Screenshot of two panes of code. The left has the grammar and examples. The right has the PureScript code that generated the grammar.  The grammar parses a number. A number is either a digit or a number followed by a digit. The digits are listed out. Below are examples showing that the empty string fails to parse, but number strings parse as expected.  The PureScript code is standard parser combinator style.](assets/images/LRGenerator.png){style="width: 100%"}

I made a fusion of a LR(1) parser generator and a parser combinator library!

(well, itʼs not a library yet, just a module embedded into my blog framework)

Itʼs an applicative combinator functor that generates a context free grammar, builds an LR(1) parse table for that grammar, and then parses it in two phases: first it uses the table to parse a string into a concrete syntax tree, and then a codec takes the concrete syntax tree and parses it into the result of the functor.

By sharing the function `parseNumber`{.purescript}, the LR(1) table is only calculated once and used for each parse. One of my goals is to statically generate it at compile time and bake it into the JavaScript.

Itʼs not a free applicative (which would be another sensible approach, but not necessary here). It just builds up the rules, the grammar, and the codecs directly, and glues it all together later.

Iʼm impressed that I was able to throw this together in a few hours after waking up at 3am ...

edit: oh I forgot to link to it, I blame the sleep deprivation: https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Comb.purs

<hr class="full-width" />

[2023/09/09]{.dated}

selective applicative parsing is harder than I thought :(

I split my parser into two steps, the grammar step and the `CST -> result`{.purescript} step, right?

The `CST -> result`{.purescript} step can fail, both as a general rule (if you have an internal error in your parsing framework that makes the CST not the right shape), and as part of the `select`{.purescript} operation for limited context sensitivity.

I was naïvely thinking that LR parsing is great: once you commit to a production rule that should be all you need to know.

Except thatʼs not quite right: you know what production rule you just reduced, but you donʼt know *where in the CST it belongs*, because the nodes above it havenʼt been figured out yet, just itself and the nodes before it.

---

So if you want to use the limited context sensitivity to disambiguate parses by preventing reductions, itʼs suddenly not so simple: you would have to apply all possible functions that might target that type of node in the tree, based on the current context.

The easy but expensive way to do this would be to hand it off to the LR(1) framework to figure out: generate extra nonterminals for each distinct function applied to a parser, and hope you can figure it out in time for reductions. But I donʼt think this quite works out: youʼd need some compromise solution when you know the same grammar rule is being reduced but with potential different functions.

The proper approach probably means keeping the grammar, but tagging the functions with extra metadata and somehow propagating it through the LR(1) states properly. (Perhaaaps it is just a semigroup operation – I will need to explore.) Then when youʼre faced with a conflict, you can try all the possible functions and hopefully rule an action out if all functions fail.

It sounds really difficult, Iʼll let you know if I figure out a good solution ...

I suppose the other hacky solution would be to embrace the CST and try _every_ function on reductions, since most of them will fail (due to the wrong shape).

<hr class="full-width" />

[2023/09/17]{.dated}

Iʼve made really good progress on my LR(1) parser combinators.

The next step (which Iʼm feeling a little stuck on, mostly because I need more energy to actually figure it out and I do not have energy), is implementing the selective applicative functor bit.

Basically it allows for a finite amount of context sensitivity: as it is parsing, it gives the userʼs logic (formed from the parser combinators) the ability to reject parses during parsing.

Thereʼs one big problem that LR parsing is nondeterministic from the perspective of the combinators, so you donʼt know what combinator to apply to the rules at first. So it could be a little inaccurate.

For cases where there isnʼt ambiguity (shift/reduce or reduce/reduce conflicts), it doesnʼt make much sense to keep trying to refute the matches. Just let the LR algorithm do its thing.

Itʼs pretty common to write LR grammars with tons of ambiguity and resolve them with token precedence – itʼs just a way to prioritize shift/reduce and reduce/reduce conflicts. I think this is called precedence operators, idk why. (Not operator precedence.)

But if you have that going on *and* selective parsing that allows you to reject cases, you must apply the selective rejection *first*, and then precedence operators.

This whole project has been an interesting adventure in different layers of logic and such like this: how they communicate and dialogue.

Anyways, if youʼre bored and want to follow along in the code:

https://github.com/MonoidMusician/blog/tree/main/PureScript/src/Parser/Comb

<hr class="full-width" />

[2023/09/17]{.dated}

okay, so what I need is a stack of associative lists from state items to acceptance/rejectance functions, and update it as necessary.

each state item can have multiple acceptance functions associated with it, representing different paths through the combinator tree to arrive at the same grammar rule.

this is basically why it canʼt be baked into the state table (although it could be baked into a separate expanded state table maybe? to avoid the expensive operations ...)

Iʼll probably implement it the slow way first, and then find some way to sneak it in as additional data on the LR table itself (using monoids of course xD)

it should be pretty obvious how to take the top-level rule (always accept, parse the main nonterminal followed by EOF), compute its closure (add the rules for the main nonterminal, and their acceptance functions for context sensitivity), and then advance through it (drop rules that donʼt mention the new thing, then push it through the remaining rules, and compute closure again).

whenever thereʼs ambiguity, the parser will stop, apply all of the acceptance functions to reject rules (both shifting and reducing), then resolve remaining ambiguity with precedence operators, then prefer shifts, then arbitrarily choose a reduction/give up

or I could add GLR ...

I guess itʼs hard to compile context sensitivity into a table per se, because acceptance functions can reject and uhh that seems nasty to keep track of (exponential blow up?)

but I guess you could probably get away with an actual map from state items to acceptance functions, since uhh they have a boolean algebra and True is a distinguished element and the algebra handles filtering and uhhhh _sleepy_

<hr class="full-width" />

[2023/09/24]{.dated}

uhh once again my bodymind refuses to let me actually code, so let me dump some thoughts about the next stage of parser implementation

the fun stuff (limited context sensitivity)

---

background:

so thereʼs the applicative structure, and then we let a parser be able to fail based on its result

`(<*>) :: f (a -> b) -> f a -> f b`{.purescript} and `compact :: f (Maybe a) -> f a`{.purescript}

so if a section of a parser can fail, we have to go through and actually compute whether it will return Just or Nothing

but most parts of a parser wonʼt fail in this way, so we need to be smarter about how we encode it

(Iʼm in a weird bind where I refuse to let myself do the obvious but very inefficient routes)

---

we need to pull out the algebra of failure specifically, aside from computing the actual result of the parser

basically this creates segments where we need to apply the applicative computation,

these segments can overlap, in the case of `compact (compact x <*> compact y)`{.purescript}, which can fail if the first or the second fail, or the whole combined parser fails

---

basically each piece of logic, `CST -> Maybe Failure`{.purescript}, needs to be tagged by referential identity, to avoid recomputing it a lot due to nondeterminism of the parser,

so the ingredients are:

- each rule gets potentially several deduplicated pieces of logic attached to subfragments, with [start,end] indices
- we need a way to advance it during the matching process; for now it will be an array of CST bits to feed in, but later it will actual be memoized in some way

so I guess this means it needs to be

```purescript
newtype Options = Array
  { pName :: nt
  , rName :: r
  , rule :: Fragment (nt /\ Options) cat
  , logicParts :: EqSet
    { start :: Int
    , end :: Int
    -- RefEq
    , logic :: Array (CST nt r o) -> Boolean
    }
  , advanced :: Array (CST nt r o)
  }
```

and then thereʼs rules for how to advance the state, which I have mostly written out already, and for how to check whether it is failed, and so on

- if any `logicPart`{.purescript} fails, that `Option`{.purescript} is filtered out
- no options for a rule means that it cannot succeed
- if any `logicPart`{.purescript} succeeds when `length advanced >= end`{.purescript}, it is filtered out of `logicParts`{.purescript} (since it will always succeed)
- an option with no `logicParts`{.purescript} for a rule means that it will always succeed
  - so other options for the same `(rule, advanced)`{.purescript} can be dropped

<hr class="full-width" />

[2023/09/28]{.dated}

^ okay I fixed a few things, and I tried to make it a bit more powerful (by chaining a bunch of reductions together), but it still doesnʼt resolve the ambiguity of Polish notation

Iʼll probably have to go to a GLR(1) approach, which augments LR(1) by keeping track of multiple states at once, to nondeterministically evaluate any ambiguity along the way

(thereʼs a compression scheme for these states, to make them into a DAG, but I donʼt know if that is compatible with the extra data I am keeping around ...... hmm)

<hr class="full-width" />

[2023/10/21]{.dated}

Iʼve been trying to come up with a story for how to encode exclusive choice via tensors. I made some great progress now:

https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Selective.purs

The backstory is that the obvious kinds of tensor-y looking things for `select`{.purescript} fail to be tensors in any meaningful way, they are so strongly biased towards the left that you cannot reassociate them without losing information, much less make them symmetric:

https://old.reddit.com/r/haskell/comments/axje88/selective_applicative_functors/ehzi6zy/

And this relates to the fact that `select`{.purescript} does not allow you to encode exclusive choice: you can introduce a `branch`{.purescript} primitive that lets you choose between two branches (exclusively), but it does not stack to let you preserve the exclusivity between three or more branches.

---

The goal is for some kinds of static analysis to be able to retain the information of which branches are exclusive. For parsers, this can tell you if there are parser conflicts. (If youʼre actually executing the computation, you probably donʼt care about exclusivity.)

Static analysis via monoids (my specialty :3) would look like this:

```purescript
aggregate ::
  -- One monoid for sequencing effects
  Monoid m =>
  -- One semigroup for exclusive branches
  Semigroup n =>
  -- We need to be able to coerce between them
  Coercible m n =>
  -- Handling one computation
  (forall a. f a -> m) ->
  -- Means we can handle a whole tree
  FreeSelective f r -> m

aggregate summarize
  (Branch f1 f2 f3) =
    (summarize f1) <>
      (exclusive (summarize f2) (summarize f3))
  where
  exclusive :: m -> m -> m
  exclusive = coerce ((<>) :: n -> n -> n)
```

In fact, you probably want some sort of semiring or lattice, maybe without one of the identities. For example, tropical semiring, with maximum and addition, is great for tallying up how many times a thing is guaranteed to be called across branches of a program.

<hr class="full-width" />

[2023/10/22]{.dated}

Okay, I figured this out to my satisfaction!

https://github.com/MonoidMusician/blog/blob/8ea8bb08fc3bab3d04c0c21fbe1758e109dd161b/PureScript/src/Parser/Selective.purs#L97-L112

This feels like where it was always meant to be headed. A little type that encodes a tree of cases, with computations to handle each case like `f (i -> r)`{.purescript}, and ways to break the original input down into these cases along the way.

```purescript
data CaseTree i f r
  = ZeroCases (i -> Void)
  | OneCase (f (i -> r))
  | TwoCases (SplitCases i f r)

-- `SplitCases i f r` is an existential of `CasesSplit a b i f r` over `a` and `b`
foreign import data SplitCases :: Type -> (Type -> Type) -> Type -> Type

-- Note that even though we are inserting an `fmap` here, we still know that
-- the left and right cases are exclusive, because of how the types line up!
-- The key is that we do not have to (and are not able to) map the output `r`
-- of each computation and feed it into another. It all is branched from the
-- input type and is up to the consumer to handle \\~somehow\\~.
data CasesSplit a b i f r
  = CasesSplit (i -> Either a b) (CaseTree a f r) (CaseTree b f r)
```

As a Haskell GADT if you prefer:

```haskell
data CaseTree i f r where
  ZeroCases ::
    (i -> Void) ->
    CaseTree i f r
  OneCase ::
    f (i -> r) ->
    CaseTree i f r
  TwoCases ::
    (i -> Either a b) ->
    CaseTree a f r ->
    CaseTree b f r ->
    CaseTree i f r
```

---

We can use this to provide a free structure (I have not done this yet), and we can also use it to provide a typeclass method that encodes this exclusive branching:

```purescript
-- Might as well make this a thing. Not sure if it can be usefully split into
-- separate methods?
class Casing f where
  caseTreeOn :: forall i r. f i -> CaseTree i f r -> f r
```

Again: the real advantage here over `select`{.purescript} is that we get to know which branches are exclusive from each other, if desired. (You can always weaken it and discard that information if you want, by interpreting it through left-nested `select`s.)

You can analyze it with a monoid, as I promised. (And this will be a different monoid than you use for analyzing `apply`{.purescript}.)

You can build up cases from smaller cases:

```purescript
twoCases ::
  forall a b i f r.
    (i -> Either a b) ->
    CaseTree a f r ->
    CaseTree b f r ->
    CaseTree i f r
twoCases f a b = TwoCases $ splitCases $ CasesSplit f a b
```

And finally, if you have gotten ahold of an input, you can ask to apply it to the appropriate case:

```purescript
-- One thing you can do is apply a `CaseTree` to a specific value of `i` to see
-- what branch it chooses.
applyCaseTree :: forall i f r. Functor f => CaseTree i f r -> i -> f r
applyCaseTree (ZeroCases toVoid) i = absurd (toVoid i)
applyCaseTree (OneCase fir) i = fir <@> i
applyCaseTree (TwoCases xy) ij =
  casesSplit xy \(CasesSplit f x y) ->
    case f ij of
      Left i -> applyCaseTree x i
      Right j -> applyCaseTree y j
```

(This is how you can weaken it back to `Monad`{.purescript} if you want.)

<hr class="full-width" />

[2023/11/28]{.dated}

you really do need selective rejection (and not just selective determined choice) for parsers

imagine macros that can be different categories, like expression versus statement

`$myStmt(…)`{.bnf} and `$myExpr(…)`{.bnf}

they need to be embedded in very different parts of the grammar, so you canʼt really selective choice your way into parsing the right one based on the name, you have to parse both and hope that they donʼt conflict, you canʼt necessarily statically analyze it

<hr class="full-width" />

[2023/11/29]{.dated}

to expand on this:

letʼs say I have three syntactic categories: lvalues, expressions, and statements

```bnf
lvalue ::= name | lvalue "." name
expression ::= name | expression "." name | name "(" expression ")"
statement ::= expression | lvalue "=" expression
```

now macros want to be typed by input and output category

```bnf
$rename : lvalue -> lvalue
$defer : expression -> expression
$increment : lvalue -> statement
```

we can handle input categories pretty easily:

```purescript
parseMacro = do
  token "$"
  macroName <- parseName
  case lookupInputCategory macroName of
    LValue -> parseLValue
    Expr -> parseExpr
    Stmt -> parseStmt
```

(this is a selective branching, Iʼm just writing it in `do`{.purescript} notation because Iʼm lazy)

we know going in what category to parse for the input to the macro, based on the input type given by the macro name

but when weʼre parsing a statement and it looks to be a macro, we need to assert that it is a macro whose output type is statement, and otherwise fail the parser:

```purescript
parseStatement = options
  [ ... normal statements ...
  , do
      token "$"
      macroName <- parseName
      case lookupOutputCategory macroName of
        Stmt -> continue parsing macro ...
        _ -> fail -- back out of parse
  ]
```

and expression and lvalue get similar treatment

which means they all parse the same syntax! itʼs ambiguous!

except itʼs never actually ambiguous: only one will be accepted, based upon `lookupOutputCategory`{.purescript}

but you canʼt just peek into the logic and know that

the logic is very opaque to static analysis; you would need some metalanguage to analyze the logic

(haha funny that Iʼm designing a metalanguage ...)
