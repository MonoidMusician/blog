---
title: MonoidMusicianʼs Blog
author: "[@MonoidMusician](https://github.com/MonoidMusician)"
---

I believe knowledge should be free & accessible.
When I have the energy and time, I will contribute to putting more knowledge out there and sharing topics I am passionate about.

This blog will be more focused on programming and mathematics, while my more personal stuff will probably continue to live on my personal website: [https://monoidmusician.github.io/](https://monoidmusician.github.io/).

I strive to be an open book, with minimal ego involved, just insight and connection.
However, I am losing patience for putting energy into ephemeral instruction, so if you want to learn something serious from me directly, maybe we can collaborate on a short blog post instead of exchanging messages on social media where it will get lost quickly.

I would also like to share bits of how I customize my software experiences and navigate the tech world.
If technology isnʼt working for the people, what is it doing?
(Lining the pockets of capitalists and actively oppressing the working class, apparently. Sigh…)

Get the behind-the-scenes peek at the [code](https://github.com/MonoidMusician/blog) and my account of the [technologies](technology.html) that go into this blog.
Itʼs not a real blog though since it doesnʼt have RSS yet …

## Posts
- [The Best Errors for Solving Dependency Versions](version_solver.html)

  > A [novel algorithm](https://github.com/purescript/registry-dev/blob/master/lib/src/Solver.purs) for resolving dependency bounds to solved versions:
  >
  > - Incorporates [transitive dependency bounds](version_solver.html#intuitive-foundations-quasi-transitive-dependencies) for a breadth-first search:
  >   1. What dependencies are required no matter which package version in the range we commit to?
  >   2. Whatʼs the loosest bound for each dependency then?
  > - By taking this intuitive approach, we gain two things:
  >   1. [Better errors](version_solver.html#errors), matching what users would expect.
  >   2. Efficiency too, if you could believe it.
  > - Implemented using semilattices ([monoids](version_solver.html#monoids-monoids-everywhere)).

- [Impossible Bézier Calligraphy: Approximating cubic nibs drawn along cubic strokes](bezier_calligraphy.html)

  > Given a pen nib of some shape, what composite shape is produced when that pen is drawn along any particular path?
  > If the inputs are cubic Bézier curves, is the output as well?
  >
  > The catch?
  > Itʼs mathematically impossible to model the output using cubic Bézier curves, as I determined after a bit of calculus.
  > However, that doesnʼt prevent us from getting pretty darn close.
  > Let me show you how it works out.

- [Interactive Parser Explanations](parser.html)

  > I have been building this framework for explaining, analyzing, and teaching about LR(1) grammars for a couple months now.
  > The interactive widgets here will allow you to build and verify your intuition by clicking through examples, because I believe that once you are armed with the basic ideas and the right intuition, you can figure out the rest of details for yourself.
  > Alternatively, it can serve as a playground to test out hypotheses about grammars, see exactly where things go wrong when conflicts occur, and what to do to fix those errors.

- [Eudoxus Real Numbers as Slopes of Pixelated Graphs](Eudoxus.html)

  > This is a post on constructing real numbers without constructing rational numbers. Along the way the rationals will sort of be constructed, or at least heavily implied, but they donʼt directly figure into the definition! Instead all we need is functions from integers to integers.
  >
  > The construction weʼll be talking about is my favorite esoteric construction of real numbers: the _[Eudoxus real numbers](https://ncatlab.org/nlab/show/Eudoxus+real+number)_.

<!-- ## Series
- [Interactive Parser Explanations](parser.html)
  1. TODO: [Parsing By Example](parser_by_example.html)
  1. TODO: [Terminology Reference](parser_terminology.html)
  1. WIP: [Basics: What Are Grammars](parser_basics.html)
  1. WIP: [Uses of Grammars](parser_applications.html)
  1. WIP: [Basics of LR(1) Parsing](parser_lr1.html) -->

## Ideas/WIP
I post these for a few reasons.
I donʼt really have the energy to write these things properly (and I am ever so ambitious in my goals).
But if you want to nudge me to write on them – or even better, if you want to contribute in some way, even just chatting or pairing on it – Iʼd be willing to prioritize them.

Admittedly the “finished” posts are only 80–90% complete themselves, _shhh…_

- Type Theory
  - Interactive type theory!
  - WIP: [User Operators with Implicits & Overloads](implicit_arguments.html), in bidirectional type checkers
  - The Algebra of Type Unification (semilattices everywhere!)
  - [TMTTMT](https://cohost.org/monoidmusician/tagged/tmttmt)
- Programming Linguistics:
  - Iʼve been thinking a lot about [selective applicative functors](https://dl.acm.org/doi/10.1145/3341694) lately.
    Mostly through the perspective of two contrasting applications: [selective applicative parsers](https://cohost.org/monoidmusician/post/2588944-more-more-more-seman), and functors for [typechecking with better errors](comprehensive_errors.html).
  - [CSS selectors as Boolean Algebra](https://github.com/MonoidMusician/purescript-free-boolean/blob/slides/scanned/Slides.pdf) (warning: 35MB PDF)

    The fun part of this work was how to interleave nested selectors.

    My conclusion back then () was that fully implementing `:not()`{.css} had edge cases that were not possible.
    But it was written before the more advanced selectors like `:has()`{.css} became standard.

    Revisit this code and probably implement a CSS selector parser just to make it interactive.
    Or maybe Iʼll co-write a parser (ask readers to write it and offer to put it up on my blog if they do).
- Paradigms:
  - I should do a blog post on what “effects” mean in FP culture

    tl;dr is “effects are as effects do”

    likeeee the real important point is itʼs all about what you leave implicit in your notation

    itʼs about notation not semantics

    in particular, this point about notation is necessary to explain why effects encompasses “pure effects” (like `List`{.haskell}, `Either`{.haskell}, `Maybe`{.haskell}, which are just data structures) and “real side effects” (like `IO`{.haskell}/`Aff`{.haskell}/`Effect`{.haskell}, of the “launch missiles” variety)

    I suppose I need to pretend to recite the history of monads and do notation in Haskell (“programmable semicolon”)

    and explaining why lists model “nondeterminism” could be a whole post of its own …
  - I should also do a blog post on “what people get wrong about the lens/prism/optics laws”, thatʼll be simpler (if more tricky)

    the main problem is that we donʼt have a good way to talk about them without invoking a metatheory of Haskell, with unification and possibly parametricity (Iʼll need to see if that comes up)

    actually, maybe parametricity is “just” the answer, and we can extract a free law from the stated laws!
  - WIP: [Why you should believe in HoTT Path Induction as a (Haskell) Programmer](programming_paths.html)
  - Passive stability:

    > Imperative code has no passive stability: all global state is mutable, and small perturbations (say, modifying a prototype) can cause unpredictably large effects down the line, if not carefully managed. Untyped code is especially bad. Carefully managed imperative code may be stable, but this is active stability: it is not inherent to the framework but imposed on top.
    >
    > Functional code on the other hand is not only passively stable, it is anchored down solid. Each building block is given a static, local denotation, and they compose together. You may still need nonlocal knowledge of the code to understand the intent of what everything represents (ahem, [boolean blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)), but the local meaning is denotationally clear.
  - log levels considered harmful
- Data:
  - The Anatomy of ADTs
  - WIP: [Subtypes/Quotients: Lies Told in Defense of the Truth](adt_lies_for_truth.html)
  - Quotients: Lets us have nice cake and eat things too.
  - Hereditarily Finite Sets
  - I should do a blog post on why [pickling](https://docs.python.org/3/library/pickle.html#what-can-be-pickled-and-unpickled) (in the sense of Python) is so cool and important, and what “data” means in general (to Pythoners, to JavaScripters, to Haskellers, to Agdaers, …)

    pickling is cool because it forces you to acknowledge that—

    _oh! spaghetti code is spaghetti because its data is spaghetti!_

    —that your ephemeral mutable data that only exists at runtime and is guarded behind weird constructors, actually has existence (and therefore meaning) in a persistent manner, that can be locked away and reconstituted into an equivalent but not identical runtime representation

    and it forces you to declare what parts of your API are foreign mumbo jumbo
- Mathematics
  - [Lebesgue measure](https://en.wikipedia.org/wiki/Lebesgue_measure) and [Lebesgue integration](https://en.wikipedia.org/wiki/Lebesgue_integration)

    My two observations:

    1. I love how in order to prove how the old-fashioned Riemann integral works (characterizing what functions are Riemann integrable), you essentially have to come up with the Lebesgue measure, which paves the way for the Lebesgue integral.
    2. It is incredibly cool and incredibly counterintuitive how turning integration on its *side* produces better results.
- Typeclasses:
  - Fundeps: how they affect defining instances and instance resolution
  - Instances as biïmplications, and the unfortunate consequences
    (It is really hard to actually come up for a use for this, outside of specific reflection/reification contexts ... harrumph maybe it actually breaks that too.)
  - Coercible redesign
