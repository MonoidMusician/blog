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

## Posts <a class="icon iconoir" href="rss.xml"><?xml version="1.0" encoding="UTF-8"?><svg width="24px" height="24px" stroke-width="1.5" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg" color="currentColor"><path d="M12 19C12 14.8 9.2 12 5 12" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path><path d="M19 19C19 10.6 13.4 5 5 5" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path><path d="M5 19.01L5.01 18.9989" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></a>

- [Design your programming languages right!](design_it_right.html) [2024/12/16]{.dated}

- [Self-names (paths): The other hardest problem in programming?](selfnames.html) [2024/12/16]{.dated}

- [Infodumping About Selective Applicative Functors](infodump_selective_applicatives.html) [2024/04/10]{.dated}

  > selective applicative functors lie in between monads and applicatives in
  > terms of their power, and show similarities with alternatives too
  >
  > unfortunately, selective applicatives are tricky to formulate correctly,
  > but the basic flavor is given by `select`{.purescript} and `branch`{.purescript}:
  >
  > ```purescript
  > select :: m (Either u v) -> m (u -> v) -> m v
  > branch :: m (Either x y) -> m (x -> z) -> m (y -> z) -> m z
  > ```
  >
  > one way to think of it from a high level is that selective applicative
  > functors let you have *determined* choice:
  >
  > the computation itself gets to determine what branch to take, by whether
  > it returns a `Left`{.purescript} or a `Right`{.purescript}


- [Pickling Tasty Data: The essence of runtime data (itʼs a graph!)](pickling.html) [2024/01/28]{.dated}

  > I want to talk about data today.
  > In particular, I want to talk about runtime representations of data, **real** data – data that can be mutable and referentially opaque at runtime – and demystify what they actually are in terms of more familiar notions of data.
  >
  > You shouldnʼt just throw up your hands once you have cyclic references!
  > Itʼs possible and worthwhile to design tools to work with the raw graph of runtime data, no matter its shape.
  >
  > With the proper metaprogramming hooks, you could save and restore a whole runtime environment *from the inside*, which is pretty amazing to think about.
  >
  > The main thing we will work up to (and go beyond) is Pythonʼs [`pickle`{.python} module](https://docs.python.org/3/library/pickle.html), including my own implementation of it for the [Nasal scripting language](https://wiki.flightgear.org/Nasal_scripting_language) (which I believe is cleaner in some respects, although obviously less industrial).

- [A Semiring From Any Semilattice: A mathematical pun?](semilattice_semiring.html) [2023/10/28]{.dated}

  > You can make a semiring out of a semilattice by adjoining a new zero element. Lifting the semilattice operation in the two obvious ways gives you `+`{.haskell} and `*`{.haskell}. Idempotence gives distributivity(!).
  >
  > This construction answers the question, “if you need a semiring for static analysis, how do you also keep other data around that does not care about the branching structure?” (like, say, a monoid).

- [TransMorphism Type Theory MetaTheory](tmttmt.html) [2023/10/13 – …]{.dated}

- [The Best Errors for Solving Dependency Versions](version_solver.html) [2023/01/02 – 2023/01/21]{.dated}

  > A [novel algorithm](https://github.com/purescript/registry-dev/blob/master/lib/src/Solver.purs) for resolving dependency bounds to solved versions:
  >
  > - Incorporates [transitive dependency bounds](version_solver.html#intuitive-foundations-quasi-transitive-dependencies) for a breadth-first search:
  >   #. What dependencies are required no matter which package version in the range we commit to?
  >   2. Whatʼs the loosest bound for each dependency then?
  > - By taking this intuitive approach, we gain two things:
  >   #. [Better errors](version_solver.html#errors), matching what users would expect.
  >   2. Efficiency too, if you could believe it.
  > - Implemented using semilattices ([monoids](version_solver.html#monoids-monoids-everywhere)).

- [Impossible Bézier Calligraphy: Approximating cubic nibs drawn along cubic strokes](bezier_calligraphy.html) [2022/09/18 – 2023/04/23]{.dated}

  > Given a pen nib of some shape, what composite shape is produced when that pen is drawn along any particular path?
  > If the inputs are cubic Bézier curves, is the output as well?
  >
  > The catch?
  > Itʼs mathematically impossible to model the output using cubic Bézier curves, as I determined after a bit of calculus.
  > However, that doesnʼt prevent us from getting pretty darn close.
  > Let me show you how it works out.

- [Interactive Parser Explanations](parser.html) [2022/07/20]{.dated}

  > I have been building this framework for explaining, analyzing, and teaching about LR(1) grammars for a couple months now.
  > The interactive widgets here will allow you to build and verify your intuition by clicking through examples, because I believe that once you are armed with the basic ideas and the right intuition, you can figure out the rest of details for yourself.
  > Alternatively, it can serve as a playground to test out hypotheses about grammars, see exactly where things go wrong when conflicts occur, and what to do to fix those errors.

- [Eudoxus Real Numbers as Slopes of Pixelated Graphs](Eudoxus.html) [2022/05/10 – 2022/07/20]{.dated}

  > This is a post on constructing real numbers without constructing rational numbers. Along the way the rationals will sort of be constructed, or at least heavily implied, but they donʼt directly figure into the definition! Instead all we need is functions from integers to integers.
  >
  > The construction weʼll be talking about is my favorite esoteric construction of real numbers: the _[Eudoxus real numbers](https://ncatlab.org/nlab/show/Eudoxus+real+number)_.

## Series
- Tools
  #. [WIP]{t=}: [Unicode Explorer](unicode.html)
  #. [tiny tool to reformat output produced by `show`{.haskell}](show.html)
  #. Abandoned?: [Generative Art Scratchpad](genart.html)
  #. [TODO]{t=}: Aspect ratio calculator/database
  #.
    [TODO]{t=}: macOS version lookupper
    - This is my philosophy of just make tools to do the things you need … wow thatʼs not a good way to articulate it sorry.
  #. [TODO]{t=}: magic constants (in hexadecimal and base64) for file hacking
  #.
    [TODO]{t=}: Unit/base calculator and such ([cf.]{t=} [insect](https://github.com/sharkdp/insect) and [numbat](https://github.com/sharkdp/numbat))
    - Would be cool to have better precision than `Number`{.purescript}, or even use [Towards an API for the Real Numbers](https://dl.acm.org/doi/pdf/10.1145/3385412.3386037).
  #. [TODO]{t=}: QR code generator and reader and formats and suchs, and bar code, and PDF417, ...
  #. CSS → Sass
- [Interactive Parser Explanations](parser.html)
  #. [TODO]{t=}: [Parsing By Example](parser_by_example.html)
  #. [TODO]{t=}: [Terminology Reference](parser_terminology.html)
  #. [WIP]{t=}: [Basics: What Are Grammars](parser_basics.html)
  #. [WIP]{t=}: [Uses of Grammars](parser_applications.html)
  #. [WIP]{t=}: [Basics of LR(1) Parsing](parser_lr1.html)

### Miniseries
- [Monoids in Public](monoids_in_public.html) – cool little monoids and uses for monoids [2023/01/02 – 2024/09/18]{.dated}
- [Knowlish](knowlish.html) – A list of little things I have learned and need reminders about.
- [For the Users](for_the_users.html) – My user scripts and user styles (& othersʼ!)
<!--
- [What Means](what_means.html) – Some colloquial definitions of words/concepts
- Co-search – nothing yet, but the idea is that I will put up requests for information, and maybe passers-by can tell me what I seek
- Donʼt make me invent it – in which I finally ask for help & references for prior art, instead of just plunging in confident that I can figure it out
-->

## Ideas/[WIP]{t=}
I post these for a few reasons.
I donʼt really have the energy to write these things properly (and I am ever so ambitious in my goals).
But if you want to nudge me to write on them – or even better, if you want to contribute in some way, even just chatting or pairing on it – Iʼd be willing to prioritize them.

Admittedly the “finished” posts are only 80–90% complete themselves, _shhh…_

### Type Theory

  - Interactive type theory!
  - [WIP]{t=}: [User Operators with Implicits & Overloads](implicit_arguments.html), in bidirectional type checkers
  - The Algebra of Type Unification (semilattices everywhere!)
  - [TransMorphism Type Theory MetaTheory](tmttmt.html)
  - Nontermination and inconsistency
  - [Source spans/provenance](https://gist.github.com/MonoidMusician/2e2b6bcbd60c056083a720921589ec8d) [2020/09/26 – 2020/09/30]{.dated}
  - [Type theory introduction?](https://gist.github.com/MonoidMusician/c42361964ece15c9a883c005282c9614) [2019/09/04 – 2020/07/24]{.dated}
    - [0](tt0.html) [1](tt1.html) [2](tt2.html) [3](tt3.html) [4](tt4.html) [5](tt5.html) [6](tt6.html) [7](tt7.html) [8](tt8.html)
  - [Trying extra reduction rules for induction (notes)](extra_induction.html) [2021/04/03 – 2021/04/10]{.dated}
  - Idea for a tool for versioning Dhall configs
    - Maintain a category whose objects are versions and whose morphisms are Dhall functions for upgrading between those versions
    - Might require it to be a [thin category](https://ncatlab.org/nlab/show/thin+category), or at least have canonical resolutions
      - Could use normalization to check thinness
      - Probably want extra eta principles
    - Generate automatic upgrades based on structure of the types where possible ([e.g.]{t=} removing a record field, adding a sum type)

### Linguistics of Programming

  - Iʼve been thinking a lot about [selective applicative functors](https://dl.acm.org/doi/10.1145/3341694) lately.
    Mostly through the perspective of two contrasting applications: [selective applicative parsers](https://cohost.org/monoidmusician/post/2588944-more-more-more-seman), and functors for [typechecking with better errors](comprehensive_errors.html).
  - [Algebra of CSS Selectors](css_selectors.html)

    The fun part of this work was how to interleave nested selectors.

    My conclusion back then (2018) was that fully implementing `:not()`{.css} had edge cases that were not possible.
    But it was written before the more advanced selectors like `:has()`{.css} became standard.

  - Relational operators versus algebraic operators

    Infix, mixfix, distfix … but what about relfix?

    I want `a < b < c`{.js} to stand for `a < b && b < c`{.js}!!
    (And this to be an extensible system that users can add their own operators and precedences to.)

    This is the main difference between how mathematicians (especially category theorists) treat \(X \to Y \to Z\) and how programmers treat `X -> Y -> Z`{.haskell}, by the way.
    The former means “two functions, which can be composed” and the latter means “a curried function of two arguments”.

### Paradigms

  - I should do a blog post on what “effects” mean in FP culture

    [tl;dr]{t=} is “effects are as effects do”

    likeeee the real important point is itʼs all about what you leave implicit in your notation

    itʼs about notation not semantics

    in particular, this point about notation is necessary to explain why effects encompasses “pure effects” (like `List`{.haskell}, `Either`{.haskell}, `Maybe`{.haskell}, which are just data structures) and “real side effects” (like `IO`{.haskell}/`Aff`{.haskell}/`Effect`{.haskell}, of the “launch missiles” variety)

    I suppose I need to pretend to recite the history of monads and do notation in Haskell (“programmable semicolon”)

    and explaining why lists model “nondeterminism” could be a whole post of its own …

    > side-{effect, condition} really just mean “not explicitly tracked in the surface syntax” <!-- [2022/06/23]{.dated} -->

  - I should also do a blog post on “what people get wrong about the lens/prism/optics laws”, thatʼll be simpler (if more tricky)

    the main problem is that we donʼt have a good way to talk about them without invoking a metatheory of Haskell, with unification and possibly parametricity (Iʼll need to see if that comes up)

    actually, maybe parametricity is “just” the answer, and we can extract a free law from the stated laws!
  - [WIP]{t=}: [Why you should believe in HoTT Path Induction as a (Haskell) Programmer](programming_paths.html)
  - Passive stability:

    > Imperative code has no passive stability: all global state is mutable, and small perturbations (say, modifying a prototype) can cause unpredictably large effects down the line, if not carefully managed. Untyped code is especially bad. Carefully managed imperative code may be stable, but this is active stability: it is not inherent to the framework but imposed on top.
    >
    > Functional code on the other hand is not only passively stable, it is anchored down solid. Each building block is given a static, local denotation, and they compose together. You may still need nonlocal knowledge of the code to understand the intent of what everything represents (ahem, [boolean blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)), but the local meaning is denotationally clear.

    - I wonder: can you define a safe subset of Python/JavaScript that cannot be hijacked by mutating globals?
      Surely it is possible at the bytecode level, but I mean in source syntax.
      I guess if statements would be a start.
      Are numeric operations safe in Python?

  - log levels considered harmful

### Data

_data = information^[bits & bytes] + structure^[expectations of what the bits & bytes can be and what they mean]_

  - The Anatomy of ADTs
  - [WIP]{t=}: [Subtypes/Quotients: Lies Told in Defense of the Truth](adt_lies_for_truth.html)
  - Quotients: Lets us have nice cake and eat things too.
  - [Hereditarily Finite Sets (HFSes)](hereditarily_finite_sets.html)
    - [HatStack](hatstack.html), a stack based, concatenative language for Hereditarily Finite Sets

### Mathematics

  - [Lebesgue measure](https://en.wikipedia.org/wiki/Lebesgue_measure) and [Lebesgue integration](https://en.wikipedia.org/wiki/Lebesgue_integration)

    My two observations:

    #. I love how in order to prove how the old-fashioned Riemann integral works (characterizing what functions are Riemann integrable), you essentially have to come up with the Lebesgue measure, which paves the way for the Lebesgue integral.
    2. It is incredibly cool and incredibly counterintuitive how turning integration on its *side* produces better results.

### Typeclasses

  - Fundeps: how they affect defining instances and instance resolution
  - Instances as biïmplications, and the unfortunate consequences
    (It is really hard to actually come up for a use for this, outside of specific reflection/reification contexts ... harrumph maybe it actually breaks that too.)
    (Actually I think it is fine, but it needs to be opt-in for the sake of APIs/incremental builds.)
  - [Redesigning `Coercible`{.purescript}](coercible.html)
