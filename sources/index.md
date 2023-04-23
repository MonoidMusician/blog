---
title: MonoidMusicianʼs Blog
author: "[@MonoidMusician](https://github.com/MonoidMusician)"
---

Iʼm creating my blog here, and Iʼm trying to encourage my programmer friends here at `cofree.coffee`{style="font-weight: 700;"} to [do the same](blog.html).
This blog will be more focused on programming and mathematics, while my more personal stuff will probably continue to live on my personal website: [https://monoidmusician.github.io/](https://monoidmusician.github.io/).

Get the behind-the-scenes peek at the [code](https://github.com/MonoidMusician/blog) and my account of the [technologies](technology.html) that go into this blog.

## Posts
- [Impossible Bézier Calligraphy: Approximating cubic nibs drawn along cubic strokes](bezier_calligraphy.html)

  > Given a pen nib of some shape, what composite shape is produced when that pen is drawn along any particular path?
  > If the inputs are cubic Bézier curves, is the output as well?
  >
  > The catch?
  > Itʼs mathematically impossible to model the output using cubic Bézier curves, as I determined after a bit calculus.
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

## Series
- [Interactive Parser Explanations](parser.html)
  1. TODO: [Parsing By Example](parser_by_example.html)
  1. TODO: [Terminology Reference](parser_terminology)
  1. WIP: [Basics: What Are Grammars](parser_basics.html)
  1. WIP: [Uses of Grammars](parser_applications.html)
  1. WIP: [Basics of LR(1) Parsing](parser_lr1.html)

## Ideas/WIP
- Interactive type theory!
- Paradigms:
  - WIP: [Why you should believe in HoTT Path Induction as a (Haskell) Programmer](programming_paths.html)
  - Passive stability:

    > Imperative code has no passive stability: all global state is mutable, and small perturbations (say, modifying a prototype) can cause unpredictably large effects down the line, if not carefully managed. Untyped code is especially bad. Carefully managed imperative code may be stable, but this is active stability: it is not inherent to the framework but imposed on top.
    >
    > Functional code on the other hand is not only passively stable, it is anchored down solid. Each building block is given a static, local denotation, and they compose together. You may still need nonlocal knowledge of the code to understand the intent of what everything represents (ahem, [boolean blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)), but the local meaning is denotationally clear.

- Data:
  - The Anatomy of ADTs
  - WIP: [Subtypes/Quotients: Lies Told in Defense of the Truth](adt_lies_for_truth.html)
  - Quotients: Lets us have nice cake and eat things too.
- Typeclasses:
  - Fundeps: how they affect defining instances and instance resolution
  - Coercible redesign
