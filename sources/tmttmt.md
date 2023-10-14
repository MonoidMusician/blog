---
title: TransMorphism Type Theory MetaTheory
subtitle: A programming language (front-end?) for making other programming languages!
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---


[🦋 _heck u! \*programs ur semicolons :3\*_ 🦋]{style="display: block;text-align: center;font-size:1.2em"}

Iʼve been dreaming of making my own metatheory for writing type theories for many years now.
I havenʼt implemented much yet, but Iʼve been refining my ideas.
Hereʼs a tiny taste:

_What you write:_

```{.js data-lang="tmTTmt"}
normalize ["if" "true" "then" exprT "else" exprF] => exprT
normalize ["if" "false" "then" exprT "else" exprF] => exprF
normalize ["if" _cond "then" expr "else" expr] => expr
normalize layer => normalized:
  map normalize layer => normalized
  // ^ okay this last line probably needs tweaking for type inference
```

_What it means:_

```{.haskell data-lang="Haskell"}
normalize (Roll (IfThenElse metadata cond exprT exprF)) =
  case normalize cond of
    -- Note that "true" above is allowed to stand for the literal in the AST
    -- (as well as the Boolean type in tmTTmt itself)
    BoolLit True -> exprT
    BoolLit False -> exprF
    condN ->
      let (exprTN, exprFN) = (normalize exprT, normalize exprF)
      in case unify exprTN exprFN of
        -- Every time we unify two expressions, especially though matching on
        -- the same variable name twice, we return a unified node, so that we
        -- have a chance to merge metadata even though the underlying data is
        -- the same. In typechecking, unifying can also unify metavariables.
        Just exprN ->
          exprN
        -- We fall back to the (special case) of the map operation. We can
        -- pass the metadata through, or update it -- but how? That will be
        -- specified in other places, e.g. through typeclasses and auxiliary
        -- functions ...
        Nothing ->
          Roll (IfThenElse (updateMetadata?? metadata) condN exprTN exprFN)
normalize (Roll layer) = Roll (map normalize layer)
```

I want to skew to the left of many design choices that have been made for most languages, aiming for very specific tradeoffs to achieve aesthetics and functionality.

My goal is to have a syntax that is straightforward for humans to write and easy for computers to interpret.

But I donʼt want this process to be magic!
I just want it to look convenient for writing powerful systems of type theory.



## Motivation

My problem with existing programming languages is that they are too heavily tied to a fixed logic of computation: ADTs in Haskell are great (especially for language design!), at least right up until you need to add extra data to your ADTs, and now that infects your whole program with ugliness and bookkeeping.

In general, **this distinction between data and metadata** is so *crucially* important to me.
And not something that is really considered in any typed programming language!
Data always got tied directly to the logic you are working in, and metadata was given no freedom to roam around.
So letʼs unleash them.
Let it loose :3

:::Example
As a very concrete example of this, imagine adding caching of which variables are present in each term (such as [Director strings](https://en.wikipedia.org/wiki/Director_string), or a simple extra `Set Name` on each node).
This can be used to skip allocations when you know nothing is changing.
But now you need to keep track of that information literally everwhere you construct nodes in your AST!
Even though it is a really simple algorithm to compute on its own, and hopefully it could be incrementally updated in most cases.

As another example, source spans are really tricky to keep around and get right.
(I have thoughts on that – that we shouldnʼt be using source spans! – but thatʼs beside the point.)
But imagine if you didnʼt have to do any work to keep them around: they logic of tmTTmt could keep that information around, and generate empty source spans for nodes that are inserted by the compiler.
(With some way to optionally borrow source spans from other node(s).)
:::

If you arenʼt forced to work in the base logic of Haskell, and instead have more freedom to come up with a linguistics and logic of type theory design itself, youʼll get several benefits:

- You donʼt have to rewrite your whole compiler to introduce optimizations, like I mentioned above.
- You could generate graphical representations of the rules, from the exact same source that the program is derived from.
  This would be fantastic for interactive typechecking rules, which could enhance error messages with the particular rule that failed to be derivable, and allow you to search back through the parts of the derivation that did succeed.

  :::Bonus
  You may still want separate sets of rules for conventional “paper” presentations, however.
  Like as a term rewriting system, instead of NbE, for example.
  But if both of them are executable, you can test whether they are equivalent!
  (With QuickCheck, unit tests, or assertions.)
  :::
- [dhall-lang#469: Machine-readable semantics](https://github.com/dhall-lang/dhall-lang/issues/469)

### History

Iʼve been ranting about tmTTmt on cohost as an outlet until I have the time to actually write the darn thing: [@monoidmusician/#tmttmt](https://cohost.org/monoidmusician/tagged/tmttmt).

The real genesis of the project was when I was attempting to write Dhall-PureScript (many apologies for dropping the ball on that one).
I wanted to go all in on extensible row types and recursion schemes.
I think theyʼre awesome tools, but they proved too impractical to use in PureScript, since they wrecked the existing pattern matching facilities of the core logic of PureScript.
I have also learned a lot in the meantime (e.g. about parsing, about languages like Erlang) and developed a lot more ideas.
I think Iʼm ready to make it happen!

## General design goals

- Simple syntax, which can be easily interpreted by other programs in other ways.
- Type-directed shenanigans.
  (Typeclasses, mostly. Also type-directed syntax sugar.)
- Algebraic effects, or something.
  This is necessary for lightweight unification.
- Compilation steps to get from that syntax to some core logic.
  - Desugaring nonlinear patterns into appropriate equality/unification steps.
  - Desugaring core logic into monads/applicative(/selectives?)
  - Inlining, removing redundant/unused steps.
    - In particular, it will be the expectation that these operations are safe for any custom monads/effects that users use.
      As an example, resolving imports in [Dhall](https://dhall-lang.org/) requires doing disk access and network requests, but those network requests are cached during resolving, so their resolution is idempotent and redundancies can safely be removed.
      (And obviously if there are thigns that do not have to be resolved, thatʼs great for efficiency! Although you can argue about safety, which is why these things need to be customizable.)
- Functors! I love functors.
- Encourage healthy abstractions.
  I think thatʼs a great word: _healthy_ abstractions.
- I have this idea for types and I donʼt know if it will pan out …
- Easy debugging and decent dev UX.
  Being able to dump terms in a representable/inspectable format.
  Being able to trace execution and focus logs.
  Flags to enable/disenable features.
  Assertions.
  Idk.

### Specific design choices

- Lightweight literals, type-directed.
  - Helps with the [“trees that grow”](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf) problem of multiple related ASTs.
  - In particular, by committing ourselves to _this_ logic of literals as ground truth, we can generate automatic coercions between subsets of complex types.
    In some targets, if they are faithful to the literals, these will be actual coercions, but because the expectation is that this is type-directed and enough type information is available, there is no reason that they are required to be coercions.
- Function calls cannot be nested; each step of logic is explicit.
  - This is for two reasons: it makes the syntax lighter, and it means that the user was very specific about the order of execution.
  - One concrete benefit is that you need much fewer delimiters, since each pattern ends at a well-known point.

### Use-cases to achieve

- I also love parsers so it would be great to integrate them.
  Maybe even into the type theory!

## Non-goals

- Not intended to support dependent types or any theorem proving features.
- This is not inteded to be a logic language, although it could be compiled to a logic language.
  Thus we will not expect to be doing proof search during execution.
  (Arguably could be doing proof search during compilation.)
- Similarly: not interested in baking in unification.
  That can (and should) be provided by users; the goal is to make the syntax lightweight enough to facilitate it.
- Probably not going to have Rank-N types for a while, if ever.
  I mean, I like Rank-N types, but most things end up being better expressed by data types …
