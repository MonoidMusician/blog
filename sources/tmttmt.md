---
title: TransMorphism Type Theory MetaTheory
subtitle: A programming language (front-end?) for making other programming languages!
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2023/10/13 – …
---


[🦋 _heck u! \*programs ur semicolons :3\*_ 🦋]{style="display: block;text-align: center;font-size:1.2em"}

Iʼve been dreaming of making my own metalanguage for writing type theories for many years now.
I havenʼt implemented much yet, but Iʼve been refining my ideas.
Hereʼs a tiny taste:

_What you write:_

```tmttmt{data-lang="tmTTmt"}
normalize:: Term -> Term
## If we are casing on a known boolean, we know which case to choose
normalize: ["if" "true" "then" exprT "else" exprF] => exprT;
normalize: ["if" "false" "then" exprT "else" exprF] => exprF;
## If both branches have the same value, the boolean is irrelevant
## This is an example of non-linear pattern matching, which will get desugared
normalize: ["if" _cond "then" expr "else" expr] => expr;
## Fallback case, including most other nodes not specified
normalize: layer => normalized:
  ## ^ We compute the result, `normalized`, by following these operation(s):
  ## Recursively calling normalize on each child node of this node
  ## (that's it, that's the only operation in this case, but there could be more)
  map normalize layer => normalized
  ## ^ okay this line probably needs tweaking for type inference ...
```

_What it means:_

```haskell
normalize (Roll (IfThenElse metadata cond exprT exprF)) =
  case normalize cond of
    -- Note that "true" above is allowed to stand for the literal in the AST
    -- (as well as the Boolean type in tmTTmt itself), but in Haskell we need
    -- an explicit constructor `BooleanLiteral` to embed it in the AST:
    BooleanLiteral True -> exprT
    BooleanLiteral False -> exprF
    condN ->
      let (exprTN, exprFN) = (normalize exprT, normalize exprF)
      in case areEqual exprTN exprFN of
        -- Every time we equate two expressions, especially though matching on
        -- the same variable name twice, we return a unified node, so that we
        -- have a chance to merge *metadata* when the underlying *data* is
        -- the same. In typechecking, unifying can also unify metavariables,
        -- through algebraic effects or something.
        Just exprN ->
          exprN
        -- We fall back to the `map` operation, special cased for the node we
        -- already matched here. We can pass the metadata through to the result,
        -- or update it -- but how?? It will be specified in other places,
        -- e.g. through typeclasses and auxiliary functions ...
        Nothing ->
          Roll (IfThenElse (updateMetadata?? metadata) condN exprTN exprFN)
normalize (Roll layer) = Roll (map normalize layer)
```

I want to skew to the left of many design choices that have been made for most languages, aiming for very specific tradeoffs to achieve aesthetics and functionality.

My goal is to have a syntax that is straightforward for humans to write and easy for computers to interpret.

But I donʼt want this process to be magic!
I just want it to look convenient for writing powerful systems of type theory.

## Abstract

[tmTTmt](blog.veritates.love/tmttmt.html) (transmorphism Type Theory metatheory) is an experiment in the UX and aesthetics of a pure, typed functional programming language, with the goal of being an interlingua (it must be easy to parse, reducible to an implementation with few required primitives, and human readable along the way, including methods for understanding what the source code is and does) and with an eye towards being suitable for being an exceptional metalanguage for type theory and compilers and also a welcoming place to write down algorithms of general interest (e.g. computing a cubic Bézier from a quadratic Bézier, or computing an LR(1) parse table from a grammar, or describing high-level cryptography operations that can be compiled to appropriate code in languages such as JavaScript, Python, or Erlang)

## Demos

AAaaahhhh

Implementation in progress!

See also [Development].

::: {.widget widget="Widget.Query" widget-empty="true" widget-datakey="default" widget-data-keys="tmttmt-example"}
:::

### Type system

::: {.widget widget="Parser.Main.TMTTMT" widget-loading="true" style="display: contents" widget-data-example="typecheck-examples"}
:::

### Evaluation

::: {.widget widget="Parser.Main.TMTTMT" widget-loading="true" style="display: contents" widget-data-example="eval-examples"}
:::


## Motivation

My problem with existing programming languages is that they are too heavily tied to a fixed logic of computation: ADTs in Haskell are great (especially for language design!), at least right up until you need to add extra data to your ADTs, and now that infects your whole program with ugliness and bookkeeping.

In general, **this distinction between data and metadata** is so *crucially* important to me.
And not something that is really considered in any typed programming language!
Data has always gotten tied directly to the logic you are working in, and metadata was given no freedom to roam around.
So letʼs unleash them.
Let it loose :3

:::Example
As a very concrete example of this, imagine caching which variables are present in each term (such as [Director strings](https://en.wikipedia.org/wiki/Director_string), or a simple extra `Set Name` on each node).
This can be used to skip allocations when you know nothing is changing in operations that target variables.
But now you need to keep track of that information literally everwhere you construct nodes in your AST!
Despite it being a really simple algorithm to compute on its own, one that hopefully could be incrementally updated in most cases.

As another example, source spans are really tricky to keep around and get right.
(I have thoughts on that – that we shouldnʼt be using source spans! – but thatʼs beside the point.)
But imagine if you didnʼt have to do any work to keep them around: the logic of tmTTmt could keep that information around, and generate empty source spans for nodes that are inserted by the compiler.
(With some way to optionally borrow source spans from other node(s).)

As a third example of why we should separate data and metadata: if we keep the identity of nodes separate from their raw data, we can keep track of which source terms interact with each other.
The better you can keep track of source spans and *provenance*, the more reliable this system will be.
If you keep track of which types are unified with each other, and can map them back to locations in the source, it could even tell you all of the places you need to edit if you want to change the type of something (or where you need to insert conversions).
:::

If you arenʼt forced to work in the base logic of Haskell, and instead have more freedom to come up with a linguistics and logic of type theory design itself, youʼll get several benefits:

- You donʼt have to rewrite your whole compiler to introduce optimizations, like I mentioned above.
- You could generate graphical representations of the rules, from the exact same source that the program is derived from.
  This would be fantastic for interactive typechecking rules, which could enhance error messages with the particular rule that failed to be derivable, and allow you to search back through the parts of the derivation that did succeed.

  :::Bonus
  You may still want separate sets of rules for conventional “paper” presentations, however.
  Like as a term rewriting system, instead of [NbE]{t=}, for example.
  But if both of them are executable, you can test whether they are equivalent!
  (With QuickCheck, unit tests, or assertions.)
  :::
- [dhall-lang#469: Machine-readable semantics](https://github.com/dhall-lang/dhall-lang/issues/469)

### History

Iʼve been ranting about tmTTmt on cohost as an outlet until I have the time to actually write the darn thing: [@monoidmusician/#tmttmt](https://cohost.org/monoidmusician/tagged/tmttmt).

The real genesis of the project was when I was attempting to write Dhall-PureScript (many apologies for dropping the ball on that one).
I wanted to go all in on extensible row types and recursion schemes.
I think theyʼre awesome tools, but they proved too impractical to use in PureScript, since they wrecked the existing pattern matching facilities of the core logic of PureScript.
I have also learned a lot in the meantime ([e.g.]{t=} about parsing, about languages like Erlang) and developed a lot more ideas.
I think Iʼm ready to make it happen!

## General design goals

- Simple syntax, which can be easily interpreted by other programs in other ways.
- Type-directed shenanigans.
  (Typeclasses, mostly. Also type-directed syntax sugar.)
- Algebraic effects, or something.
  This is necessary for lightweight unification.
- Compilation steps to get from that syntax to some core logic/runtime in some language.
  - Desugaring nonlinear patterns into appropriate equality/unification steps.
  - Desugaring core logic into monads/applicative(/selectives?)
  - Inlining; removing redundant steps.
    - In particular, it will be the expectation that these operations are safe for any custom monads/effects that users use.
      As an example, resolving imports in [Dhall](https://dhall-lang.org/) requires doing disk access and network requests, but those network requests are cached during resolving, so their resolution is idempotent and redundancies can safely be removed.
      (And obviously if there are URLs that do not have to be resolved, thatʼs great for efficiency! Although you can argue about safety, which is why these things need to be customizable.)
  - Personally I think it would be fun to target PureScript, JavaScript, Erlang ... very different needs across each of those.
- Functors! I love functors.
- Encourage healthy abstractions.
  I think thatʼs a great word: _healthy_ abstractions.
- I have this idea for a type system and I donʼt know if it will pan out&nbsp;…
  Something like TypeScript done better (or similar sorts of ad-hoc type systems).
- Easy debugging and decent dev UX.
  Being able to dump terms in a representable/inspectable format.
  Being able to trace execution and focus logs.
  Flags to enable/disenable features.
  Assertions.
  Idk.

### Specific design choices

- Lightweight literals, type-directed.
  - A literal is a string or a list of literals or variables.
    (Basically reinventing lisp lol.)
  - Types are basically patterns of literals, meaning literal singleton types plus arrays and unions of types, like TypeScript but hopefully in a sensible way.
    Thus it is obvious what type a literal has, and then this can be subsumed by other types.
  - There are also nominal types; still figuring out the details there.
    The main goal is to mostly get rid of newtype wrappers, so you can just match on the constructors-as-literals you want through all the cruft.
    But type annotations will still be necessary to disambiguate types in some cases.
    And full type annotations are usually tedious, so some system of named coercions may be helpful.

  - :::Key_Idea
    In particular, by committing ourselves to _this_ logic of **literals as ground truth for comparing _across types_**, we can generate automatic coercions between subsets of complex types.

    I understand why a lot of languages want literals to have distinct types ([e.g.]{t=} Haskell ADTs all have distinct, named constructors), but it just poses a barrier to the fluidity I want to have in this system for language design of all things. If you name something `["if" _ "then" _ "else" _]`{.tmttmt} then you know what it represents! No matter if it is in the source CST, the desugared AST, or a final core pass&nbsp;…
    :::

    In some target runtimes, if they are faithful to the literals, these will be actual zero-cost coercions.
    However, because the expectation is that compilation is type-directed and enough type information is available to insert conversions as necessary, there is no reason that they are required to be coercions in implementation.

  - Restriction types, of nominal types constrained to fewer possible cases, would be incredibly useful.

  - [tl;dr]{t=} is that this should help with the [“trees that grow”](https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf) problem of multiple related ASTs.

  - Iʼm wavering on including records: I think they donʼt mesh well with the system of inference.
    But there is an alternative, which is to include sort of “grab bags”: where you donʼt pretend to know the totality of the record (in particular, there is no sensible way to implement `Eq`{.haskell} for records), but you have some partial knowledge of what you want to be in there.

    In concrete terms, this means that inclusion in the grab bag is the only sensible constraint you get to ask for; you donʼt really get to “delete fields” or “merge” or such.

  - Avoiding row types&nbsp;… idk.
    Row types are great but I think there are enough alternatives in this type theory that they would not be so important.
    In particular, having union types (and maybe restriction types) means that you can talk about parts of the AST.

    If I did have row types, I would want to make sure they are not limited to existing constructs of records and variants (product and sum types), there are so many other symmetric tensors to think about!
    [E.g.]{t=} configuration options tend to come as a record of maybes, but sometimes you need a bunch of things tensored together with `These`{.haskell}, so you know that at least one is specified.

- Function calls cannot be nested, functions are only applied to literals/variables.
  - This is for two reasons: it makes the syntax lighter, and it means that the user was very specific about the order of execution.
  - One concrete benefit is that you need much fewer delimiters in the syntax, since each pattern ends at a well-known point.

#### Patterns

https://cohost.org/monoidmusician/post/3252802-first-class-patterns

- We need a way to reflect patterns into values, filling in any variables with default values.
  This is most useful to implement unification: to unify a term with a pattern, you first replace the variables with unification variables, call the unification function (which has no idea what a pattern is), and then match the pattern against the result.

  So if you want to unify `T`{.tmttmt} against `["tuple" x y]`{.tmttmt}, you first generate two unification variables `U1` and `U2`, then run `unify T ["tuple" U1 U2] => R`{.tmttmt} (if `T`{.tmttmt} is a unification variable, this will write into state that it is now known to be a tuple!), and finally do regular pattern matching of `R`{.tmttmt} against `["tuple" x y]`{.tmttmt}, binding `x`{.tmttmt} and `y`{.tmttmt} to the respective subnodes of `R`{.tmttmt}.

  - Iʼm not quite sure if this deserves to be called first-class patterns.
    To be honest, Iʼm not sure what first-class patterns would even mean!
    But it is very simple, practical, and it does all the things I personally would want out of first-class patterns.

  - It is a technique I have also been using in more and more places: in my [LR parser combinator framework](https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Languages/Talk.purs), and in writing a compiler.

    The basic idea is that a `Pattern`{.haskell} or `Matcher`{.haskell} (or whatever you want to call it) is a combination of the shape that it expects (minus actual data), and then what to do once it receives that shape (with the data filled in, producing some arbitrary result).
    You can combine these applicatively and with other tools (if you can combine the shapes and pick them back apart); it is very useful, even without any language support whatsoever, just DSLs and fancy types.
    These are basically codecs in a single direction (not bidirectional).

- Non-linear pattern matching.

- Static evaluation of functions, so they can be considered as macros producing patterns.

  This means that they have to reduce to a pattern, without running any effectful functions, and cannot have stuck case matching, and so on.

- Pattern aliases?

### Use-cases to achieve

- Writing down programs in the style of judgment rules for different flavours of typechecking (unification and bidirectional) and normalization (rewrite systems and normalization by evaluation).
- Optimizing these algorithms by applying transformations from the source code to add \~things\~.
  And using the same techniques to add additional metadata, for nice features.

  :::{.Bonus box-name="Mini Rant"}
  This is my problem with a ton of code that gets written, and I am certainly guilty of it too:
  we get the fundamental logic written, but never get over the hump to the point of providing nice features, in part because the languages/libraries we use do not facilitate the nice features – heck, they even made writing the logic so arduous in the first place that we run out of steam – and partly because, as I have mentioned repeatedly, it would mean touching half of the codebase again just to add fields that none of the existing logic cares about.

  Urgh.
  Letʼs find ways to do better, and create the tools that reflect our values.

  … Anyways, back to those use-cases:
  :::

- Trace evaluation of these programs and generate interactive visualizations based on this.
- Generate types for precise errors based on analyzing failure modes of the written logic.
- Working with multiple related AST types.
  Working with types related in other ways, such as non-empty constraints.
  (These get pretty onerous to work with, when you have to write completely separate types for non empty things, and make sure you preserve non-emptiness in the right places. Trust me, Iʼve tried!)
- Simplify writing advanced programming techniques:
  - Continuation Passing Style (CPS).
    This is (apparently) really great for efficiency for a bunch of reasons ([e.g.]{t=} quicker backtracking), but can be mind-bending to write directly.
  - Deriving zippers/one-hole contexts for datatypes, possibly even [Clowns & Jokers](http://strictlypositive.org/CJ.pdf) style for incremental stack-safe computations.
    (One-hole contexts are possible to derive generically with typeclass machinery.
    But the conversions get super annoying...)
  - Functional Reactive Programming (FRP).
    Existing FRP frameworks are&nbsp;… alright.
    But none really capture the right logic/linguistics to make it easy.
  - Incremental computation.
    I mean&nbsp;… just imagine an incremental compiler, where trivial refactors donʼt cost any time, changing constants in the source code changes them directly in the compiled artefacts, and other tasks scale proportionally to the amount of things they actually affect.
  - “Free” constructions (I mean, minus laws, since we donʼt have quotients).
    These are just so difficult to make, with a lot of boilerplate.
  - Codecs. Parsing.
    I love parsers so it would be great to integrate them.
    Maybe even into the type theory!
    (It is apparently possible to algorithmically decided whether one regular expression is contained in another uwu :3.)
  - STM. Eventual consistency. Other lattice-y stuff.
  - Parallel evaluation, à la `unamb`{.haskell} or so.
- Idiolects?
  - Providing N×M variations of APIs is annoying:
    ```purescript
    setPrecA :: forall rec err prec nt cat o. cat -> Associativity -> prec -> Comb rec err prec nt cat o Unit
    setPrecA cat assoc prec = case pure unit of
      Comb c -> Comb c { tokenPrecedence = [cat /\ (prec /\ assoc)] }

    setPrecL :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
    setPrecL = setPrecA <@> AssocL
    setPrecR :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
    setPrecR = setPrecA <@> AssocR
    setPrec :: forall rec err prec nt cat o. cat -> prec -> Comb rec err prec nt cat o Unit
    setPrec = setPrecA <@> NoAssoc

    tokenPrecA :: forall rec err prec nt cat o. Token cat o => cat -> Associativity -> prec -> Comb rec err prec nt cat o o
    tokenPrecA cat assoc prec = setPrecA cat assoc prec *> token cat

    tokenPrecL :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
    tokenPrecL = tokenPrecA <@> AssocL
    tokenPrecR :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
    tokenPrecR = tokenPrecA <@> AssocR
    tokenPrec :: forall rec err prec nt cat o. Token cat o => cat -> prec -> Comb rec err prec nt cat o o
    tokenPrec = tokenPrecA <@> NoAssoc
    ```

    Does reifying the arguments as a datatype with super lightweight syntax do what we need? maybe&nbsp;…
  - Would be nice to have ways to say “add a precedence to *this* (whatever it is)” or “add a name to *that*”.
    Idk.

## Non-goals

- Not intended to support dependent types or any theorem proving features.
- This is not intended to be a logic language, although it could be compiled to a logic language.
  Thus we will not expect to be doing proof search during execution.
  (Arguably could be doing proof search during compilation.)
- Similarly: not interested in baking in unification.
  That can (and should) be provided by users; the goal is to make the syntax lightweight enough to facilitate it.
- Probably not going to have Rank-N types for a while, if ever.
  I mean, I like Rank-N types, especially for APIs, but most things end up being better expressed by inductive data types, and this way I have a type inference algorithm that is actually tractable&nbsp;…

## More Details

(A separate section so I donʼt bury [Non-goals])

### Abstractions I want

Worship the shape of data and the structure of code ...

- Any metatheory that makes dealing with variable binding easier is worth a lot!
  - What I did in Dhall-PureScript: [Dhall/Variables.purs](https://github.com/MonoidMusician/dhall-purescript/blob/469c3e10d51a8afb90f2e231cf3e6101f50814eb/src/Dhall/Variables.purs#L119-L201)
    This just does basic bookkeeping of when variables are bound, based on the functors I used in the recursion schemes, but I think it proved to do most of what I needed.
  - :::{.Bonus box-name="Aside"}
    The other, silly solution, is to commit to only having one binder: lambda, and phrasing pi in terms of lambda.
    I convinced myself it works out on paper but I got a little stuck trying to prove it to Agda.
    Heh heh heh&nbsp;…
    :::
- Container functors, the building blocks of an AST.
  - `traverseWithIndex :: (i -> a -> m b) -> (f a -> m (f b))`{.haskell}
  - `mergeWith :: (i -> a -> b -> c) -> f a -> f b -> Maybe (f c)`{.haskell}
    - I believe that we need a lot more binary operations like this, for matching on two shapes at once!
      It is not something that is covered by recursion schemes for example.
      `Data.Map`{.haskell} has a terrible interface (`unionWith`{.haskell} is so bleh).
  - [Zippers/one-hole contexts](https://github.com/MonoidMusician/dhall-purescript/blob/main/src/Dhall/Core/Zippers.purs) (optional – I never actually used them in Dhall-PureScript, but they could be useful for some things):
    - `upZF :: ZF f' x -> f x`{.haskell}
    - `downZF :: f x -> f (ZF f' x)`{.haskell}
    - `ixF :: f' x -> i`{.haskell}
- Array stuff
  - normal `zipWith`
  - “long” `zipWith`
  - `takeWhileJustWithRest :: (a -> Maybe b) -> Array a -> (Array b, Array a)`
    - some kind of condensor pattern
  - something better than `mapAccumL`/`mapAccumR` lol
    - every time I want to reach for a stateful traversal, I find it so annoying!
  - maybe some actual parser type thing

### Examples

_\~Disclaimer that I use typechecking and type inference interchangeably.\~_

#### Typechecking lists

I think it is *very **very*** useful to move from thinking of unification as a binary operation to it as a N-ary operation.
As one example, consider (homogeneous) list literals.

The way a lot of typecheckers work when inferring list literals is that it assumes the first item has the right type, and then it typechecks the remaining items against it.
But what if it is the first item that has the wrong type, and all 12 other items are actually right?
I believe it is best to typecheck each term in isolation, then see if the results can be unified all at once – and then unify the unification states, since unification variables may have been unified in inconsistent ways.
(This requires unification state to be `WriterT`{.haskell} not `StateT`{.haskell}. Yeah.)

```tmttmt{data-lang="tmTTmt"}
typecheck ["ListLiteral" items] => ["App" "ListType" itemType]
  map typecheck items => itemTypes
  ensureConsistency itemTypes => itemType
```

#### Typechecking non-dependent pi types

I would like to be able to short-circuit typechecking non-dependent functions, and return a result even if the argument is ill-typed or does not have the correct type.

(Why? Because having a more global view of errors is often useful, since the hyperlocal errors we are used to can obscure the real problem.)

This would show up as a soft error that allows further typechecking to proceed.
Soft errors can be turned into critical errors when we need to be able to trust the result of typechecking, [e.g.]{t=} to know that normalization is going to complete.

```tmttmt{data-lang="tmTTmt"}
typecheck: term -> type
typecheck ["App" fn arg] => resultType:
  ## Unifies the result with a "Pi" type
  typecheck fn => ["Pi" binder domain codomain]
  ## See if `codomain` does not in fact depend on `binder`
  tryApplyConstant binder codomain
  ? ["constant" resultType]:
    ## `resultType` got assigned, so this case is not necessary to produce
    ## *some* result that can inform further type errors, though this node does
    ## not truly typecheck if it fails:
    typecheck arg => domain
    ## `domain` is a non-linear pattern match, unifying `argType` and `domain`
    ## (any further references to `domain` would refer to the unified node)
  ? ["non-constant"]:
    ## Typecheck the argument in strict mode to ensure that type errors result
    ## in an immediate failure even if an approximate result can be computed:
    strictly (\[] => typecheck arg) => domain
    ## (Unification with `domain` is always strict, it never adds soft errors.)

    ## Now that it is safe to compute with `arg`, we apply it to compute the
    ## result type:
    substitute binder arg codomain => resultType
  !

## Probably should simplify this somehow ...
```

<!--
branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
-->

:::{.Note box-name="Aside"}
Is this good notation for lambdas as arguments to functions?
I donʼt know.

```tmttmt{data-lang="tmTTmt"}
  strictly | [] => r:
    typecheck arg => r
  ! => domain
```

Macros for currying?

```tmttmt{data-lang="tmTTmt"}
asdf (!2 append !1 !0 !)
```

I want to avoid some problems:

- Indentation. Figuring out how to indent lambdas as arguments to functions is so annoying.
- Related: figuring out where the lambdas end is also annoying.
  I do like dangling lambdas actually.

```tmttmt{data-lang="tmTTmt"}
["if" ($matches-tag arg1) (: MyExprType) "then" "true" "else" ($failed-match)]
```
:::

```haskell
-- The behavior of `select` for the typechecker monad/thingy is that if the
-- first computation returns a `Left`, it will accumulate errors from the second
-- computation, instead of failing and blocking computation like `>>=`.
--
-- In particular, it does accumulate errors from `strictly`, in that case.
select :: f (Either b a) -> f (a -> b) -> f b
strictly :: f a -> f a
tryApplyConstant :: Binder -> Type -> Maybe Type

typecheck :: Type -> f Type
typecheck (App fn arg) =
  select
    ( typecheck fn >>= \fnType ->
        unifyPi fnType >>= \binder domain codomain ->
          case tryApplyConstant binder codomain of
            Just r -> Left r
            Nothing -> Right Unit
    )
    ( strictly $ typecheck arg >>= \argType ->
        unify argType domain <#> \_unified ->
          apply binder arg codomain
    )
```

## Development

For ease of development I am just embedding it as part of the [the code for my blog](https://github.com/MonoidMusician/blog/tree/main/PureScript/src/Parser/Languages/TMTTMT).

I have a rough parser (needs to be updated) using my parser combinator library, yay.

Working on the type system currently: starting with structural types.
Then need nominal types, constraints, effects, polymorphism, ...

The evaluator needs local variables.

### Types

Implementing the basic structural type system for tmTTmt.
Basically composed of unions of structural types in the language of JSON, including singletons, tuples, (non-empty) lists, and (non-empty) strings.
(No objects, yet – those require constraints.)
Oh, and functions, though you cannot interact with those via pattern matching (no view patterns!).

Enter a type in the top box and some patterns in the bottom box, and it will show you the type that each pattern matches (and the types of the variables it binds), and it will show you the refined type of the leftover *non*-match at the end.
(Currently does not handle the fact that non-linear pattern matches should not refine the type ... or something. Idk how exhaustivity checking is going to work tbh.)

::: {.widget widget="Parser.Main.TMTTMT.TypeSplit" widget-loading="true" style="display: contents"}
:::

The rough grammar for types:

```bnf
ty =
  | '+' ty             ## non-empty list
  | '*' ty             ## possibly-empty list, sugar for `'+' ty '|' '[' ']'`
  | '?' ty             ## optional, sugar for `'[' ty ']' '|' '[' ']'`
  | ty '|' ty          ## union (may also have leading bars, or trailing bars, if within parentheses)
  | '$$'               ## non-empty strings
  | '$'                ## strings, sugar for `'$$' '|' '"' '"'`
  | string             ## string singleton
  | ty '->' ty         ## function type
  | name               ## type variable
  | '(' ty+ ')'        ## type application (maybe?)
  | '(?' ty* '?'? ')'  ## type hole
  | '(' '|' ')'        ## empty type (Void/never)
  | '[' aspect* ']'                 ## vector literal
  | '{'  (string '=' aspect)*  '}'  ## hash literal
  | '{.' (string '=' aspect)* '.}'  ## enumerated record literal
```

<details class="Example">

<summary>Example types</summary>

```tmttmt
(# primitive #)
#type(Void)( (|) )

#type(Unit)( [] )

#type(Wrap a)( [a] )

(# sugar for `$$ | ""` #)
#type(String)($)
(# primitive #)
#type(NEString)($$)

(# `Maybe a` can be coerced to `List a` #)
#type(Maybe a)([] | [a])

Maybe2List:: #forall(a)(Maybe a -> List a)
Maybe2List: a => a

(# sugar for `+a | []` #)
#type(List a)(*a)
(# primitive #)
#type(NEList a)(+a)

#type(Cons a)([] | [a (Cons a)])
#type(Snoc a)([] | [(Snoc a) a])

#type(Endo a)(a -> a)

#type(Tuple a b)([a b])

#type(Either a b)(
  | ["L" a]
  | ["R" b]
)

(# newtype (once I have nominal types) #)
#type(Validation a b)(Either a b)

#type(These a b)(
  | Either a b
  | ["B" a b]
)

(# strings and lists, recursively #)
#type(AnyData)(
  | $
  | *AnyData
)

(# strings, lists, and functions, recursively #)
(# for an untyped language #)
#type(UniType)(
  | AnyData
  | (UniType -> UniType)
)

(# sorry not sorry #)
(# (you will appreciate me later) #)
#type(Nat)([] | [Nat])
```

</details>

Check whether one type is a subtype of another:

::: {.widget widget="Parser.Main.TMTTMT.SubType" widget-loading="true" style="display: contents"}
:::
