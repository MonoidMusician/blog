---
title: The Best Errors for Solving Dependency Versions
subtitle: For the PureScript registry
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

:::{.Bonus box-name="Backstory"}
> Just copy the [Elm version solver](https://github.com/elm/compiler/blob/0.19.1/builder/src/Deps/Solver.hs) from Haskell to PureScript, itʼll be easy.

Uh huh. Totally.

> Oh we need good errors too.

Yup. Thought so.^[The Elm version solver has [network errors](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/builder/src/Reporting/Exit.hs#L902-L905), but apparently [no actual errors](https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/builder/src/Deps/Solver.hs#L102) from the simple solve-by-backtracking algorithm.]
:::


And so the feature creep started … but the journey was _so_ worth it.

How did I get here and what did I come up with?

:::{.Key_Idea box-name="tl;dr"}
- A novel algorithm for resolving dependency bounds to solvable versions.
- Incorporates transitive dependency bounds for a breadth-first search.
- By taking this intuitive approach, we gain two things:
  1. Better errors, matching what users would expect.
  2. Efficiency too, if you could believe it.
- Implemented using semilattices (monoids).
:::

## The PureScript registry

The PureScript community has been designing a new registry to hold PureScript packages for some time now.
PureScript projects initially used [Bower](https://bower.io/) (a defunct npm competitor for Node.js packages), and I embarrassingly hung on to Bower until very recently.
Most of the community, however, has been using [Spago](https://github.com/purescript/spago), a PureScript build tool supporting package sets (fixed versions of packages that are known to be compatible).
Long story, but some core members have been designing a new registry to house current and historical PureScript packages.
Weʼre very close to releasing it!^[No, for real this time!!]

In the interest of maintaining a healthy ecosystem, we want the new registry to support not just package sets but also traditional version solving.
And thatʼs where I came in.
Something about my mathy skills being a perfect fit for getting [nerd-sniped](https://xkcd.com/356/) by a version solving algorithm.
Oh and would you help fix the versioning issues for legacy packages while youʼre at it?
Sure, sure I will.

## Version solving

Quick notes on conventions/background before we get too far in:

The actual details of how versions are tagged doesnʼt matter, just that they are totally ordered.^[Actually we probably donʼt even need a total order, a partial order would work fine for it?]
For example, it could just be flat integers for all we care.
But usually we take them to be lexicographically-ordered lists of integers, like `5.0.3` which is less than `5.1.0`.

How we form _ranges_ over versions is pretty important, though, and early on the registry decided to only allow half-open intervals.
That is, ranges have the form `>=A.B.C <X.Y.Z`, which I will use throughout this article.
Again, it isnʼt very sensitive to details here (who cares that it is half-open?), but this does seem to be the right level of generality.
Supporting more complex queries is asking for trouble.

Finally, from a version-solving point of view, a registry contains information of what versions of packages there are, and for each package version a record of what dependencies it requires and the appropriate version ranges for those packages.
That is, it can be represented with the following PureScript datatype:
```haskell
-- A list of required dependencies and their versions
type Manifest = Map PackageName Range

-- A list of all extant package versions
type RegistryIndex =
  Map PackageName
    (Map Version Manifest)
```

Solving means taking a manifest and finding versions for each package in it, preferring later versions^[This is actually a weird requirement, since there can be incomparable solutions that need to be tie-broken arbitrarily. In practice I do it alphabetically just by dint of how the package versions are tried.]:
```haskell
solve :: RegistryIndex -> Manifest -> Either SolverError (Map PackageName Version)
```

With some correctness constraints like
```haskell
isASolution = and
  -- All packages received a version
  [ m `isSubsetOf` fromRight (solve r m)
  -- All solved versions fit into the range
  -- as required in the manifest
  , allWithIndex
    ( \package version ->
        maybe false
          (\range -> range `includes` version)
          (Map.lookup package m)
    )
    (fromRight (solve r m))
  -- All versions
  ]
-- There is no strictly better versions to be found
isOptimalSolution =
-- Everything is required???
```

### Dependencies are tricky

In particular, note that dependencies are associated with a particular _version_.
A package _range_ doesnʼt need to have well-defined dependencies at all!

This is something that we forget about when using packages in our day-to-day lives, but an algorithm needs to handle all cases we throw at it.

HOWEVER, this insight is what makes my algorithm so good.

## Depth-first backtracking algorithm

As I alluded to in the intro, I started off by copying Elmʼs version solving algorithm.
Itʼs a very simple depth-first backtracking algorithm: try the latest compatible version of the package in front of you, add its dependencies and see if those all solve recursively, and backtrack to the next latest version at each failure.

Itʼs easy to see why this is worst-case exponential, and not going to hit fast cases particularly often.
In fact, we expect the problem to remain worst-case exponential, but spoiler: we can do much better in most reasonable cases!

However, the problem I wrestled with first is that it had no errors.

### Errors for a backtracking algorithm

Thereʼs some good lore here on what I believe errors should look like, but thatʼs for another post.

The backtracking algorithm essentially corresponds to a Boolean expression.
Thinking of it as `Applicative`+`Alternative`, we see that `<*>` corresponds to conjunctive `&&` and `<|>` corresponds to disjunctive `||`.

```
console >=5.0.0 <6.0.0

(console == 5.0.0 && prelude >=5.0.0 <6.0.0)
|| (console == 5.0.1 && prelude >=5.0.1 <6.0.0)
```

### Drawbacks of depth-first

With a depth-first algorithm in particular, .

I mean, you _can_ report the kinds of errors I mentioned above, but they are so confusing that you might as well just throw up your hands and say “I tried something and it didnʼt work.”
Like, thatʼs all the user would get from the errors anyways, since thatʼs really all the algorithm did.
It started with an essentially random version, committed to it immediately, tried other things as a consequence, and eventually reported that nothing worked.

So, since my goal was better errors, my next idea was to try to patch it to _run_ the depth-first backtracking algorithm, but create a post-mortem analysis to _report_ more sensible errors.
For example, from the Boolean algebra perspective, you can do basic tricks to factor out common sub-expressions.^[Foreshadowing …]

I couldnʼt bring myself to write that.
I just wrote a novel breadth-first algorithm.
I spent a significant chunk of time writing it.
I spent several weekends debugging its performance.
And the results are amazing.

## My breadth-first algorithm

Hereʼs where I admit my biggest weakness: prior art.
I have a great difficulty reading existing research on some topics.
Especially when the problem is so obviously begging for a nice solution like this!
Itʼs easier to work out the details for myself to be honest.
And then blog about it so that people who are _not_ like me learn what I have done.
(Apologies to those who are like me who will never read this and perhaps reinvent it. Godspeed.)

### Intuitive foundations: quasi-transitive dependencies

Recall what I said in the background: package _ranges_ donʼt have well-defined dependencies.
Oh but they often do in practice.

The idea is that we come up with quasi-transitive dependencies for a package range.

#### Composing relations

It looks like a familiar formula.

We can certainly think of it this way, as an approximation.
Maybe there is a way to categorify it directly, I donʼt know.

### Implementing it

The core backtracking algorithm actually still exists in the spine of the solver, but its role is greatly reduced.

#### Monoids, monoids everywhere

Again, a topic for another blog post, but I love monoids, semilattices in particular, because they capture information gathering in ways that are hard to get wrong.

#### Knowledge propagation

This kept me up for days.
It turns out I had done it wrong the first time, so it is good I thought it over again!

#### One simple trick for efficiency

It turns out that the algorithm is naturally efficient, with some help.

The biggest trick is using global constraints to discard redundant local constraints.
That is, if the manifest you are solving already constrains `prelude >=6.0.0 <7.0.0`, then each package that lists that requirement or a looser one can ignore it.

Unfortunately I had to add a bit of special casing in other places to handle this, in the propagation code in particular, but the exceptional efficiency is more than worth the slight inelegance.

### Errors

Room for improvement.
But decent off the bat.
And a clear direction for improvement, unlike depth-first algorithms.

######## Side thought
Itʼs interesting that nothing here required that dependencies are acyclic.
I actually made some tiny decisions that ensured that this would work, without causing an infinite loop for example, but it was minor things.
