---
title: The Best Errors for Solving Dependency Versions
subtitle: For the PureScript registry
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
date: 2023/01/02 – 2023/01/21
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
A [novel algorithm](https://github.com/purescript/registry-dev/blob/master/lib/src/Solver.purs) for resolving dependency bounds to solved versions:

- Incorporates [transitive dependency bounds](#intuitive-foundations-quasi-transitive-dependencies) for a breadth-first search:
  1. What dependencies are required no matter which package version in the range we commit to?
  2. Whatʼs the loosest bound for each dependency then?
- By taking this intuitive approach, we gain two things:
  1. [Better errors](#errors), matching what users would expect.
  2. Efficiency too, if you could believe it.
- Implemented using semilattices ([monoids](#monoids-monoids-everywhere)).
:::

(I know youʼre probably not going to read this whole long article and [Errors] is the very last section, but please feel free to skip ahead to that one since that was the whole point of this exercise!)

## Background

### The PureScript registry

The PureScript community has been designing a new registry to hold PureScript packages for some time now.
PureScript projects initially used [Bower](https://bower.io/) (a defunct npm competitor for Node.js packages), and I embarrassingly hung on to Bower until just last year.
Most of the community, however, has been using [Spago](https://github.com/purescript/spago), a PureScript build tool supporting package sets (fixed versions of packages that are known to be compatible).
Long story, but some core members have been designing a [new registry](https://github.com/purescript/registry) to house current and historical PureScript packages.
Weʼre very close to releasing it!^[No, for real this time!!]

In the interest of maintaining a healthy ecosystem, we want the new registry to support not just package sets but also traditional version solving.
And thatʼs where I came in.
Something about my mathy skills being a perfect fit for getting [nerd-sniped](https://xkcd.com/356/) by a version solving algorithm.
Oh and would you help fix the [versioning issues for legacy packages](https://github.com/purescript/registry-dev/pull/580) while youʼre at it?
Sure, sure I will.

### Version solving

The challenge of version solving in a package ecosystem is coming up with particular version of packages that satisfy not only the dependencies of the current project, but their own dependencies too.
You also want to ensure they are up-to-date by taking the latest possible versions – but sometimes those are not compatible with other declared dependencies.
The problem is expected to be difficult and slow to solve in general, but it is possible to optimize for what package dependencies look like in practice, and that is what I have done.

#### Details of versions and version ranges

Quick notes on conventions/terminology before we get too far in:

The actual details of how versions are tagged doesnʼt matter, just that they are totally ordered.^[Actually we probably donʼt even need a total order, a partial order would work fine for it?]
For example, it could just be flat integers for all we care.
But usually we take them to be lexicographically-ordered lists of integers, like `5.0.3`{.boo} which is less than `5.1.0`{.boo}.

How we form _ranges_ over versions is pretty important, though, and early on the registry decided to only allow half-open intervals.
That is, ranges have the form `>=A.B.C <X.Y.Z`{.boo}, which I will use throughout this article.
Again, it isnʼt very sensitive to details here (who cares that it is half-open?), but this does seem to be the right level of generality.
Supporting more complex queries is asking for trouble.

Finally, from a version-solving point of view, a registry contains information of what versions of packages there are, and for each package version a record of what dependencies it requires and the appropriate version ranges for those packages.
That is, it can be represented with the following PureScript datatype:

```purescript
-- A list of required dependencies
-- with their version ranges
type Manifest = Map PackageName Range

-- A list of all extant package versions
type RegistryIndex =
  Map PackageName
    (Map Version Manifest)
```

#### The problem statement

Solving means taking a manifest and finding versions for each package in it, preferring later versions^[This is actually a weird requirement, since there can be incomparable solutions that need to be tie-broken arbitrarily. In practice I do it alphabetically just by dint of how the package versions are tried.]:

```purescript
solve
  :: RegistryIndex
  -> Manifest
  -> Either SolverErrors
      (Map PackageName Version)
```

Along with some correctness constraints to ensure it is the solution we want.

<details class="Details" data-box-name="CSS">
<summary>Correctness constraints</summary>

```purescript
let r :: RegistryIndex
let m :: Manifest
let otherSol :: Map PackageName Version

-- We need the solution to solve the manifest and dependency's requirements
isASolutionFor r m (fromRight (solve r m)) &&
-- There are no strictly better versions to be found
( isASolutionFor r m otherSol
  `implies` isn'tWorseSolutionThan otherSol (fromRight (solve r m))
)
where
satisfies
  :: Map PackageName Version
  -> Map PackageName Range
  -> Boolean
satisfies sol m =
  allWithIndex
    ( \package range ->
        case Map.lookup package sol of
          Nothing -> false
          Just version -> range `includes` version
    )
    m

isASolutionFor
  :: RegistryIndex
  -> Manifest
  -> Map PackageName Version
  -> Boolean
isASolutionFor r m sol = and
  -- All packages received a version
  [ Map.keys m `isSubsetEqOf` Map.keys sol
  -- All solved versions fit into the range
  -- as required in the manifest
  , sol `satisfies` m
  -- All packages have their dependencies satisfied
  , allWithIndex
      ( \package version ->
          case Map.lookup package r >>= Map.lookup version of
            Nothing -> false
            Just deps ->
              sol `satisfies` deps
    )
    sol
  ]

isn'tWorseSolutionThan :: Map PackageName Version -> Map PackageName Version -> Boolean
isn'tWorseSolutionThan other optimal =
  Maps.keys optimal `isSubsetEqOf` Map.keys other
  && not allWithIndex
    ( \package version ->
        case Map.lookup package other of
          Nothing -> true

    )
    optimal
  -- FIXME
```
</details>

#### Dependencies are tricky

In particular, note that dependencies are associated with a particular _version_.
A package _range_ doesnʼt need to have well-defined dependencies at all!

This is something that we forget about when using packages in our day-to-day lives, but an algorithm needs to handle all cases we could throw at it.

## Depth-first backtracking algorithm

As I alluded to in the intro, I started off by copying [Elmʼs version solving algorithm](https://github.com/elm/compiler/blob/0.19.1/builder/src/Deps/Solver.hs).
Itʼs a very simple depth-first backtracking algorithm:

1. Try the latest compatible version of the package in front of you, based on the global requirements
2. Add its dependency ranges to the global requirements^[Since we chose a particular version in the previous step, its dependency ranges are well-defined, just being given in its manifest.]
3. Recursively see if the new global requirements can be solved
4. Backtrack to the next latest version at each failure.

Itʼs easy to see why this is worst-case exponential, and not going to hit fast cases particularly often.
In fact, we expect the problem to remain worst-case exponential, but spoiler: we can do much better in most reasonable cases!

Besides performance, the main obstacle I wrestled with was that it had no errors.
It turns out these are related concerns:
because the algorithm is so naïve, it isnʼt making use of available information to make smart choices, and this would reflect in the errors it could produce.

### Errors for a backtracking algorithm

I discovered that this problem of solving package versions corresponds well to what I have been thinking about in terms of compiler/typechecker errors for the past couple years.
So thereʼs some good lore here on what I believe errors should look like, but thatʼs for another post.

Basically, good errors should be a faithful reflection of the internal logic of the solver.
This is the main hint that performance and errors are linked:
if the solver is trying too many bad options, itʼs going to generate a ton of errors for all of those choices.
These errors are bad because they mainly reflect bad choices of the solver, not necessarily problems with the underlying data (the manifests).
Itʼs only once _every option_ has failed that you know that the underlying manifests were not compatible.
Our goal later, then, will be to reduce the number of choices to make and commit to errors as soon as possible.

The second problem with the errors is that the naïve backtracking does a *lot* of duplicate work, in between choices of packages.
In the worst case scenario, two package versions have the same manifests, so trying them separately will duplicate most of the work!^[
The only difference between two versions of the same package with the same manifests is that some later requirements may constrain that packageʼs range to eliminate one or the other.
]

It is possible to deduplicate errors after the fact, but those heuristics seem complex in general, and there are two problems still:

1. Youʼve already lost the performance associated with the duplicate work, and are spending more time trying to fix it
2. You might as well write the algorithm to incorporate the deduplication in the first place!!

There are some existing approaches to increase sharing/reduce duplicate work, in the context of general constraint solving and more particularly version solving with these type of bounds.
I briefly glanced at them, but they donʼt seem to address the heart of the issue like my algorithm does.

#### Algebraic errors

In a solver algorithm, we write programs in terms of some error monad.
The backtracking algorithm essentially corresponds to a complicated Boolean expression, a tree of various constraints joined with conjunction and disjunction.
Thinking of it as `Applicative`{.purescript}+`Alternative`{.purescript}, we see that `<*>`{.purescript} corresponds to conjunction `&&`{.purescript} and `<|>`{.purescript} corresponds to disjunction `||`{.purescript}.

```boo
console >=5.0.0 <6.0.0

(console == 5.0.0 && prelude >=5.0.0 <6.0.0)
|| (console == 5.0.1 && prelude >=5.0.1 <6.0.0)
```

An error, then, is some kind of proof that the Boolean always evaluates to false.
SAT solvers have done a great job of doing this in the general case.
And you can think a bit about what this means.

In addition to the literal Boolean clauses, we want the errors to record some [additional metadata](#provenance) about where they came from: particular manifests and the current dependency from the manifest we are trying to solve.

### Drawbacks of depth-first

However, we can only do so much: we remain limited to the logic of the algorithm.
With a depth-first algorithm in particular, the errors donʼt convey the global picture that the user is looking for.

I mean, you _can_ report these kinds of Boolean clause errors, but they are so confusing that you might as well just throw up your hands and say “I tried something and it didnʼt work.”
Thatʼs all the user would get from the errors anyways, since thatʼs really all the algorithm did:
It started with an essentially random package, committed to a version of it immediately, tried other things as a consequence, and eventually reported that nothing worked.

So, since my goal was better errors, [my next idea](https://github.com/purescript/registry-dev/pull/496#issuecomment-1225145757) was to try to patch it to _run_ the depth-first backtracking algorithm, but create a post-mortem analysis to _report_ more sensible errors.
For example, from the Boolean algebra perspective, you can do basic tricks to factor out common sub-expressions, which you can combine with what you know about comparing versions to ranges.^[Foreshadowing …]

I couldnʼt bring myself to write that.
So I just wrote a novel breadth-first algorithm.

I spent a significant chunk of time writing it.
I spent several weekends debugging its performance.

And the results are amazing.
_/me pats self on back_

## My breadth-first algorithm

Hereʼs where I admit my biggest weakness: prior art.
I have a great difficulty reading existing research on some topics.
Especially when the problem is so obviously begging for a nice solution like this!
Itʼs easier to work out the details for myself to be honest.
And then blog about it so that people who are _not_ like me learn what I have done.
(Apologies to those who are like me who will never read this and perhaps reinvent it. Godspeed.)

I spent a couple months designing a whole new algorithm from scratch.
The basic idea is that we gather as much information we can before committing to any versions.
This is done through the use of what I have coined as [quasi-transitive dependencies](#intuitive-foundations-quasi-transitive-dependencies).

:::{.Details box-name="Overview"}
The main steps are:

1. Load the slice of the registry index that we care about: package versions that are transitively reachable from the package ranges mentioned in the current manifest.
2. Gather information about _quasi-transitive_ dependencies for manifests in the registry as well as the current manifest we are solving, looping until there is no more obvious information to discover.
3. Check if the requirements have hit an error in the requirements already.
4. If not, check if we have solved it: do all the latest versions of requirements work as a solution?
5. Only as a last resort do we succumb to picking a package and recursively solving each of its versions, starting from the latest.

Note that the quasi-transitive dependencies check essentially commits to unique versions immediately, so by the time we reach step 5 we know that there are at least two possible versions of some dependency and are forced to commit to one to make progress.
It turns out that in practice, we already hit errors before we have to do that, so weʼve avoided the worst of the exponential blowup!

You can [read these steps in the code directly](https://github.com/purescript/registry-dev/blob/30a88ac7bd48a73bb2bcf9240b20b09a713ee0b9/lib/src/Solver.purs#L249-L289).
:::

### Intuitive foundations: quasi-transitive dependencies

Recall what I said in [Dependencies are tricky]: “A package _range_ doesnʼt need to have well-defined dependencies at all!”
Oh – but they often _do_ in practice.

If we can get extra knowledge about requirements before committing to any particular versions,
we have a chance at implementing some sort of breadth-first search.

How much extra knowledge we obtain depends on how packages treat their dependency bounds in the registry.
In the case of how PureScript packages tend to bound dependencies, it turns out to be a lot of knowledge.
This is because most stable PureScript libraries update with each breaking compiler release and depend on the corresponding major version range of `prelude` and various other core packages.
Since a lot of versions move in lockstep, it is pretty safe to assign loose dependencies to a package range and even reach for further transitive dependencies.

In general, when bumping minor and patch versions, packages tend to keep the same list of dependencies at similar version ranges.
Things are a bit more chaotic between major versions, but it is rarer that packages allowed different major versions in their manifests in the first place, and so there is some semblance of continuity.

Now we need to use this to our advantage:

:::Key_Idea
The idea is that we come up with _quasi-transitive dependencies_ for a package range – a lower bound of the absolutely necessary requirements that follow from a package _range_ being required.

There are two rules here:

1. If a package is not required by all versions in the range, we cannot say it is required overall.
2. When it _is_ depended on by all versions in a range, we take the loosest bounds we see: the lowest lower bound and the greatest upper bound.
:::

It turns out that we can formulate this rule as a [semigroup instance](https://pursuit.purescript.org/packages/purescript-functors/5.0.0/docs/Data.Functor.App#v:semigroupApp) that applies the logic for us to a collection of manifests:
```purescript
instance Semigroup (App (Map PackageName) Loose) where
  append (App m1) (App m2) = append <$> m1 <*> m2

foldMap1
  :: NonEmptyArray (App (Map PackageName) Loose)
  -> App (Map PackageName) Loose

instance Coercible Manifest (App (Map PackageName) Loose)
```

Note that this is in fact not a monoid: [`Map`{.purescript}](https://pursuit.purescript.org/packages/purescript-ordered-collections/docs/Data.Map#t:Map) only has an [`Apply`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Apply#t:Apply) instance (which gives the `<*>`{.purescript} operator to merge common keys), not [`Applicative`{.purescript}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Applicative#t:Applicative) (which would give `pure`{.purescript} but does not make sense for `Map`{.purescript} since it would have to contain _all_ possible keys!).

As a further optimization, while we are checking package versions, we may discard those that do not solve due to an obvious conflict.
This may seem strange: In the PureScript registry, each package will solve individually, we check that on upload.
But given the additional constraints of a particular manifest we are solving, we may end up with conflicts against various package versions that are incompatible with the global requirements, especially as we continue to aggregate quasi-transitive dependencies.

```purescript
-- | We record what dependency ranges are required no matter which version
-- | of the package we pick from the registry. That is, we report the loosest
-- | bounds when all packages report a bound for it. By filling in transitive
-- | dependencies on the registry itself, then, these bounds become more
-- | accurate.
-- |
-- | Also note that removing the redundant requirements via `addFrom` is safe
-- | with the assumptions here: if one local requirement is equal to or looser
-- | than a global requirement, then this result here would also be equal to or
-- | looser than the global requirement.
commonDependencies
  :: TransitivizedRegistry
  -> PackageName
  -> Intersection
  -> SemigroupMap PackageName Intersection
commonDependencies registry package range =
  let
    inRange =
      getPackageRange registry package range
    solvableInRange =
      Array.mapMaybe (traverse toLoose) (Array.fromFoldable inRange)
  in
    case NEA.fromArray solvableInRange of
      Nothing -> mempty
      Just versionDependencies ->
        case NEA.foldMap1 App (un SemigroupMap <$> versionDependencies) of
          App reqs ->
            SemigroupMap $ reqs <#> asDependencyOf range <<< fromLoose
```

#### Composing relations

This quasi-transitive dependency business looks a bit like a familiar formula: the composition of two relations in logic.

:::Details
Phrased in terms of set theory, [Wikipedia says](https://en.wikipedia.org/wiki/Composition_of_relations#Definition):

> If \(R \subseteq X \times Y\) and \(S \subset Y \times Z\) are two binary relations, then their composition \(R;S\) . . . is defined by the rule that says \((x,z)\in R;S\) if and only if there is an element \(y\in Y\) such that \(x\,R\,y\,S\,z\) (that is, \((x,y)\in R\) and \((y,z)\in S\)).
:::

The key part here is that we take our input and our output and we ask: is there something _in the middle_ that serves to connect the input to the output?
(Thinking of relations as boxes that connect certain inputs to certain outputs.)

However, we arenʼt dealing with general relations here, weʼre only dealing with half-open intervals.
Weʼre asking: for a version _range_, what _range_ is constructed by taking the ranges of _each version_ in the middle?

To be a bit more direct with this analogy, a relation \(R \subseteq X \times Y\) can equivalently be written as \(R \in \mathcal{P}(X \times Y)\).
(\(\mathcal{P}(Z)\) here is the powerset monad \(\mathcal{P}(Z) = Z \to \textrm{Prop}\), which consists of all subsets of the given set \(Z\).)
And by currying, this can be viewed as \(R \in X \to \mathcal{P}(Y)\).
This construction \(X \to M(Y)\) for a monad \(M\) is called the Kleisli category.
So now the question is: do intervals also form a monad, by taking loose bounds?

The easy answer is that we can certainly think of it as an approximation on top of the underlying set-relation model.
That is, we know how to make intervallic dependencies a relation, so we compose them as relations and then take the smallest interval that contains every interval we came across.

Perhaps there is a way to categorify it directly, I donʼt know.
We can come up with an identity, but Iʼm not so sure that associativity would hold.

:::{.Details box-name="Clarification"}
To see how it fits.
Unit -> Package X range -> Package Y range
(X depends on Y)

Thatʼs only dealing with versions of a single package.
Bundle it together.
:::

### Implementing it

The core backtracking algorithm actually still exists in the spine of the solver, but its role is greatly reduced.
In fact, this has a funny implication for testing the algorithm:
_the correctness is visible not in finding the right solutions but in the algorithmʼs efficiency and errors._

The literal results of the solver were accurate all along.
But when I finally got it working _fast_, I knew all my logic was in place for all the intermediate steps.
In particular, this means that we preempted most of the (exponential) backtracking.

#### Monoids, monoids everywhere

Again, a topic for another blog post, but I love monoids, especially semilattices, because they capture information gathering in ways that lend themselves to reliable implementation.

In particular, because of their idempotence, semilattices are great because you just need to make sure you cover all cases.
Thereʼs no such thing as double-counting in a semilattice computation!
When youʼre dealing with a well-behaved logical scenario, if have written your logic correctly ([i.e.]{t=} each derivation is valid) and you cover all the cases (you eventually produce every fact you are allowed to derive), thereʼs no chance that you accidentally make things break.^[
If the logical scenario does not have a finite upper bound of information to derive, this naïve process may not terminate, but in our case it is certainly finite: the registry itself is finite, so any logical derivations from it will eventually be saturated.]

We already saw our first semilattice `Semigroup (App (Map PackageName) Loose)`{.purescript} above.
However, I left out the definition of `Loose`{.purescript} and its `Semigroup`{.purescript} instance.

The _data_ contained in `Loose`{.purescript} is just a lower bound and an upper bound, and we want the lower bound to be less than the upper bound for it to be valid.
We also pack in _metadata_ that describes where each bound came from, the `SolverPosition`{.purescript} datatype which we will discuss below in [Provenance].

To achieve this, we first define a type that describes a bound with metadata packed in.
Then we add to this operations that take the maximum and minimum of the bounds, and _aggregate_ the metadata if they were the same bound.
Thatʼs right, **the metadata itself forms a semilattice!**^[I cannot emphasize how key this is to a lot of the work of carrying around metadata by bundling it in with data like this.]
```purescript
data Sourced = Sourced Version SolverPosition

newtype MinSourced = MinSourced Sourced

instance Semigroup MinSourced where
  append a@(MinSourced (Sourced av as)) b@(MinSourced (Sourced bv bs)) =
    case compare av bv of
      LT -> a
      GT -> b
      EQ -> MinSourced (Sourced av (as <> bs))

newtype MaxSourced = MaxSourced Sourced

instance Semigroup MaxSourced where
  append a@(MaxSourced (Sourced av as)) b@(MaxSourced (Sourced bv bs)) =
    case compare av bv of
      GT -> a
      LT -> b
      EQ -> MaxSourced (Sourced av (as <> bs))
```

Now we get both `Loose`{.purescript} and `Intersection`{.purescript} for free by the right arrangement of these types.
Heck, we even get their coercion for free:
```purescript
newtype Loose = Loose
  { lower :: MinSourced
  , upper :: MaxSourced
  }
derive newtype instance Semigroup Loose

newtype Intersection = Intersection
  { lower :: MaxSourced
  , upper :: MinSourced
  }

derive newtype instance Semigroup Intersection

-- API for `Intersection`
upperBound :: Intersection -> Version
upperBound (Intersection { upper: MinSourced (Sourced v _) }) = v

lowerBound :: Intersection -> Version
lowerBound (Intersection { lower: MaxSourced (Sourced v _) }) = v

good :: Intersection -> Boolean
good i = lowerBound i < upperBound i

satisfies
  :: Version -> Intersection -> Boolean
satisfies v r = v >= lowerBound r && v < upperBound r

-- `Loose` has to be a valid interval
toLoose :: Intersection -> Maybe Loose
toLoose i | good i = Just (coerce i)
toLoose _ = Nothing

fromLoose :: Loose -> Intersection
fromLoose = coerce
```

Why donʼt we require `Intersection`{.purescript} to be a valid interval?
As we will talk about in the next section, `Intersection`{.purescript} is the primary way we keep track of the knowledge we have learned already.
Being in the business of aggregating information, we want to know all we can about the situation our solver is confronted with, and we just can accumulate knowledge by throwing it into this semilattice.

We could make taking the intersection of intervals a partially-defined operation (`Intersection -> Intersection -> Either Error Intersection`{.purescript}), but that means we have to bail out once a single intersection becomes invalid.
Instead, we integrate them directly into the semilattice structure by keeping invalid intervals around and turning them into [errors] later (this is why we give them the metadata about [provenance]!).
This gives us multiple errors emerging from one step for free, it is incredibly convenient.

#### Knowledge propagation

Figuring out the correct way to propagate known requirements kept me occupied for days.
It turns out I had done it wrong the first time, so it is good I thought it over again!

Our goal is to implement `solveStep`{.purescript} here using `commonDependencies`{.purescript} (see [above](#intuitive-foundations-quasi-transitive-dependencies)) and `exploreTransitiveDependencies`{.purescript}:
```purescript
-- Semilattice version of `Registry`
type TransitivizedRegistry =
  SemigroupMap PackageName
    (SemigroupMap Version
      (SemigroupMap PackageName Intersection)
    )

type RRU =
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  , updated :: TransitivizedRegistry
  }

-- | Discover one step of quasi transitive dependencies, for known requirements
-- | and the rest of the registry too.
solveStep :: RRU -> RRU

-- Key piece:
exploreTransitiveDependencies :: RRU -> RRU
```

The `registry :: TransitivizedRegistry`{.purescript} and `required :: SemigroupMap PackageName Intersection`{.purescript} represent the local dependencies for each package version and the global requirements of the initial manifest given to the solver, respectively.
They both are purely accumulative: what goes in comes out with some more information.
The additional information will simply be added dependencies and tightened bounds on existing dependencies.
Provenance metadata may accumulate too (we donʼt really need to care about that, it is just along for the ride).

The other field, `updated :: TransitivizedRegistry`{.purescript}, is a bit different: it does not carry over from step to step, it only talks about what changed at the last step.
This is because as weʼre keeping `registry :: TransitivizedRegistry`{.purescript} updated, we want to only calculate updates to the things that might need it.

When we first call `solveStep`{.purescript}, we treat everything as updated:
```purescript
solveSeed :: RR () -> RRU
solveSeed { registry, required } = { registry, required, updated: registry }
```
and the process stabilizes when there are no updates:
```purescript
-- | Add quasi transitive dependencies until it stabilizes (no more updates).
-- | Needs to know what was updated since it last ran.
solveSteps :: RRU -> RR ()
solveSteps r0 = go r0
  where
  go r@{ registry, required } | noUpdates r = { registry, required }
  go r = go (solveStep r)
```

Keeping track of what was updated is certainly the trickiest part of the whole algorithm to reason about, but there is this one nugget of insight that coalesced into the knowledge I needed to turn it into an algorithm:

:::Key_Idea
The manifests for package versions might need to update when some of their dependencies update.
However, not all updates need to propagate like this from dependencies to their reverse dependencies.

In particular, in the case that a manifest is updating because its dependencies tightened, _if_ this could affect its reverse dependencies they should _already_ be depending on the transitive dependencies directly and updating because of it.
This leaves us with the only major updates being because a dependency was _added_, which the parent did not know about yet so it needs to rescan its dependencies to potentially add the dependency itself.

The other case is that if a package version picks up an obvious failure, its reverse dependencies need to be notified.
They may pick up a quasi-transitive dependency once this failing package version is dropped, if it was missing that particular dependency but others had it.
:::

```purescript
-- | A package may update because its dependencies tightened, but any reverse
-- | dependencies should have already caught that update in this same tick.
-- | So what we look for is either a new transitive dependency picked up (which
-- | the parent will need to incorporate), or newly failing to solve,
-- | both of which may introduce new dependencies for reverse dependencies
-- | through the `commonDependencies` calculation.
majorUpdate :: SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> Boolean
majorUpdate (SemigroupMap required) (SemigroupMap orig) updated =
  let
    minor = { added: false, failedAlready: false, failedNow: false }

    info :: { added :: Boolean, failedNow :: Boolean, failedAlready :: Boolean }
    info = updated # anyWithIndex \package range ->
      case Map.lookup package orig of
        Nothing ->
          -- This bound may have been omitted merely because it was subsumed by
          -- a global requirement (see `addFrom`), so adding it back does not
          -- count as a major update:
          case Map.lookup package required of
            Nothing -> minor { added = true }
            Just range' -> minor { added = lowerBound range > lowerBound range' || upperBound range < upperBound range' }
        Just r -> minor { failedAlready = not good r, failedNow = not good range }
  in
    case info of
      { added: true } -> true
      { failedNow: true, failedAlready: false } -> true
      _ -> false

-- | Update package versions in the registry with their quasi-transitive
-- | dependencies, if their dependencies were updated in the last tick. The set
-- | global requirements is needed here because those are elided from the
-- | dependencies in each package version, so to tell how the local requirements
-- | updated we need need to peek at that (see `majorUpdate`).
exploreTransitiveDependencies :: RRU -> RRU
exploreTransitiveDependencies lastTick = (\t -> { required: lastTick.required, updated: accumulated (fst t), registry: snd t }) $
  lastTick.registry # traverseWithIndex \package -> traverseWithIndex \version deps ->
    let
      updateOne depName depRange = case Map.isEmpty (unwrap (getPackageRange lastTick.updated depName depRange)) of
        true -> mempty
        false -> Tuple (Disj true) (commonDependencies lastTick.registry depName depRange)
      Tuple (Disj peek) newDeps = foldMapWithIndex updateOne deps
      -- keep GC churn down by re-using old deps if nothing changed, maybe?
      dependencies = if peek then deps <> newDeps else deps
      updated = case peek && majorUpdate lastTick.required deps dependencies of
        true -> doubleton package version dependencies
        false -> mempty
    in
      Tuple updated dependencies

-- | Discover one step of quasi transitive dependencies, for known requirements
-- | and the rest of the registry too.
solveStep :: RRU -> RRU
solveStep initial =
  { required: initial.required <> moreRequired
  , registry: moreRegistry
  , updated: updated <> updatedOfReqs
  }
  where
  -- Transitivize direct requirements
  moreRequired = initial.required # foldMapWithIndex (commonDependencies initial.registry)
  -- Record updates to them
  updatedOfReqs = requirementUpdates initial moreRequired
  -- Transitivize the rest of the registry, which should be:
  --   (1) Pruned at the start to only reachable package versions
  --   (2) Only touching packages that were directly updated last round
  { updated, registry: moreRegistry } = exploreTransitiveDependencies (initial { registry = map (addFrom moreRequired) <$> initial.registry })
```


#### One simple trick for efficiency

It turns out that the algorithm is naturally efficient, with some help.

The biggest trick is _using global constraints to discard redundant local constraints_.
That is, if the manifest you are solving already constrains `prelude >=6.0.0 <7.0.0`{.boo}, then each package that lists that requirement or a looser one can ignore it.

```purescript
-- | The key to efficiency: take information from the bounds of global
-- | requirements and add it to the local requirements of each package version
-- | in the registry, BUT remove redundant bounds as we do so.
-- |
-- | For example, if we have a global requirement `>=3.1.0 <4.0.0`, then in the
-- | registry we will keep local dependency ranges for the same package that
-- | look like `>=3.2.0 <4.0.0` or `>=3.1.0 <3.9.0` and remove ranges like
-- | `>=3.0.0 <4.0.0` or `>=3.1.0 <4.0.0` itself.
addFrom
  :: SemigroupMap PackageName Intersection
  -> SemigroupMap PackageName Intersection
  -> SemigroupMap PackageName Intersection
addFrom (SemigroupMap required) =
  over SemigroupMap $ Map.mapMaybeWithKey \package ->
    case Map.lookup package required of
      Nothing -> Just
      Just i -> \j ->
        if j `wouldUpdate` i then Just (j <> i)
        else Nothing

-- | Used in `addFrom, `wouldUpdate j i` is an optimized version of
-- | `(i <> j /= i)`.
wouldUpdate :: Intersection -> Intersection -> Boolean
wouldUpdate j i =
  lowerBound j > lowerBound i ||
  upperBound j < upperBound i
```

Unfortunately I had to add a bit of special casing in the propagation to handle this, in particular [when checking for major updates](https://github.com/purescript/registry-dev/blob/30a88ac7bd48a73bb2bcf9240b20b09a713ee0b9/lib/src/Solver.purs#L493-L495), but the exceptional efficiency is more than worth the slight inelegance.

##### Oh but I spent so much time getting here

I almost made a profiling analysis library.
JavaScript performance testing is useless because it gets washed away in a sea of lambdas, and I couldnʼt find/make a tool to aggregate the lambda information into their named parents.
Wrap particular segments in profiling.

I also needed a histogram viewer.

Lots of micro optimizations.

- Using a specific order of `<>`{.purescript}, since `Map`{.purescript} appends are implemented as a fold over the second argument so it should be the smaller argument.
- Using a difflist (Cayley) representation when I know Iʼm only appending one key at a time but with mixed associativity.
- Implementing `wouldUpdate`{.purescript} directly instead of using the semigroup operation.
- Optimizing the `Ord Version`{.purescript} instance since it is the most common operation in this whole thing.

Did they make a difference?
I donʼt know!
They appeared to make incremental difference as I was testing it, but once I did the big optimization above I gave up on testing that.

### Errors

Room for improvement.
But decent off the bat.
And a clear direction for improvement, unlike depth-first algorithms.

Conflicts.
(Conflict “clauses.”)
The problem with backtracking was that the errors .
Particular clauses could conflict, sure, but then you had to work out why that made the whole boolean expression fail, and what that corresponds to in the version solving model.

In the new model, since we just keep adding requirements at each step to tighten bounds, the basic form of conflict is really simple: a required upper bound got pushed below a required lower bound.
Or, we could have restricted to a range that has no registered versions.^[It turns out these are the [same check](https://github.com/purescript/registry-dev/blob/30a88ac7bd48a73bb2bcf9240b20b09a713ee0b9/lib/src/Solver.purs#L308-L311).]

There are two ways we combine these errors within the logic of the solver:

1. First we note that we may encounter errors in multiple requirements at the same time, so we keep a (non-empty) map of package names to their conflicts.
  (Reporting multiple errors at once is very helpful!)
2. Second it may be the case that a package has versions in range, but we happen to know that none of them are still solvable, they all have conflicts of their own.
  (We actually just do a [very shallow check of this](https://github.com/purescript/registry-dev/blob/30a88ac7bd48a73bb2bcf9240b20b09a713ee0b9/lib/src/Solver.purs#L300-L304).)

This gets us [this data type for errors](https://github.com/purescript/registry-dev/blob/30a88ac7bd48a73bb2bcf9240b20b09a713ee0b9/lib/src/Solver.purs#L149-L151):
```purescript
data SolverError
  = Conflicts (Map PackageName Intersection)
  | WhileSolving PackageName (Map Version SolverError)
```

This isnʼt completely faithful to the logic of the solver.
You have to trust that the system determined these are required: it wonʼt tell you exactly what decisions led to it requiring it.

But it does keep around provenance information that tells you enough about where it originated.

#### Provenance

Normally I like to keep full provenance to detail exactly the path a piece of data took to get through the logic.^[
In particular, in dependently-typed languages it is really helpful to be able to trace terms through their evaluation, so that by the time you get to a type error you know exactly why those particular things popped up, not just a rough idea of where in the source they were originally found once upon a time.
Especially because once separate terms are unified, you donʼt want to arbitrarily pick a location, you want to know both locations!
]
However, it is really slick in this domain: we only need to keep track of the endpoints, users donʼt exactly care about what came in between (just that it is reasonable to assume, because it is in fact correct).

So in this case I keep track of which particular package version manifest(s) gave us the constraint we are talking about (`LocalSolverPosition`{.purescript}), and which constraints in the current manifest caused it to be required.
Thereʼs some logic to combine these positions which I will not reproduce here.

```purescript
data LocalSolverPosition
  -- | Dependency asked for in manifest
  = Root
  -- | Committed to a specific version
  | Trial
  -- | Required transitive dependency seen in said packages
  | Solving
      ( NonEmptySet
          { package :: PackageName
          , version :: Version
          }
      )

data SolverPosition = Pos LocalSolverPosition (Set PackageName)
```

It seems that it weakens the logical connection just a bit, I donʼt know if they can be put into formal properties anymore.
([E.g.]{t=} “Deleting the mentioned constraint from the current manifest affects it in _this_ way.”)

But I believe it is the information that users want to see; it certainly falls into the category of making it actionable so they can fix things and run it again to make progress.
In the case that it is a local error, knowing which clauses of the current manifest led to it is crucial in answering the question, “What do I need to change to fix the error”.
And sometimes it is a deeper error of outdated dependencies, so you want to know what package is responsible for that incongruous version requirement.

:::{.Bonus box-name="Side thought"}
Itʼs interesting that nothing here required that dependencies are acyclic.
I actually made some tiny decisions that ensured that this would work, without causing an infinite loop for example, but it was minor things.
:::
