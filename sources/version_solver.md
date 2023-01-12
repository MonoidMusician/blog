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
A novel algorithm for resolving dependency bounds to solvable versions:

- Incorporates transitive dependency bounds for a breadth-first search:
  1. What dependencies are required no matter which package version in the range we commit to?
  2. Whatʼs the loosest bound for that dependency then?
- By taking this intuitive approach, we gain two things:
  1. Better errors, matching what users would expect.
  2. Efficiency too, if you could believe it.
- Implemented using semilattices (monoids).
:::

## Background

### The PureScript registry

The PureScript community has been designing a new registry to hold PureScript packages for some time now.
PureScript projects initially used [Bower](https://bower.io/) (a defunct npm competitor for Node.js packages), and I embarrassingly hung on to Bower until very recently.
Most of the community, however, has been using [Spago](https://github.com/purescript/spago), a PureScript build tool supporting package sets (fixed versions of packages that are known to be compatible).
Long story, but some core members have been designing a new registry to house current and historical PureScript packages.
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
```haskell
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
```haskell
solve
  :: RegistryIndex
  -> Manifest
  -> Either SolverErrors
      (Map PackageName Version)
```

Along with some correctness constraints to ensure it is the solution we want.

<details class="Details" data-box-name="CSS">
<summary>Correctness constraints</summary>
```haskell
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
Thinking of it as `Applicative`{.haskell}+`Alternative`{.haskell}, we see that `<*>`{.haskell} corresponds to conjunction `&&`{.haskell} and `<|>`{.haskell} corresponds to disjunction `||`{.haskell}.

```boo
console >=5.0.0 <6.0.0

(console == 5.0.0 && prelude >=5.0.0 <6.0.0)
|| (console == 5.0.1 && prelude >=5.0.1 <6.0.0)
```

An error, then, is some kind of proof that the Boolean always evaluates to false.
SAT solvers have done a great job of doing this in the general case.
And you can think a bit about what this means.

In addition to the literal Boolean clauses, we want the errors to record some additional metboota about where they came from: particular manifests and the current manifest we are trying to solve.

### Drawbacks of depth-first

However, we can only do so much: we remain limited to the logic of the algorithm.
With a depth-first algorithm in particular, the errors donʼt convey the global picture that the user is looking for.

I mean, you _can_ report these kinds of Boolean clause errors, but they are so confusing that you might as well just throw up your hands and say “I tried something and it didnʼt work.”
Thatʼs all the user would get from the errors anyways, since thatʼs really all the algorithm did:
It started with an essentially random package, committed to a version of it immediately, tried other things as a consequence, and eventually reported that nothing worked.

So, since my goal was better errors, [my next idea](https://github.com/purescript/registry-dev/pull/496#issuecomment-1225145757) was to try to patch it to _run_ the depth-first backtracking algorithm, but create a post-mortem analysis to _report_ more sensible errors.
For example, from the Boolean algebra perspective, you can do basic tricks to factor out common sub-expressions.^[Foreshadowing …]

I couldnʼt bring myself to write that.
So I just wrote a novel breadth-first algorithm.

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

Recall what I said in [dependencies are tricky]: “A package _range_ doesnʼt need to have well-defined dependencies at all!”
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
```haskell
instance Semigroup (App (Map PackageName) Loose) where
  append (App m1) (App m2) = append <$> m1 <*> m2

foldMap1
  :: NonEmptyArray (App (Map PackageName) Loose)
  -> App (Map PackageName) Loose

instance Coercible Manifest (App (Map PackageName) Loose)
```

Note that this is in fact not a monoid: [`Map`{.haskell}](https://pursuit.purescript.org/packages/purescript-ordered-collections/docs/Data.Map#t:Map) only has an [`Apply`{.haskell}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Apply#t:Apply) instance (which gives the `<*>`{.haskell} operator to merge common keys), not [`Applicative`{.haskell}](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Applicative#t:Applicative) (which would give `pure`{.haskell} but does not make sense for `Map`{.haskell} since it would have to contain _all_ keys!).

As a further optimization, while we are checking package versions, we may discard those that do not solve due to an obvious conflict.
This may seem strange: In the PureScript registry, each package will solve individually, we check that on upload.
But given the additional constraints of a particular manifest we are solving, we may end up with conflicts against various package versions that are incompatible with the global requirements, especially as we continue to aggregate quasi-transitive dependencies.

```haskell
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

It looks like a familiar formula: the composition of two relations in logic.
Phrased in set theory, [Wikipedia says](https://en.wikipedia.org/wiki/Composition_of_relations#Definition):

> If \(R \subseteq X \times Y\) and \(S \subset Y \times Z\) are two binary relations, then their composition \(R;S\) is the relation
> \[ R;S=\{(x,z)\in X\times Z:{\text{ there exists }}y\in Y{\text{ such that }}(x,y)\in R{\text{ and }}(y,z)\in S\}. \]
>
> In other words, \(R;S\subseteq X\times Z\) is defined by the rule that says \((x,z)\in R;S\) if and only if there is an element \(y\in Y\) such that \(x\,R\,y\,S\,z\) (that is, \((x,y)\in R\) and \((y,z)\in S\)).

The key part here is that we take our input and our output and we ask: is there something _in the middle_ that serves to connect the input to the output?

However, we arenʼt dealing with generic relations here, weʼre only dealing with half-open intervals.

We can certainly think of it as an approximation.
That is, we know how to make intervallic dependencies a relation, so we compose them as relations and then take the largest interval that fits.
Maybe there is a way to categorify it directly, I donʼt know.

### Implementing it

The core backtracking algorithm actually still exists in the spine of the solver, but its role is greatly reduced.
In fact, this has a funny implication for testing the algorithm:
_the correctness is visible not in solutions of the algorithm but in its speed and errors._

The literal results of the solver were accurate all along.
But when I finally got it working _fast_, I knew all my logic was in place for all the intermediate steps.
In particular, this means that we preempted most of the (exponential) backtracking.

#### Monoids, monoids everywhere

Again, a topic for another blog post, but I love monoids, semilattices in particular, because they capture information gathering in ways that lend themselves to reliable implementation.

In particular, because of their idempotence, semilattices are great because you just need to make sure you cover all cases.
If youʼre dealing with a well-behaved logical scenario, and have written your logic correctly, thereʼs no chance that you accidentally make things break.
Thereʼs no such thing as too much where semilattices are involved!

#### Knowledge propagation

Figuring out the correct way to propagate known requirements kept me occupied for days.
It turns out I had done it wrong the first time, so it is good I thought it over again!

As weʼre keeping our `TransitivizedRegistry`{.haskell} updated, we want to only calculate updates to the things that might need it.
Itʼs certainly the trickiest part of the whole algorithm to reason about, but there is this one nugget of insight that coalesced into the knowledge I needed to turn it into an algorithm:

:::Key_Idea
The manifests for package versions might need to update when some of their dependencies update.
However, not all updates need to propagate like this from dependencies to their reverse dependencies.
In particular, if a manifest is updating because its dependencies tightened, _if_ this will significantly affect its reverse dependencies they should _already_ be depending on the transitive dependencies directly and updating because of it.
This leaves us with the only major updates being because a dependency was _added_, which the parent did not know about yet so it needs to rescan its dependencies.

The other case is that if a package version picks up an obvious failure, its reverse dependencies need to be notified.
They may pick up a quasi-transitive dependency once this failing package version is dropped, if it was missing that particular dependency but others had it.
:::

```haskell
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
```


#### One simple trick for efficiency

It turns out that the algorithm is naturally efficient, with some help.

The biggest trick is using global constraints to discard redundant local constraints.
That is, if the manifest you are solving already constrains `prelude >=6.0.0 <7.0.0`{.boo}, then each package that lists that requirement or a looser one can ignore it.

```haskell
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

### Errors

Room for improvement.
But decent off the bat.
And a clear direction for improvement, unlike depth-first algorithms.

Conflicts.
(Conflict “clauses.”)

You have to trust that the system determined these are required: it wonʼt tell you exactly what decisions led to it requiring it.

But it does keep around provenance information that tells you enough about where it originated.

#### Provenance

Normally I like to keep full provenance.
However, it is really slick in this domain: we only need to keep track of the endpoints.
In particular, I keep track of which particular package version manifest(s) gave us the constraint we are talking about, and which constraints in the current manifest caused it to be required.

It seems that it weakens the logical connection just a bit, I donʼt know if they can be put into formal properties anymore.
(E.g. “Deleting the mentioned constraint from the current manifest affects it in _this_ way.”)

But I believe it is the information that users want to see, and certainly falls into the category of making it actionable so they can run it again to make progress.
In particular, knowing which clauses of the current manifest led to it is crucial in answering the question, “What do I need to change to fix the error”.

######## Side thought
Itʼs interesting that nothing here required that dependencies are acyclic.
I actually made some tiny decisions that ensured that this would work, without causing an infinite loop for example, but it was minor things.
