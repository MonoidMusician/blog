Sketch of what a good stdlib might look like

https://blog.functorial.com/posts/2015-12-06-Counterexamples.html
https://blog.functorial.com/posts/2021-10-29-Counterexamples-Interactive.html

somewhere in between Haskell and Agda... with some modernizations

need to seamlessly handle:

- lawful vs lawless
- having identities split from operations is nice
  - not just `Semigroup` and `Monoid`, but also `Pointed` I guess
  - there is a paucity of identities, e.g. in `Set`, every monoidal tensor has an identity of `0` or `1` (empty or singleton types/sets)
- combinatorial explosion
  - somewhat mitigatable for MTL, i think
- deep inheritance
  - e.g. just define `Field`, and you get the _entire_ algebraic hierarchy for free: `Ring`, `Semiring`, etc. etc. etc.
- newtyping
  - define the classes you want to change, and inherit everything else

categories and lattices

# Algebraic typeclasses

- zero operators: `Default`. just cuz. probably should agree with `mempty` where possible
- one operator:
  - `Semigroup`, `Monoid`, `Group`, `Semilattice`, `BoundedSemilattice`
    - additive by default, so disjunctive (join/supremum), I guess
    - separate typeclasses for identities, because they sometimes/often require fewer constraints?
- two operators:
  - numeric: `NonUnitalNearSemiring`, `NearSemiring`, `NonUnitalSemiring`, `Semiring`, …, `Ring`, …, `Meadow`, `Field`
    - I guess `.+.`, `.*.`, `.^.` for saturating arithmetic, `.-.` for monus, and `./.` for floored div? `.%.` for mod?
    - maybe `.<.`, `.>.` for bitwise implications or something
  - logical: `Lattice`, `BoundedLattice`, `DistributiveLattice`, ..., `HeytingAlgebra`, `BooleanAlgebra`
    - Bitwise operations, `.|.`, `.&.` (even when not fixed width), and bitwise negation and top element when fixed width
    - Same carrier type and laws, but intention matters!
    - `(.|.) :: SemiBooleanAlgebra (Bitwise carrier) => carrier -> carrier -> carrier`
- vector-like:
  - modules over semirings, rings, fields, ...
  - linear and affine transformations
    - `around basepoint` somehow transforms linear transformations into affine transformations?
  - evaluation/manipulation of polynomials vs Béziers uses different constraints
  - Lie algebras?
- less standard
  - circular orders, projective spaces, 

# Functor typeclasses

technically every functor thing should implement Invariant`

- Variance of data types:
  - The strongest is `x = y -> f x -> f y`, but this is ... rare. only caused by type equality itself and nominal roles. but every type function *does* satisfy this
  - The next strongest is `Coercible x y => f x -> f y`, which basically everything should implement, really!
    We also specifically want it as a superclass of `Functor` and so on
  - Next we get functors over bijections `Iso x y => f x y` (this does not really exist in Haskell)
  - Then over `(x <-> y) -> f x -> f y`, this is `Invariant`
    - Most often it can be decomposed into a `Profunctor`, with covariant and 
  - Then we leave the world of groupoids and consider the directed versions:
    - `Functor`: `(x -> y) -> f x -> f y` (“covariant”)
    - or `Contravariant`: `(y -> x) -> f x -> f y` (“contravariant”)
    - but when the categories are not closely related, there may be no real convention for variance
  - Ideally it would be a typeclass for a type constructor that describes each of its args (what if dependent types? I guess telescoped) with its variance, then implements the correct mapping function, and maybe a wrapper to phrase it in a CT way
- Control:
  - `Functor`, `Pointed`, `Apply`, `Applicative = Apply && Pointed`, `Selective`, `Bind`, `Monad`
    - these all require coherent laws like in Haskell, but there will be a way to make a “single” type with parallel and sequential instances and so on
  - `Empty`, `Alt`, `Plus`, …, `Alternative`
    - technically this can come in parallel and sequential flavors too, e.g. if you're a parser lifted over `IO` and you want to race two sides
  - `Extract`, `Extend`, `Comonad`
  - `Representable` (`f a` isomorphic to `(i -> a)`), thus `Distributive`
    - for other categories these can be more interested
- Data:
  - `FunctorWithIndex`, somehow incorporated into all of these
    - `Foldable f where foldMapWithIndex :: FunctorWithIndex i f => (i -> a -> m) -> f a -> m`, where you can refute it (due to the orphan instance policy), or maybe if you implement `FunctorWithIndex` later it makes you fill it in there to provide for `Foldable` too
  - single: `Foldable1`, `Foldable`, `Traversable1`, …, `Traversable`, …, `Witherable` (no `Witherable1`), `Crosswalk`
    - breadth-first traversals via [applicative phases](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/phases.pdf)?
  - double: `Align`: `(f x, f y) -> f (These x y)`, `SemiUnify`: `(f x, f y) -> Maybe (f (These x y))`, `Unify`: `(f x, f y) -> Maybe (f (x, y))`, `Zip`, `Repeat`; `Unalign` (map-like), `Unzip` (only for efficiency)
    - I guess `Unify => Align` via `Both`, and `Align => SemiUnify` via `Just`, so `Unify => SemiUnify`
    - however the identity for `Align` does not make sense for `Unify`
    - `Zip` and `Unify` have different semantics (common prefix vs totally matching), but representable functors will implement both (constant shape)
    - `Unify` gives a basic `diff :: (f x, f y) -> Diff f x y` for `data Diff f x y = Similar (f (x, y)) | Different (f x) (f y)` which lends itself to information-preserving recursive diffs (`Diff f x y -> (f x, f y)`), although the exact diff format/output should be able to be tuned ... especially useful for ASTs, viewed via recursion schemes over pattern functors
      - `DiffList x y = NilNil | ConsNil x | NilCons y | ConsCons x y (DiffList x y)`, more restrictive than `List (These x y)`, but does not account for middle insertions
      - whereas `DiffMap k x y = Map k (These x y)` because there is no requirement of contiguousness
      - https://pursuit.purescript.org/packages/purescript-incremental-functions/2.0.0/docs/Data.Incremental
    - `Confoldable f => Foldable f` with `confold :: Monoid m => (These x y -> m) -> f x -> f y -> m`

monoidal tensors, list-like data types, map-like data types, AST-like data types, free constructions

## Monad typeclasses

Transformers: `ReaderT`, `WriterT`, `StateT`, `ExceptT`, `LogicT`

Mostly their MTL classes express that there are morphisms `m ~> ExceptT r m`, `ExceptT r m ~> m`.
But you do want the procedural style sometimes (`logThis`, `connectDb`, etc.).

`MonadBracket`/[`MonadMask`](https://hackage.haskell.org/package/safe-exceptions-0.1.7.4/docs/Control-Exception-Safe.html#t:MonadMask)

> A class for monads which provide for the ability to account for all possible exit points from a computation, and to mask asynchronous exceptions. Continuation-based monads are invalid instances of this class.

[`CheckpointMonad`](https://hackage.haskell.org/package/hoopl-3.10.2.2/docs/Compiler-Hoopl.html#t:CheckpointMonad)/rollback (records writes to revert them, but only while scoped... could bracket, have explicit drop, and/or use weak refs maybe?)

`IO`/`ST`/`Effect` plus niceties: resource management, [early exit](https://oleg.fi/gists/posts/2024-03-17-st-with-early-exit.html), rollback...

## Other than Functor

- `Contravariant`: https://pursuit.purescript.org/packages/purescript-contravariant/6.0.0/docs/Data.Decide
- `Profunctor`
- `Category`
- `Arrow`
- `Juxt`

Monoidal tensors (identities, associativities). Adjunctions. Strength.

# Data typeclasses

- `Finite`, with a finite set?
- `Eq`, total `Ord` for programming purposes (`OrdMap`)
  - basically all datatypes except functions should be able to implement this!
  - not convinced that Rust `PartialEq` should be a thing .... idk. maybe just lawful vs unlawful `Eq`
  - `PhasedEq`, into `PhasedAll = [Bool]` (Haskell-style laziness)
  - `PhasedOrd` possibly...
  - `Eq1` with `eqBy :: (x -> y -> Bool) -> f x -> f y -> Bool`
    - `Eq1 f => Confoldable f`
    - `eqWith2 :: Eq t => (x -> t, y -> t) -> f x -> f y -> Bool`
    - `eqWith :: Eq t => (x -> t) -> f x -> f x -> Bool`
- `Ord` and `Representable` should both correlate to Maps and Sets somehow, right? sorting algorithms and memoization structures? like `Ord Int` should give you access to `IntMap` and `IntSet`, sum types could store their variants in separate maps, and so on
- `PreOrderable`, `PartialOrderable`, `TotalOrderable` for semantic purposes
  - I guess in DTT, there is just a pre order Prop-relation, laws for each typeclass, and then optionally some (efficient) decision functions later
  - `(Ord, TotalOrderable) <= Orderable` will state that they agree!
- `Ord <= Enum`, `Bounded`, `BoundedEnum`
- `Show`, `Debug`, `Pretty`, `ToTeX`, `ToString`, `Interpolate`, etc.
- `Index`, [`At`](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/8.0.0/docs/Data.Lens.At)
- bidirectional binary and JSON serialization codecs
- `Default` and `IsDefault`, for default maps (functions of finite support)?

# Conversions/Coercions

- ints and ints (`fromIntegral`), float and ints, ...
- `Foldable ~> Alternative` but with feeling (`unwrap . foldMap (Alt . pure)`)
- `sample` into monoids, for statistical analyses and such
  - `(Ord level, Semigroup metadata) => Semigroup (MinWith level metadata)`, `(MaxWith level metadata)`
    - keep track of metadata that rides along with the choices made in finding the min or max level

# Datatypes

- `Void`, `Unit`
- `data Bool = true | false`
- `data Ordering = eq | lt | gt`
- `data PartialOrdering = eq | uncomp | lt | gt`
- `data PreOrdering = eq | equiv | uncomp | lt | gt`

- `Identity = 'mk`, `Maybe = nothing | just`, `Either = left | right` and validation functors, `Tuple = tuple`, `These = this | that | both`, ...

# Syntax

## Precedence

- Algebraic operators
  - Arithmetic: `+`, `*`, `^`, `-`, `/`, `%`, smul, etc.
  - Bitwise: `!.`, `.|.`, `.&.`, etc.
  - Lattice: `/\`, `\/`, cannot be combined with arithmetic
- Relational operators
  - `<`, `>=`, etc.
  - `==`, `!=`
- Logical operators
  - `||`, `&&`
- Dependent operators
  - `=`
- Type formers
  - `->`
- Control flow combinators
  - `$`
    - Right-hand scoped like `let…in`, so it works with `:` annotations?
- Distfix default
  - `(if:then:else:)`, `(:if:else:)`
- Pseudo-language syntax
  - something for associative arrays, `::=`
- Language syntax
  - `:`
  - `,`, `;`, `|`, `:=`
