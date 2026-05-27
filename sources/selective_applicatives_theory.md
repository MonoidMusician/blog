---
title: Selective Applicative Functors
subtitle: "Theory of Composition, Syntax, Analysis, [&c.]{t=}"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

As a quick [recap](selective_applicatives_theoretical_basis.html):

- Selective applicative functors are meant to sit in between applicatives and monads in terms of constraints vs flexibility.
  The implementation has more freedom (there are more selective applicative structures for a carrier type than monad or applicative structures!), while the combinators offer fewer guarantees than monads.
- The original formulation of selective applicative functors was about selectively running or skipping effects, based on previous results in the applicative functor (what I call “determined choice”).
- I argued that this is not enough, and we should really encode exclusive (and exhaustive) determined choice.
- This required focusing on arrows instead of functors, but we got pretty far with focusing on applicative arrows `f (i -> o)`{.haskell} (which can be seen as a [change of enriching category](https://ncatlab.org/nlab/show/change+of+enriching+category)).
- We derived laws: the basic laws for “raw control flow” (consisting of associativities and identities for the sequential and branching structures), and the near-semiring laws for “refined control flow” (refining the interaction of sequencing and branching).

Now we want to focus on the theory of selective applicative functors: what kind of syntax could we use for them, how do they compose, what do transformers look like, what are useful forms of static analysis, and so on.

One of the main obstacles is that there is no “state transformer functor” for selective applicatives:

- For monads, [`newtype StateT s m o = StateT (s -> m (s, o))`{.haskell}](https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-State-Lazy.html#t:StateT)^[Yes, I prefer that order of the tuple, since it is actually a `Functor`{.haskell} then.] is the state monad transformer.
- For arrows more generally, it is [`newtype StateArrow s p i o = StateArrow (p (s, i) (s, o))`{.haskell}](https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-State.html).^[There is also a `CoStateArrow`{.haskell}, with `p (s -> i) (s -> o)`{.haskell}, but this is not compatible with selective applicative functors.]
- We can then see that `StateT`{.haskell} is derived via the chain of isomorphisms

  ```haskell
  StateArrow s (Kleisli m) i o
  ≈ Kleisli m (s, i) (s, o)   -- definition of StateArrow
  ≈ (s, i) -> m (s, o)        -- definition of Kleisli
  ≈ i -> s -> m (s, o)        -- swap and curry
  ≈ i -> StateT s m o         -- definition of StateT
  ≈ Kleisli (StateT s m) i o  -- definition of Kleisli
  ```

  That is, we can pop the input state `s`{.haskell} out as its own argument to the function, since `Kleisli`{.haskell} arrows have that structure of exposing the input directly.
- However, there is no way to do this for applicatives: `StateArrow`{.haskell} works great, `f ((s, i) -> (s, o))`{.haskell} is exactly what we want, it is an applicative arrow that carries state `s`{.haskell} around, but there is no way to express this as a functor transforming `f`{.haskell}: it would need to look at the arrow wrapped inside `f`{.haskell}.^[I believe this is because this is not a closed category?]

So, if we want to work on the theory in full generality, and incorporate things like state monads, we will want to work with arrows instead of functors.^[I believe that state monads are the main piece that is left out by considering functors instead of arrows.]

But this comes at a cost: arrows have worse syntax, you have to work with them point-free.
The benefit of working with selective applicative functors was that we just had to consider `case`{.haskell} structure as its own thing, but otherwise they were just applicative functors `f o`{.haskell} which we already had applicative `do`{.haskell}/`ado`{.purescript} notation for.

So I think a reasonable middle ground is that we should work with arrows that we can lower to applicative arrows, that is, equipped with a natural isomorphism `forall i o. p i o <-> f (g i -> h o)`{.haskell}, for functors `f`{.haskell}, `g`{.haskell}, and `h`{.haskell} determined by `p`{.haskell}.
The `g`{.haskell} and `h`{.haskell} functors here serve the purpose of tracking the additional state variables that come along for the ride.
(We will want some conditions on these: `g`{.haskell} probably wants to be comonadic or at least copointed, maybe `h`{.haskell} wants to be monadic; they need to be compatible in some way; they should lift over `CaseTree`{.haskell}, like `(s,)`{.haskell} already does: `CaseTreeP p i o -> CaseTreeF f (g i) (h o)`{.haskell}; and they will want to commute with the functors from transformers.)

Normally, when `g ~ Id`{.haskell}, we saw that the function disappears: `f (() -> h o) ≈ f (h o)`{.haskell} due to strength of the functor.
So when there is no state involved, we can work with a familiar functor interface again.

## Composition

How can we expect selective applicative functors / control flow arrows to compose?

It turns out that they compose somewhat like monads, but a little bit like applicatives too.

- You can take the `Product`{.haskell} of two selective applicative functors or control flow arrows, in the sense of `data ProductF f g x = ProductF (f x) (g x)`{.haskell} or `data ProductP p q i o = ProductP (p i o) (q i o)`{.haskell}, where the branches operate independently.
- This does not work for the the `Day`{.haskell} convolution, unfortunately.
  But you can take the `Day`{.haskell} convolution of applicative arrows, and take the `Product`{.haskell} with that, or `Compose (Day ...) ...`{.haskell}.
- You can compose an applicative functor `f`{.haskell} on the outside: `(Applicative f, Casing g) => Casing (Compose f g)`{.haskell} or `StaticArrow f p i o = f (p i o)`{.haskell}.
  This in general demotes it to “raw control flow”, unless the applicative functor is particularly well behaved, like `Maybe`{.haskell} for example (which is idempotent and even commutative, so it preserves all of the laws).
- You can compose certain functors on the inside: `Tuple`{.haskell} like `WriterT`{.haskell}, `Maybe`{.haskell} like `MaybeT`{.haskell}, `Either`{.haskell} like `ExceptT`{.haskell}.
- I am not sure about lists!
  First of all, `ListT`{.haskell} is not even implementable without `ArrowTraversing`{.haskell}: it cannot generate infinite case branches to handle arbitrary sized lists.
  And `ListT`{.haskell} is not well behaved; is there an equivalent of `LogicT`{.haskell} to apply to arrows?
- A state transformer requires using arrows instead of functors.
  Without that, you need to do manual threading of the state, which gets annoying and gets in the way of other abstractions!
- Various [constant functors](#monoids) work well: near-semirings (or even just monoids, for “raw control flow”); lattices and semilattices (many static analyses that compilers use take this form!), including sets, both as lattices (where branches are joined using intersection) and as semilattices (where branches are joined using unions, just like sequencing).^[This is the generalization of `Over`{.haskell}- and `Under`{.haskell}-approximation from the original paper on selective applicative functors. Note how we can now capture the fact that two branches request the same resource, for example, and thus include it in the tally.]

Finally, these are just guides to how things may compose, examples to demonstrate what is possible at a minimum.
The real joy of selective applicative functors is that there is a lot of creativity in how these layers of structure may interact: .

## Arrows

Our goal is to have all of our familiar programming niceties, but now in the land of arrows, for more precise representation of control flow, without making it too onerous to use.
It shouldnʼt require writing all code point-free!

What is the existing state of the art for arrows?

Arrows are still an obscure and neglected abstraction, to the point that arrow syntax has been buggy in GHC[?](https://github.com/tomjaguarpaw/Arrows2/issues/1)[?](https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=all&label_name%5B%5D=Arrows&first_page_size=20).

I think arrows have suffered from the difficulties of their syntax – no one wants to rewrite their whole program to fit with a different abstraction – and from the lack of concrete-feeling applications.
Itʼs not that arrows have no applications – on the contrary, they should be widely useful.
But besides “oh you can use Kleisli arrows to do monads but with worse syntax”, and maybe a few things like FRP (which doesnʼt really need arrows either tbh, it works fine with functors), there hasnʼt been a real need for arrows.
The fact that applicatives can be used to build arrows is not something Iʼve even seen advertised before, tbh.

However, there is the [arrows](https://hackage.haskell.org/package/arrows-0.4.4.2) package, which provides transformers and MTL-style typeclasses.
This is a good source of batteries to get started with.

Besides the monad-style transformers (Reader, Writer, State, Error), the other notable transformer is [`newtype StaticArrow f p i o = StaticArrow (f (p i o))`{.haskell}](https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-Static.html), which provides exactly what we want for applicative arrows (change of enriching category).

Also of interest is `ArrowChoice`{.haskell}: the combinators `(+++) :: p u x -> p v y -> p (u \/ v) (x \/ y)`{.haskell} and `(|||) :: p u o -> p v o -> p (u \/ v) o`{.haskell} *would* encode exclusive determined choice, but `left :: p i o -> p (i \/ c) (o \/ c)`{.haskell} and `right :: p i o -> p (c \/ i) (c \/ o)`{.haskell} do *not*.^[And it is not clear to me if the laws require them to agree… they seem to imply that `(|||)`{.haskell} should only influence efficiency, not behavior?]

(Additionally, `left`{.haskell} and `right`{.haskell} do not really make sense without `Category`{.haskell}, and for my other purposes – namely, bidirectional codecs – I do want a combinator that works on profunctors without `Category`{.haskell}.)

So … I think the situation is salvalgeable.
We should make our own combinators, types, and typeclasses, but it is worth taking inspiration from there.

:::{.Details box-name="Aside"}
Ironically, `ArrowApply`{.haskell} is the typeclass that expresses that an arrow is monadic, it is not related to `Applicative`{.haskell} at all.
:::

### Control Flow Arrows

We have strength in the normal sense (we should be able to preserve data through all of the control flow):

`first :: p u x -> p (u /\ c) (x /\ c)`{.haskell}
`second :: p v y -> p (c /\ v) (c /\ y)`{.haskell}

We have exclusive determined choice:

`(+++) :: p u x -> p v y -> p (u \/ v) (x \/ y)`{.haskell}

We have a free structure, arrow-style now (where `p`{.haskell} should be a strong profunctor already):

```haskell
data ControlFlowP p i o where
  Pure :: (i -> o) -> ControlFlowP i o
  Lift :: p i o -> ControlFlowP i o
  Sequence :: ControlFlowP p i j -> ControlFlowP p j o -> ControlFlowP p i o
  Absurd :: (i -> Void) -> ControlFlowP f i o
  CaseBranch ::
    (i -> Either x y) ->
    ControlFlowP p x o ->
    ControlFlowP p y o ->
    ControlFlowP p i o
```

And to make the syntax more palatable, we might ask that it can be lowered into something more familiar.

```haskell
class
  ( Applicative f
  , Monad m
  , Profunctor p
  ) => ConcreteArrow p f m | p -> f m where
    lowerP :: forall i o. p i o -> f (i -> m o)
    raiseP :: forall i o. f (i -> m o) -> p i o

instance ConcreteArrow p f m => ConcreteArrow (ControlFlowP p) (FreeSelective f) m

-- The product instance requires `m` to agree, unfortunately
instance (ConcreteArrow p f1 m, ConcreteArrow q f2 m) =>
  ConcreteArrow (ProductP p q) (ProductF f1 f2) m

-- Composing with an applicative on the outside is easy
instance (Applicative f1, ConcreteArrow p f2 m) =>
  ConcreteArrow (StaticArrow f1 p) (Compose f1 f2) m

newtype Mixolydian f m i o = Mixolydian (f (i -> m o))
instance (Applicative f) => Category (Mixolydian f m) where
  identity = pure pure
  (<<<) = lift2 (<=<)
instance (Applicative f, NicelyMixed g h) => ArrowChoice (Mixolydian f g h) where
  (+++) = lift2 \x y -> either (x >>> fmap Left) (y >>> fmap Right)
```

However, `g`{.haskell} and `h`{.haskell} are likely to be riddled with newtype wrappers, making this inconvenient to use.^[This is one of my main complaints about Haskell: I wish there were a way to make newtype constructors much more transparent. This is one of the things I want to address in [tmTTmt](tmttmt.html).]

So we might as well introduce a class that is allowed to unwrap those types for you, at the expense of not having clean `(Functor g, Functor h)`{.haskell} superclasses:

```haskell
class
  ( Applicative f
  , Profunctor p
  ) => ConcreteArrowN p f i o gi ho
    | p -> f, p i -> gi, p o -> ho
  where
    lowerN :: p i o -> f (gi -> ho)
    raiseN :: f (gi -> ho) -> p i o
```

### Traversing Arrows

We can traverse over some functors already:

- `Tuple s`{.haskell} is easy: this is just the strength of the arrow, which all of our arrows already satisfied.
- `Maybe`{.haskell} and `Either e`{.haskell} are also easy: it is just binary branching control flow, where one branch is pure (to pass through `Nothing`{.haskell} and `Left e`{.haskell}), and the other wraps the arrow.
- `data Pair t = Pair t t`{.haskell} requires a little bit more work, but it is also possible.

  ```purescript
  instance casingPairT :: (Casing f) => Casing (PairT f) where
  caseTreeOn (PairT p) cases = PairT $ uncurry Pair <$>
    caseTreeOn (caseTreeOn pTuple (firstCaseTree (slot1 cases))) (secondCaseTree (slot2 cases))
    where
    pTuple = p <#> \(Pair l r) -> Tuple l r
    slot1 :: forall i r. CaseTree i (PairT f) r -> CaseTree i f r
    slot1 = hoistCaseTree \(PairT t) -> t <#> \(Pair f _) -> f
    slot2 :: forall i r. CaseTree i (PairT f) r -> CaseTree i f r
    slot2 = hoistCaseTree \(PairT t) -> t <#> \(Pair _ g) -> g
  ```

In general, we find that the functors need to not only be traversable (which implies that each constructor holds a finite number of the wrapped type), but also have a finite number of cases to handle – be non-recursive.
Things like `List`{.haskell} are *not* possible to handle out of the box, unfortunately.
Each individual case (`Nil`{.haskell}, `Cons _ Nil`{.haskell}, `Cons _ (Cons _ Nil)`{.haskell}, and so on) hold a finite number of items, but there is no finite case tree to handle all of them at once.

So we can introduce a typeclass to add this capability:

```haskell
-- The intention is that `t` is `Traversable`, though
-- some implementations may not need that constraint.
class ArrowTraversing t p where
  traverseP :: forall i o. p i o -> p (t i) (t o)
```

For `Klesli m`{.haskell}, this is clear: it is literally just the normal `traverse`{.haskell} (although interestingly the `Monad m`{.haskell} constraint required for `Category (Kleisli m)`{.haskell} is not required for `traverse`{.haskell}, just `Applicative m`{.haskell}).

For applicative arrows `f (i -> o)`{.haskell} on the other hand, it is not clear how to do this.

The implementation is clear, actually: surprisingly it would just be `fmap fmap :: forall f t i o. (Functor f, Functor t) => f (i -> o) -> f (t i -> t o)`{.haskell}, not even using `Traversable t`{.haskell}!
But there is no way to duplicate the effects of `f`{.haskell} up front, before we even get to see what shape `t i`{.haskell} has.

So this only works for *idempotent applicatives* satisfying `fx <* fx = fx = fx *> fx`{.haskell}: for example, `f = Maybe`{.haskell} or `f = Either e`{.haskell}.
So we can implement `ArrowTraversing`{.haskell} for `Maybe (i -> o)`{.haskell} and `Either e (i -> o)`{.haskell} without any reservation: duplicating the effect wouldnʼt have any effect (ahem) anyway.

:::{.Warning box-name="Caveat"}
There is one caveat: strictly speaking, idempotence means that the same effect repeated one-or-more times performs no additional effects.
However, the traversable `t`{.haskell} could be empty (`t Void`{.haskell}), at which point the effects would be supposed to happen zero times?

We just disregard this case.
It seems more useful to have an upper bound for the effects than to worry about an effect that might not run.

However, one could restrict it to nonempty containers with `Traversable1`{.haskell}, and/or have a dedicated implementation that is the identity for empty containers.

For example, for `StaticArrow Maybe (Kleisli Maybe)`{.haskell} (a contrived example, but perhaps useful as a building block for other use cases), one could implement `traverseP @Maybe`{.haskell} like this (yes there are three `Maybe`{.haskell}s floating around: as a traversable, as an inner monad, and as an outer applicative):

```haskell
traversePMaybeMaybeMaybe :: Maybe (i -> Maybe o) -> Maybe (Maybe i -> Maybe (Maybe o))
traversePMaybeMaybeMaybe mimo = Just \case
  Nothing -> Just Nothing -- we did not need the effect anyways
  Just i -> case mimo of
    -- apply the effect and re-wrap in `Just`
    Just imo -> (\o -> Just o) <$> imo i
    -- whoops, we needed an effect but did not have it
    Nothing -> Nothing
```
:::

### Looping Arrows

Ironically these are a little easier to think about than traversing arrows.

Haskellʼs [`ArrowLoop`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Arrow.html#v:loop) suffers from the classic Haskell problem of requiring laziness to work.
It loops in the sense of “tying the knot”, instead of in the sense of `do … while`{.c} loops of classic control flow.

Instead, letʼs try something in the style of [`MonadRec`{.purescript}](https://pursuit.purescript.org/packages/purescript-tailrec/6.1.0/docs/Control.Monad.Rec.Class#t:MonadRec):

```haskell
class Profunctor p => ArrowWhile p where
  doWhileP :: String -> p i (Either r i) -> p i r
```

<!-- doWhileF :: ControlFlowF f i (Either r i) -> f i -> f r -->

Now you know the effects of `p`{.haskell} happen one or more times, so again, having idempotence of effects is nice, or having a Kleisli arrow is also sufficient.

In contrase to `traverseP`{.haskell}, `doWhileP`{.haskell} provides absolutely no guarantee of termination: it could happily produce `Left`{.haskell} every time.

Sometimes we can do a little better, and produce a fixed point.
For example, in selective applicative parsers as I have implemented them (in the [LR(1)]{t=} style, though this is not essential), the `leastFixedPointP`{.haskell} produces a named nonterminal to pass to the function, which then generates the rest of the grammar _for that nonterminal_ and returns it.
The output then uses the named nonterminal that has been incorporated into the grammar.

```haskell
class (Profunctor p, ArrowWhile p) => ArrowFixedPoint p where
  leastFixedPointP :: String -> (p i r -> p i r) -> p i r
```

(I like giving my fixed points names, so that you can print out a grammar for the parser, for example!)

Now we can implement `doWhileP`{.haskell} in terms of `leastFixedPointP`{.haskell}:

```haskell
doWhileP name producesRight = leastFixedPointP name \continue -> do
  producesRight >>> (id +++ continue)
```

When you hear “least fixed point”, you should instantly think “semilattices”.
That is the kind of structure we need to make this work for applicative arrows and not just monads.
Yet another sign that having semilattice structure is important for control flow when we can have it.

### Multi-Staged Arrows

For mixed static analysis, like `Haxl`{.haskell}.

```haskell
class Applicative f => MultiStage f where
  twoStages :: f (f r) -> f r
class Profunctor p => MultiStageP p where
  twoStagesP :: p i (p i r) -> p i r

nextStage :: forall f r. MultiStage f => f r -> f r
nextStage = twoStages . pure
```

This says that the static analysis can be interleaved with (monadic) evaluation:
you get a glimpse of static analysis of the first stage of the program,
then you can evaluate it and come up with a result already, or a second stage to analyze and then run recursively.

If `f`{.haskell} is a monad, its selective should agree with that monad, and then `twoStages = join`{.haskell} as well.
However, in general we do not expect `nextStage = id`{.haskell}: instead, it will hide the static analysis from the first stage and delay evaluation to the second stage.
This means that it does not necessarily make a monad, but it should probably still be associative: `twoStages . twoStages = twoStages . fmap twoStages`{.haskell}.

For `Haxl`{.haskell}, `nextStage`{.haskell} would be `Haxl . pure @IO . Blocked mempty`{.haskell} that runs no effects and contributes no blocking dependencies, delaying all effects until the second stage.
This, for instance, could be used to delay the effects of an argument to `select`{.haskell} or `branch`{.haskell} to cancel or delay its speculative execution, like `>>=`{.haskell} also expresses.

:::Note
The argument that `Haxl`{.haskell} is a monad relies on the fact that data fetch effects should be unobservable.

:::

```haskell
-- A Haxl computation is either completed (Done) or Blocked on pending data requests
data Result a = Done a | Blocked BlockedRequests (Haxl a) deriving Functor
newtype Haxl a = Haxl { runHaxl :: IO (Result a) } deriving Functor
```

It is such a common pattern, great for static analysis.

```haskell
data RequestRespond w r m a = RR w (r -> m a)
instance (Monoid w, Applicative m) => Applicative (RequestRespond w r m)

-- The same data type as a free monad
data Stages f a = Present a | Future (f (Stages f a))

instance Applicative f => Selective (Stages f) where
  select (Present (Right b)) _ = Present b
  select (Present (Left a)) fab = fab <@> a
  select fab (Present f) = either f id <$> fab
  select (Future f1) (Future f2) = Future $ select <$> f1 <*> f2

instance Applicative f => Casing (Stages f) where
  -- If we know the value already, we can select the correct case
  -- from the case tree directly
  caseTreeOn (Present value) cases = applyCaseTree cases value
  -- Otherwise we sequence the first layer of `f` effects from
  -- the case tree and run `caseTreeOn` the results
  caseTreeOn (Future scrutinee) cases = Future $
    caseTreeOn <$> scrutinee <*> traverseCaseTree cue cases
```

Note that `nextStage`{.haskell} for the free _monad_ would still be the identity function: `join . Return`{.haskell} literally computes to its input.
But the `nextStage`{.haskell} we want to talk about uses `pure`{.haskell} from **`f`{.haskell}** instead (specifically: `Future . pure @f . Present`{.haskell}), so it does not collapse like `Present`{.haskell} would.

For my [staged, token-gathering parse machine](https://gist.github.com/MonoidMusician/b8ce75f012a4008f7904cde941a7fcba#file-parsemachine-hs-L107), one cannot delay the effects (having the set of next tokens to parse is required), but there is internal staging to separate between speculative parses and committed parses.
(This means that `nextStage`{.haskell} would be idempotent: it can promote to committed parses but there is no stage beyond that.)
Incidentally that parser type is already a monad, but it is a similar idea to what dedicated selective applicatives might want to do.



## Monoids

Before we dive into monoids for static analysis (that is, the non-functor part of the picture), I want to emphasize that the strength of selective applicative functors/arrows is that not only do you get static analysis for a computation overall, but because it can be interleaved with the functor/arrow structure, it lets you refine that static analysis *as the computation runs*.

For example, for the `Haxl`{.haskell} functor, the computation of the set of blocking requests is interleaved with actually running those requests, so when a particular branch is selected based on known information, it actually knows what requests to use.

Weʼll make our own typeclass for static analysis, since it doesnʼt quite fit into existing typeclasses, and it is much nicer to compose analyses as types than as functors.

```haskell
class StaticAnalysis m where
  analyzeSequence :: [m] -> m
  analyzeBranches :: [m] -> m

-- | Product of analyses
instance (StaticAnalysis m, StaticAnalysis w) => StaticAnalysis (m, w)

-- | A constant functor, for `Casing`
newtype AnalyzeBy m t = Analysis m

instance StaticAnalysis m => Casing (AnalyzeBy m) where ...

-- | Or we can bolt it onto an existing `Casing` functor
-- | (this is isomorphic to `ProductF (AnalyzeBy m)`).
newtype WithAnalysis m f t = WithAnalysis m (f t)

instance (StaticAnalysis m, Casing f) => Casing (AnalyzeBy m) where ...
```

You *can* do static analysis with any old monoid, having the sequencing and branching structures be the same – that is what the paucity of laws for “raw control flow” allows.
But letʼs talk about more interesting, better-behaved analyses.

One of the best ways to tabulate information is with sets.
For Haxl, this would be the set of dependencies to fetch.
Another example would be the set of nonterminals used in a parser combinator expression.

We can use sets in two ways: as a lattice, where sequencing is union and branching is intersection, or as a semilattice, where both operations are union.

For `UnderSet s`{.haskell}, we need a constructor for the empty intersection \(x \cap \top = x\), which will function as an absorptive element \(\top \cup x = \top\) to follow the near-semiring law `0 * x = 0`{.haskell}.^[Technically we could use `BoundedEnum s`{.haskell}, but that is less efficient.]

However the lattice structure isnʼt quite right: the commutativity of the lattice would force `x * 0 = 0`{.haskell}, or \(x \cup \top = \top\), but we do not want to discard requirements *before* absurdity, only after absurdity.
So we use a non-commutative multiplication where `x * 0 = x`{.haskell} instead.
However, *this* is in conflict with the multiplicative identity `y = 1 * y`{.haskell}, since ` 0 = 1 * 0 = 1`{.haskell} then.
So we need a new multiplicative identity, to denote totally pure computations (directly lifted from functions), as opposed to computations with no dependencies (but possibly other effects).

At this point we might as well pull the identities out into their own structure, which will normalize the control flow before it reaches two semigroups for sequencing and branching.
However there is one last choice that the implementation cannot make, which is what happens when one or more branches of control flow are pure.
For `OverSet`{.haskell}, `withPureBranch`{.haskell} is just the identity function, but for `UnderSet`{.haskell}, it needs to produce `Set.empty`{.haskell} to indicate a computation with no requirements (no guaranteed dependencies) but that may be impure nontheless.

```haskell
class StaticAnalysis1 m where
  analyzeSequence1 :: NonEmpty m -> m
  analyzeBranches1 :: NonEmpty m -> m
  -- Should be idempotent and commute with `analyzeBranches1`
  withPureBranch :: m -> m

data MaybeAnalysis m
  = SomeAnalysis m
  | PureAnalysis
  | AbsurdAnalysis

instance StaticAnalysis1 m => StaticAnalysis (MaybeAnalysis m) where
  analyzeSequence =
    -- Absurd computations also cancel everything beyond them
    takeWhile notAbsurd >>>
    -- Pure computations do not matter; extract the wrapped analysis
    mapMaybe nonPure >>>
    \case
      -- It ended up being wholly pure
      [] -> PureAnalysis
      -- Defer to the wrapped analysis
      (a : as) -> SomeAnalysis (analyzeSequence1 (a :| as))
    where
    notAbsurd AbsurdAnalysis = False
    notAbsurd _ = True

    nonPure (SomeAnalysis m) = Just m
    nonPure _ = Nothing

  analyzeBranches =
    -- We need to know if we have at least one pure branch
    partition notPure >>>
    -- But we can filter out all of the absurd branches
    lmap (mapMaybe nonAbsurd) >>>
    \case
      -- No non-absurd branches at all
      -- 0 + 0 = 0
      ([], []) -> AbsurdAnalysis
      -- Only pure and absurd branches
      -- 1 + 0 = 1 + 1 = 0 + 1 = 1
      ([], _) -> PureAnalysis
      (a : as, []) -> SomeAnalysis (analyzeBranches1 (a :| as))
      -- At least one pure branch
      -- 1 + x + 1 = 1 + x
      (a : as, _) -> SomeAnalysis (withPureBranch (analyzeBranches1 (a :| as)))
    where
    notPure PureAnalysis = False
    notPure _ = True

    nonAbsurd (SomeAnalysis m) = Just m
    nonAbsurd _ = Nothing

unMaybeAnalysis :: forall m. StaticAnalysis m => MaybeAnalysis m -> m
unMaybeAnalysis (SomeAnalysis m) = m
unMaybeAnalysis PureAnalysis = analyzeSequence []
unMaybeAnalysis AbsurdAnalysis = analyzeBranches []
```

Now we can get to the sets.

```haskell
-- | For under-approximation of requirements,
-- | sequencing is union and branching is intersection.
newtype UnderSet s = UnderSet (Set s)

instance Ord s => StaticAnalysis1 (UnderSet s) where
  analyzeSequence1 = coerce Set.unions
  analyzeBranches1 = coerce Set.intersections

-- | Sequencing and branching are both union, for
-- | over-approximation/tallying what occurs.
newtype OverSet s = OverSet (Set s)

instance Ord s => StaticAnalysis (OverSet s) where
  analyzeSequence = coerce Set.unions
  analyzeBranches = coerce Set.unions
```
