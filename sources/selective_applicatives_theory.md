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

## Pluralism

So, if we want to work on the theory in full generality, and incorporate things like state monads, we will want to work with arrows instead of functors.^[I believe that state monads are the main piece that is left out by considering functors instead of arrows.]

But this comes at a cost: arrows have worse syntax, you have to work with them point-free.
The benefit of working with selective applicative functors was that we just had to consider `case`{.haskell} structure as its own thing, but otherwise they were just applicative functors `f o`{.haskell} which we already had applicative `do`{.haskell}/`ado`{.purescript} notation for.

<details class="Details">
<summary>Compromise</summary>
One possible middle ground is that we could work with arrows that we can lower to applicative arrows, that is, equipped with a natural isomorphism `forall i o. p i o <-> f (g i -> h o)`{.haskell}, for functors `f`{.haskell}, `g`{.haskell}, and `h`{.haskell} determined by `p`{.haskell}.
The `g`{.haskell} and `h`{.haskell} functors here serve the purpose of tracking the additional state variables that come along for the ride.
(We will want some conditions on these: `g`{.haskell} probably wants to be comonadic or at least copointed, maybe `h`{.haskell} wants to be monadic; they need to be compatible in some way; they should lift over `CaseTree`{.haskell}, like `(s,)`{.haskell} already does: `CaseTreeP p i o -> CaseTreeF f (g i) (h o)`{.haskell}; and they will want to commute with the functors from transformers.)

Normally, when `g ~ Id`{.haskell}, we saw that the function disappears: `f (() -> h o) ≈ f (h o)`{.haskell} due to strength of the functor.
So when there is no state involved, we can work with a familiar functor interface again.
</details>


:::Key_Idea
The takeaway is that we ought to develop a standard library with support for both styles.
Functors (monads and applicatives and selectives) when they are convenient, of course, and arrows (of corresponding types) for when we need their precision and for handling the branches of control flow specifically.
We cannot give up on abstraction just because one form is less convenient, less standard.
We will look for more nuanced control flow on *both* sides.
:::

However, with that said, we will need to focus on arrows more than functors.
The standard library of functors is pretty well developed already, and the existing theory of arrows needs some tweaks to be workable for our purpose.


## Composition

How can we expect selective applicative functors / control flow arrows to compose?

It turns out that they compose somewhat like monads, but a little bit like applicatives too.

- You can take the `Product`{.haskell} of two selective applicative functors or control flow arrows, in the sense of `data ProductF f g x = ProductF (f x) (g x)`{.haskell} or `data ProductP p q i o = ProductP (p i o) (q i o)`{.haskell}, where the branches operate independently.
  This already works for monads and applicatives but often isnʼt the type of composition you want.
- This does not work for the the `Day`{.haskell} convolution, unfortunately.
  But you can take the `Day`{.haskell} convolution of applicatives/applicative arrows, and use that applicative as a rigid selective.^[Rigid means it comes from an applicative functor, and does not support selective effects.]
- You can compose an applicative functor `f`{.haskell} on the outside: `(Applicative f, Casing g) => Casing (Compose f g)`{.haskell} or `StaticArrow f p i o = f (p i o)`{.haskell}.
  This in general demotes it to “raw control flow”, unless the applicative functor is particularly well behaved, like `Maybe`{.haskell} for example (which is idempotent and even commutative, so it preserves all of the laws).
- You can compose certain functors on the inside: `Tuple`{.haskell} like `WriterT`{.haskell}, `Maybe`{.haskell} like `MaybeT`{.haskell}, `Either`{.haskell} like `ExceptT`{.haskell}.
- I am not sure about lists!
  First of all, `ListT`{.haskell} is not even implementable without `ArrowTraversing`{.haskell}: it cannot generate infinite case branches to handle arbitrary sized lists.
  And `ListT`{.haskell} is not well behaved; is there an equivalent of [`LogicT`{.haskell}](https://hackage-content.haskell.org/package/logict-0.8.2.0/docs/Control-Monad-Logic.html#t:LogicT) to apply to arrows?
- A state transformer requires using arrows instead of functors.
  Without that, you need to do manual threading of the state, which gets annoying and gets in the way of other abstractions!
- Various [constant functors](#monoids) work well: near-semirings (or even just monoids, for “raw control flow”); lattices and semilattices (many static analyses that compilers use take this form!), including sets, both as lattices (where branches are joined using intersection) and as semilattices (where branches are joined using unions, just like sequencing).^[This is the generalization of `Over`{.haskell}- and `Under`{.haskell}-approximation from the original paper on selective applicative functors. Note how we can now capture the fact that two branches request the same resource, for example, and thus include it in the tally.]

Finally, these are just guides to how things may compose, examples to demonstrate what is possible at a minimum.
The real joy of selective applicative functors is that there is a lot of creativity in how these layers of structure may interact:
some parts may be rigid and act all the time, others may be selectively run based on control flow.
Lots of types of information brought together to synthesize a more complete picture.

And one can invent new selective applicatives from whole cloth, not really related to other constructs.

## Nuanced Control Flow

Our goal is to have all of our familiar programming niceties, but now in the land of selectives and arrows, for more precise representation of control flow, without making it too onerous to use.
Arrows are a powerful tool for this, but it shouldnʼt require writing all code point-free!

### Arrows

What is the existing state of the art for arrows?

Arrows are still an obscure and neglected abstraction, to the point that arrow syntax has been buggy in GHC[?](https://github.com/tomjaguarpaw/Arrows2/issues/1)[?](https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=all&label_name%5B%5D=Arrows&first_page_size=20).

I think arrows have suffered from the difficulties of their syntax – no one wants to rewrite their whole program to fit with a different abstraction – and from the lack of concrete-feeling applications.
Itʼs not that arrows have no applications – on the contrary, they should be widely useful.
But besides “oh you can use Kleisli arrows to do monads but with worse syntax”, and maybe a few things like FRP (which doesnʼt really need arrows either tbh, it works fine with functors), there hasnʼt been a real need for arrows.
The fact that applicatives can be used to build arrows is not something Iʼve even seen advertised before, tbh.
Arrows seem to follow the applicative/selective/monadic distiction like functors.

However, there is the [arrows](https://hackage.haskell.org/package/arrows-0.4.4.2) package, which provides transformers and MTL-style typeclasses.
This is a good source of batteries to get started with.

Besides the monad-style transformers (Reader, Writer, State, Error), the other notable transformer is [`newtype StaticArrow f p i o = StaticArrow (f (p i o))`{.haskell}](https://hackage.haskell.org/package/arrows-0.4.4.2/docs/Control-Arrow-Transformer-Static.html), which provides exactly what we want for applicative arrows (change of enriching category).

Also of interest is [`ArrowChoice`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Arrow.html#t:ArrowChoice): the combinators `(+++) :: p u x -> p v y -> p (u \/ v) (x \/ y)`{.haskell} and `(|||) :: p u o -> p v o -> p (u \/ v) o`{.haskell} *would* encode exclusive determined choice, but `left :: p i o -> p (i \/ c) (o \/ c)`{.haskell} and `right :: p i o -> p (c \/ i) (c \/ o)`{.haskell} do *not*.^[And it is not clear to me if the laws require them to agree… they seem to imply that `(|||)`{.haskell} should only influence efficiency, not behavior?]

(Additionally, `left`{.haskell} and `right`{.haskell} do not really make sense without `Category`{.haskell}, and for my other purposes – namely, bidirectional codecs – I do want a combinator that works on profunctors without `Category`{.haskell}.)

So … I think the situation is salvalgeable.
We should make our own combinators, types, and typeclasses, but it is worth taking inspiration from there.

:::{.Details box-name="Aside"}
Ironically, [`ArrowApply`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Arrow.html#t:ArrowApply) is the typeclass that expresses that an arrow is monadic, it is not related to `Applicative`{.haskell} at all.
:::

### Control Flow Arrows

We have strength in the normal sense (we should be able to preserve data through all of the control flow):

`first :: p u x -> p (u /\ c) (x /\ c)`{.haskell}
`second :: p v y -> p (c /\ v) (c /\ y)`{.haskell}

We have exclusive determined choice:

`(+++) :: p u x -> p v y -> p (u \/ v) (x \/ y)`{.haskell}

We have a free structure, arrow-style now (where `p`{.haskell} needs to be a strong profunctor already):

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

### Concrete Arrows

And to make the syntax more palatable, we might ask that it can be lowered into something more familiar.
We should not rely on this, but it is nice to have sometimes.

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

### Traversing

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
  traverseP :: forall i o. LoopName -> p i o -> p (t i) (t o)

class Selective f => TraverseFlow f where
  traverseF :: LoopName -> forall i o. f (t i) -> ControlFlowF f i o -> f (t o)
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

### Looping

Ironically these are a little easier to think about than traversing arrows.

Haskellʼs [`ArrowLoop`{.haskell}](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Arrow.html#v:loop) suffers from the classic Haskell problem of requiring laziness to work.
It loops in the sense of “tying the knot”, instead of in the sense of `do … while`{.c} loops of classic control flow.

Instead, letʼs try something in the style of [`MonadRec`{.purescript}](https://pursuit.purescript.org/packages/purescript-tailrec/6.1.0/docs/Control.Monad.Rec.Class#t:MonadRec):

```haskell
class Profunctor p => ArrowWhile p where
  doWhileP :: LoopName -> forall i r. p i (Either r i) -> p i r

class Monad m => MonadRec m where
  tailRecM :: LoopName -> forall i r. (i -> m (Either r i)) -> i -> m r

class Selective f => WhileF f where
  doWhileF :: LoopName -> forall i r. f i -> ControlFlowF f i (Either r i) -> f r
```

Now you know the effects of `p`{.haskell} happen one or more times, so again, having idempotence of effects is nice, or having a Kleisli arrow is also sufficient.

In contrase to `traverseP`{.haskell}, `doWhileP`{.haskell} provides absolutely no guarantee of termination: it could happily produce `Left`{.haskell} every time.

Sometimes we can do a little better, and produce a fixed point.
For example, in selective applicative parsers as I have implemented them (in the [LR(1)]{t=} style, though this is not essential), the `leastFixedPointP`{.haskell} produces a named nonterminal to pass to the function, which then generates the rest of the grammar _for that nonterminal_ and returns it.
The output then uses the named nonterminal that has been incorporated into the grammar.

```haskell
class (Profunctor p, ArrowWhile p) => ArrowFixedPoint p where
  leastFixedPointP :: LoopName -> forall i r. (p i r -> p i r) -> p i r

class (Functor f, WhileF f) => FixedPointF f where
  leastFixedPointF :: LoopName -> forall r. (f r -> f r) -> f r
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

### Multi-Staged Functors and Arrows

For mixed static analysis, like `Haxl`{.haskell}.

```haskell
class Applicative f => MultiStage f where
  twoStages :: forall r. f (f r) -> f r
class Profunctor p => MultiStageP p where
  twoStagesP :: forall i r. p i (p i r) -> p i r

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

I understand the practicalities of making this argument, to make substantial progress in an existing codebase and ecosystem.

But I also believe we ought to investigate abstractions that bring these details to the forefront.
Because it is not just about benign effects, effects we can sweep under the rug.
It is also about details we care about: static analysis, control flow, program planning.
Letʼs expose them to the light of day.
:::

```haskell
-- A Haxl computation is either completed (Done) or Blocked on pending data requests
data Result a = Done a | Blocked BlockedRequests (Haxl a) deriving Functor
newtype Haxl a = Haxl { runHaxl :: IO (Result a) } deriving Functor
```

:::Warning
(This is the `Haxl`{.haskell} from the paper, much simplified from the concurrent implementation in the library.)
:::

It is such a common pattern, great for static analysis.
Letʼs abstract it a bit more:

```haskell
data RequestRespond w r m a = RR w (r -> m a)
instance (Monoid w, Applicative m) => Applicative (RequestRespond w r m)

-- This is the same carrier type as a free monad, but with different semantics!
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


## Monoids for Static Analysis

This is its own article, (Monoids for Static Analysis)[monoids_static_analsis.html], addressing the non-functor part of the picture.

The monoids for static analysis are important not just for static analysis, either ahead of time or interleaved with execution.
But their structure also reveals possibilities for the functors, patterns of code and control flow.


## Syntax

Letʼs think about syntax for selective applicative computations.

### Parsers

I think the most compelling is for parsers.
Not parser combinators, but parser grammars – Yacc-, Bison-, [Happy](https://haskell-happy.readthedocs.io/en/latest/introduction.html)-, [Menhir](https://cambium.inria.fr/~fpottier/menhir/)-style parsers.

Basically these parsers annotate a BNF grammar so each nonterminal comes with a type, and each production has a value in the host language.

For example, we can recognize a digit and return its value in the host language.
Then we can stitch those digits together to parse a decimal natural number.
This is an applicative-style parser.

```parser
digit :: Parser Natural
digit ::=
  | "0" { 0 }
  | "1" { 1 }
  | "2" { 2 }
  | "3" { 3 }
  | "4" { 4 }
  | "5" { 5 }
  | "6" { 6 }
  | "7" { 7 }
  | "8" { 8 }
  | "9" { 9 }

natural :: Parser Natural
natural ::=
  | d <- digit
    { d }
  | v <- natural; d <- digit
    { 10*v + d }
```

In PureScript `ado`{.purescript} notation, this translates to the following:

```purescript
digit :: Parser Natural
digit = empty
  <|> 0 <$ match "0"
  <|> 1 <$ match "1"
  <|> 2 <$ match "2"
  <|> 3 <$ match "3"
  <|> 4 <$ match "4"
  <|> 5 <$ match "5"
  <|> 6 <$ match "6"
  <|> 7 <$ match "7"
  <|> 8 <$ match "8"
  <|> 9 <$ match "9"

natural :: Parser Natural
natural = rec \natural ->
  let
    singleDigit = ado
      d <- digit
      in d
    moreDigits = ado
      v <- natural
      d <- digit
      in 10*v + d
  in singleDigit <|> moreDigits
-- desugars to
natural = rec \natural ->
  let
    singleDigit = (\d -> d) <$> digit
    moreDigits =
      pure (\v d -> 10*v + d)
      <*> natural
      <*> digit
  in singleDigit <|> moreDigits
```

What does selective control flow add?
Pattern matching, including guards!
This is related to the fact that selective control flow relates to selective rejection in parsers.

```parser
prime :: Parser Natural
prime ::=
  | v <- natural
    -- Reject any `v` that is not prime
    { True <- isPrime v }
    { v }

-- Assume this is a full string parser
-- that handle escaping and stuff
-- (heck, it could even evaluate
-- contexprs or such)
string :: Parser String

data Prefixed
  = Digit Natural
  | Natural Natural
  | Prime Natural

prefixed :: Parser Prefixed
prefixed ::=
  | "digit" <- string; ":";
    d <- digit
    { Digit d }
  | "natural" <- string; ":";
    v <- natural
    { Natural v }
  | "prime" <- string; ":";
    p <- prime
    { Prime p }
  -- any other value is a soft parse error
  -- as it could be handled elsewhere
```

```purescript
prime :: Parser Natural
prime = filter (\v -> isPrime v) natural

string :: Parser String

data Prefixed
  = Digit Natural
  | Natural Natural
  | Prime Natural

prefixed :: Parser Prefixed
prefixed =
  let
    parsePrefix :: Parser (Branches3 String String String)
    parsePrefix =
      -- discard matches that are not in one of the
      -- three branches
      filterMap identity ado
        s <- string
        in case s of
          "digit" -> Just (Branch1of3 s)
          "natural" -> Just (Branch2of3 s)
          "prime" -> Just (Branch3of3 s)
          _ -> Nothing
  in branch3 (parsePrefix <* match ":")
    (Digit <$> digit)
    (Natural <$> natural)
    (Prime <$> prime)
```

Detecting the pattern matching on `string` in the combinator enables turning it into a selective case branch.
Using the result of the `string` parser to directly inform which branch to take up front, instead of trying them one by one and backtracking.
(For this example it does not make much difference, of course.)

For length-prefixed data it is a little more complicated.
We could use a `while`{.parser} loop to encode it.

```parser
numbers :: Parser (Array Natural)
numbers ::=
  length <- natural; ":";
  n := { 0 };
  items := { [] };
  while { n < length }:
    item <- natural
    n := { n + 1 }
    items := { [...items, item] }
  end;
  { items }
```
```purescript
type LoopState =
  { length :: Natural
  , n :: Natural
  , items :: Array Natural
  }

numbers :: Parser (Array Natural)
numbers =
  let
    init :: Parser LoopState
    init :: ado
      length <- natural
      match ":"
      in { length, n: 0, items: [] }
    whichBranch :: LoopState -> Either (Array Natural) LoopState
    whichBranch state@{ n, length }
      | n < length = Right state
      | otherwise = Left state.items
    parseMore :: Parser (LoopState -> LoopState)
    parseMore = ado
      item <- natural
      in \state@{ n, items } ->
        -- Update the state
        state { n = n + 1, items = Array.snoc items item }
  in doWhileF init
    (ControlFlow whichBranch (Action parseMore) (Pure identity))
```

Or we could be a little more cheeky and do it as a traversal.
(This is probably the better choice.)

```parser
numbers :: Parser (Array Natural)
numbers ::=
  length <- natural; ":";
  items <- for _ of { replicate length () }:
    item <- natural
    { item }
  end;
  { items }
```
```purescript
numbers :: Parser (Array Natural)
numbers =
  traverseF
    ado
      length <- natural
      match ":"
      in replicate length unit
    natural
```

Both of these will parse lists like `3: 0 1 2`, `7: 9 23 43 1 52 44 95`, `1: 1`.
Maybe you want to guard it to be nonzero, use `0;` for the empty list:

```parser
numbers :: Parser (Array Natural)
numbers ::=
  | 0 <- natural; ";" { [] }
  | length <- natural; ":";
    -- Still need to guard that length is nonzero here
    { True <- length > 0 };
    items <- for _ of { replicate length () }:
      item <- natural
      { item }
    end;
    { items }
```



```xml {.skylighting}
<?xml version="1.0" encoding="UTF-8"?>
<!-- ```{.xml .skylighting}``` -->
<!-- https://docs.kde.org/stable5/en/kate/katepart/highlight.html -->
<language name="parser" casesensitive="1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="language.xsd">
  <highlighting>
    <contexts>
      <context name="Root" attribute="Normal Text" lineEndContext="#stay">
        <RegExpr attribute="Keyword" context="#stay" String="data|while|end|for|of|%[-a-zA-Z_][-a-zA-Z0-9_]*"/>
        <RegExpr attribute="Variable" context="#stay" String="\$[-a-zA-Z_][-a-zA-Z0-9_]*"/>
        <RegExpr attribute="Annotation" context="#stay" String="@[-a-zA-Z_][-a-zA-Z0-9_]*:?"/>
        <RegExpr attribute="Keyword" context="#stay" String="\s:\s|-&gt;|Π|Σ"/>
        <RegExpr attribute="Meta" context="#stay" String="…|&lt;[^][(){}&lt;&gt;]+&gt;"/>
        <IncludeRules context="FindOperators"/>
        <IncludeRules context="FindComments"/>
        <IncludeRules context="FindStrings"/>
        <RegExpr attribute="Data Type" context="#stay" String="[A-Z][-a-zA-Z0-9_]*"/>
      </context>

      <context name="FindOperators" attribute="Normal Text" lineEndContext="#stay">
        <RegExpr attribute="Operator" context="#stay" String="::|::=|&lt;-|;|\||="/>
      </context>

      <context name="FindComments" attribute="Comment">
        <StringDetect attribute="Comment" context="LineComment" String="--"/>
      </context>

      <context name="LineComment" attribute="Comment" lineEndContext="#pop">
      </context>
      <context name="BlockComment" attribute="Comment" lineEndContext="#stay">
        <StringDetect attribute="Comment" context="BlockComment" String="{#"/>
        <StringDetect attribute="Comment" context="ParenComment" String="(#"/>
        <StringDetect attribute="Comment" context="#pop" String="#}"/>
      </context>
      <context name="ParenComment" attribute="Comment" lineEndContext="#stay">
        <StringDetect attribute="Comment" context="BlockComment" String="{#"/>
        <StringDetect attribute="Comment" context="ParenComment" String="(#"/>
        <StringDetect attribute="Comment" context="#pop" String="#)"/>
      </context>

      <!-- FindStrings looks for single and double quoted strings -->
      <context name="FindStrings" attribute="Normal Text" lineEndContext="#stay">
        <DetectChar context="StringSQ" attribute="String SingleQ" char="'"/>
        <DetectChar context="StringDQ" attribute="String DoubleQ" char="&quot;"/>
      </context>

      <!-- StringSQ consumes anything till ' -->
      <context name="StringSQ" attribute="String SingleQ" lineEndContext="#stay">
        <!--no line continuation here-->
        <Detect2Chars attribute="Escape" char="\" char1="'"/>
        <Detect2Chars attribute="Escape" char="\" char1="\"/>
        <DetectChar attribute="String SingleQ" context="#pop" char="'"/>
      </context>

      <!-- StringDQ consumes anything till ", substitutes vars and expressions -->
      <context name="StringDQ" attribute="String DoubleQ" lineEndContext="#stay">
        <LineContinue attribute="Escape"/>
        <Detect2Chars attribute="Escape" char="\" char1="&quot;"/>
        <Detect2Chars attribute="Escape" char="\" char1="$"/>
        <Detect2Chars attribute="Escape" char="\" char1="\"/>
        <DetectChar attribute="String DoubleQ" context="#pop" char="&quot;"/>
      </context>
    </contexts>

    <itemDatas>
      <itemData name="Normal Text"    defStyleNum="dsNormal"/>
      <itemData name="Keychar"        defStyleNum="dsSpecialChar"/>
      <itemData name="Variable"       defStyleNum="dsVariable"/>
      <itemData name="Operator"       defStyleNum="dsOperator"/>
      <itemData name="Builtin"        defStyleNum="dsBuiltIn"/>
      <itemData name="Index"          defStyleNum="dsAttribute"/>
      <itemData name="Import"         defStyleNum="dsImport"/>
      <itemData name="Function"       defStyleNum="dsFunction"/>
      <itemData name="Data Type"      defStyleNum="dsDataType"/>
      <itemData name="Keyword"        defStyleNum="dsKeyword"/>
      <itemData name="Annotation"     defStyleNum="dsAnnotation"/>

      <itemData name="String SingleQ" defStyleNum="dsString"/>
      <itemData name="String DoubleQ" defStyleNum="dsString"/>
      <itemData name="Escape"         defStyleNum="dsSpecialChar"/>
      <itemData name="Meta"           defStyleNum="dsPreprocessor"/>

      <itemData name="Regular Expression" defStyleNum="dsSpecialString" spellChecking="false"/>

      <itemData name="Comment"        defStyleNum="dsComment"/>

      <itemData name="Error"          defStyleNum="dsError"/>
    </itemDatas>
  </highlighting>
</language>

```


### Haskelllikes

For functional programming languages, we donʼt have the phasing distinction of parse grammars versus host language.
Instead, the staging is represented by scoping, like for `ado`{.purescript} syntax in PureScript.

```purescript
prefixed = ado
  s <- string
  case s of
    "digit" -> ado
      d <- digit
      in Digit d
    "natural" -> ado
      v <- natural
      in Natural v
    "prime" -> ado
      p <- prime
      in Prime p
```

`case ... of ... -> ado ... in ...`{.purescript}

Variables are bound on the left side of `<-`{.purescript} (in `ado`{.purescript}) and the left side of `->`{.purescript} (in `case`{.purescript}) as well.
Actions are on the right side of `<-`{.purescript}.

The `ado`{.purescript} in the `case`{.purescript} statement(!) (not an expression) is just a syntactic reminder that it is still in a case combinators.

Already-bound variables are available in certain syntactic positions:
- the final `in`{.purescript}, which defines the return value, just like plain `ado`{.purescript} syyntax
- the scrutinee of the `case`{.purescript} statement (the expression whose value is inspected in the case branch matches)
- `let`{.purescript} statements in an `ado`{.purescript} block
- conditional guards in pattern matches, for selective rejection

Note that desugaring this is far from easy.
The compiler essentially needs to commit to an evaluation scheme for the pattern matches and encode it in the combinators.

### tmTTmt

That syntax is … alright.
It could work.

But I prefer the much different syntax I use in [tmTTmt](tmttmt_syntax.html).
It is losely inspired by horizontal bar notation.

Aside from the output `output`{.tmTTmt} being declared at the top(!) of the function,
the statement syntax follows the flow of the data better.^[Yes, it gets the value of `output`{.tmTTmt} from whichever branch was run: that variable leaks back into the outer scope.]

```tmTTmt
prefixed => output:
  string => {{
  | "digit":
    digit => d
    ["Digit" d] => output
  | "natural":
    natural => v
    ["Natural" v] => output
  | "prime":
    natural => p
    isPrime p =>? "True"
    ["Prime" p] => output
  }}
```

Now, this is not explicit like `ado`{.purescript}.
But like Haskell `do`{.purescript} notation with the [`ApplicativeDo`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/applicative_do.html) extension, it is possible to detect scoping to compile to the more restrictive Applicative (and Selective) combinators when possible.
