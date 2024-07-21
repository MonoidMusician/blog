---
title: Hereditarily Finite Sets
subtitle: A gentle, computational introduction to set theories/foundations <3
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

<script src="assets/js/hfs.js"></script>

<!--
<div data-hfs="0"></div>
<div data-hfs="1"></div>
<div data-hfs="2"></div>
<div data-hfs="3"></div>
<div data-hfs="4"></div>
-->

<script>
document.querySelectorAll("[data-hfs]").forEach(div => {
  if (!div.childElementCount) {
    div.appendChild(HFStoSVG(parseInt(...div.dataset.hfs.split("_")), {simple:!!div.dataset.simple}));
  }
});

document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll("[data-hfs]").forEach(div => {
    if (!div.childElementCount) {
      div.appendChild(HFStoSVG(parseInt(...div.dataset.hfs.split("_")), {simple:!!div.dataset.simple}));
    }
  });
});
</script>

> What is a set?

I think this is one of the more mysterious questions in math.

And itʼs funny, because math prides itself on asking precise questions *about* sets, on formulating rigorous questions (about other things) *using* sets.
But somehow itʼs hard to say what a set really means, what it captures and what it excludes.

Instead, I propose we start somewhere simpler: **hereditarily finite sets** (HFSes).

<div data-hfs="157842" data-simple="true"></div>

<div data-hfs="124875" data-simple="true"></div>

<div data-hfs="124875_9" data-simple="true"></div>

Donʼt mind the fancy words and fancier picture, I promise these sets are actually simpler!
I can tell you _all_ about what hereditarily finite sets are!

Theyʼre a great place to build intuition *and* rigour.
In particular, you can take your hard-earned skills from programming and apply them in this setting!
And I hope it can serve as a jumping-off point to talk about other aspects of set theory, type theory, and foundations of mathematics.

:::Note
This is a construction of finite set theory based on the most commonly used set theory: Zermelo–Fraenkel set theory, abbreviated ZF.
(Adding the Axiom of Choice to ZF gets you ZFC, the de facto choice for mathematics.)

There are other ways to present set theory, such as [von Neumann–Bernays–Gödel set theory (NBG)](https://en.wikipedia.org/wiki/Von_Neumann%E2%80%93Bernays%E2%80%93G%C3%B6del_set_theory), which formalizes the notion of classes more.

You can also present sets in a point-free fashion, specifying set theory only using [sufficient categorical properties](https://math.stackexchange.com/questions/3608426/can-we-build-set-theory-from-category-theory#answer-3608471).
:::

## What is a finite set? Hereditarily finite set?

Well, okay. What is a set?

A set is a Thing™ that has a membership relation.^[Specifically a Mathematical Thing™.]

A finite set is a set with only a finite amount of members.

This seems great, right?
Letʼs just explore a universe made of finite sets and call it a day.

Except this isnʼt as well-behaved as weʼd like;
finite sets are not suitable to make a theory out of.

Finite sets might themselves be finite, but nothing says the sets inside of them are also finite!

We really do want to talk about *hereditarily* finite sets, where the set itself *and all of its descendents* are finite.
That means the direct members, the members of those members, and so on, all the way down.
(And it can only go down so far – [it cannot go down forever](https://en.wikipedia.org/wiki/Axiom_of_regularity).)

:::Key_Idea
This is one of the strengths of set theory: the ability to keep dissecting sets based on the membership relation.
:::

## How do you model HFSes?

This is what I mean when I say I canʼt tell you what a set is: I donʼt know of a concrete model I can point to and say, “this captures the totality of sets”.^[Synthetic, analytic, …]

The situation is much different for hereditarily finite sets.
We can definitely do this!

<div data-hfs="44203"></div>

Hereʼs a graphic model Iʼve developed!
Visualizing the brackets of the textual representation as slopes in the visual representation.

But more importantly, you need to learn how to model HFSes in terms of familiar data types.
Lists, sets … and even plain numbers will do the trick!

### Lists all the way down

As an appetizer, letʼs consider the simplest system of semistructured data a programmer (say a LISPer) would invent.
Lists are great, so why not create a type system of only lists?
Lists of lists of lists of lisps of lists of …

Of course when we are programming, we consider finite lists, and since thatʼs all we have, we end up with hereditarily finite lists by default.

Iʼll give a few different definitions of it just so you really get the idea!

```haskell
newtype HFL = Layer [HFL]
-- or as a GADT:
newtype HFL where
  Layer :: [HFL] -> HFL
  deriving ( Eq, Ord )

orderedMembers :: HFL -> [HFL]
orderedMembers (Layer theOrderedMembers) = theOrderedMembers

empty :: HFL
empty = Layer []
```

(please substitute your favorite strict lists/arrays instead!)

```purescript
newtype HFL = Layer (Array HFL)

derive instance eqHFL :: Eq HFL
derive instance ordHFL :: Ord HFL

orderedMembers :: HFL -> Array HFL
orderedMembers (Layer theOrderedMembers) = theOrderedMembers

empty :: HFL
empty = Layer []
```

I swear this is _just_ data!
See, we can derive `Eq`{.haskell} and `Ord`{.haskell} just like any other datatype!
And they are well behaved instances, I promise there is nothing strange or suspicious going on there!
Itʼs just recursion like you would do for many other data types: like ASTs for source code, whatever.

### Sets all the way down

Now letʼs replace lists with sets!

```haskell
newtype HFS = Layer (Set.Set HFS)
-- or as a GADT:
newtype HFS where
  Layer :: Set.Set HFS -> HFS
  deriving ( Eq, Ord )

members :: HFS -> Set.Set HFS
members (Layer theMembers) = theMembers

member :: HFS -> HFS -> Boolean
member inner outer = Set.member inner (members outer)

empty :: HFS
empty = Layer Set.empty
```

```purescript
data HFS = Layer (Set.Set HFS)

derive instance eqHFS :: Eq HFS
derive instance ordHFS :: Ord HFS

members :: HFS -> Set.Set HFS
members (Layer theMembers) = theMembers

member :: HFS -> HFS -> Boolean
member inner outer = Set.member inner (members outer)

empty :: HFS
empty = Layer Set.empty
```

Now we can define the most important part of sets: the set membership relation `member :: HFS -> HFS -> Boolean`{.purescript}.

You can define this for HFLs too, but it isnʼt as well-behaved: it doesnʼt *characterize* the HFLs the way it does for HFSes.
More about this later.

:::Note
I am always going to qualify references to the [`Data.Set`{.haskell}](https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html)/[`Data.Set`{.purescript}](https://pursuit.purescript.org/packages/purescript-ordered-collections/3.1.1/docs/Data.Set) modules.
This is to distinguish “sets the data structure containing ordered values” from “hereditarily finite sets”.

Just consider how the type of the `member`{.purescript} relation differs: for `Data.Set` it has type `k -> Set.Set k -> Boolean`{.purescript} (a heterogeneous relation) but for HFSes it is of type `HFS -> HFS -> Boolean`{.boolean} (a homogeneous relation).

Since weʼll be using HFSes to be our universe of set theory, “set” (within the set theory) will simply mean HFS, but itʼs a different thing than the data structure used to recursively build them.
:::

#### `Eq`{.purescript} and `Ord`{.purescript}

Itʼs time to fess up to my first sleight of hand already.

I derived `Eq HFS`{.purescript}.
This is **great**, weʼll talk more about what it means later.

I also derived `Ord HFS`{.purescript}.
You can think of it as an implementation detail, for now!
Sets arenʼt supposed to be inherently ordered^[In both senses: (1) having their members given in a structural order that can be varied, which is very bad, and (2) being globally ordered with respect to each other, which is alright but not always something you can assume in set theories.] … but we _can_ order them, and we _will_.

We are forced to order them because of how `Data.Set`{.purescript} works.
In return, this nice implementation ensures that we really have canonical representations of our sets, so that `Eq HFS`{.purescript} works exactly how we want.
And it makes it relatively efficient.

### Syntax

`{ {}, {{}}, { {}, {{}} } }`{.erlang}

Every HFS looks like this!
We can just print them out!

### There, there be dragons

The dragons arenʼt here!
We simply didnʼt let them into the universe of hereditarily finite sets.

But when you look at ZFC, some strange things happen.

#### Infinite sets

In normal ZF, there is an infinite set: the set of natural numbers.
From this, you can build larger and larger infinite sets, such as by taking power sets, or sets of functions.
(You can also introduce even larger infinities by adding them as axioms – [large cardinal axioms](https://en.wikipedia.org/wiki/Large_cardinal).)

These are inconvenient for our theory for two reasons: they ruin the nice computational aspects we were using, and yeah.

#### Zombie sets\*\*

_\*\*not a technical term_

The Axiom of Choice, added in ZFC (which, again, is accepted by most mathematicians), makes for some weird sets.

In _theory_, given infinite time, a computer could pick apart a set in ZF and take it down to familiar, recognizable, printable notions (as long as you have infinite patience and infinite paper).

Not so in ZFC: some sets are just magicked into existence with choice, but donʼt really have a way to be picked apart in this manner.
They are impenetrable except for the property you asked that they satisfy.
(Obviously in some cases that property reveals enough to pick them apart, but not always!)

So yeah, it is kind of weird.

Itʼs cool! but weird

So we banished them from our hereditarily finite set theory, where everything is concrete and computational.

#### Atoms

Some set theories also introduce “atoms” that are embedded in sets but donʼt act as sets themselves.
The atoms are given meaning outside of the set theory.
This turns out to be very useful in the study of set theory, but is pretty weird otherwise.

Weʼre here to understand sets all the way down, not sets whose decomposition can get stuck at atoms.

### `HFS`{.purescript} is a countably infinite type

There are a number of ways to see this.

The compositional explanation is that we have nothing but sets, and sets of a countable type are countably infinite, so by induction `HFS`{.purescript} is countable too.

If you think about it with your programming hat on, `HFS`{.purescript} is (de)serializable, and all serializable types are countable.

This is like constructing an injection into the natural numbers.
(Bit/bytestrings are also countably infinite, or whatever your model for serialization is.)

Bit we can do better!

We can encode sets directly in the bits of a natural number:

```purescript
encode :: HFS -> Natural
encode (HFS itsMembers) =
  sum (Set.map encodeMember itsMembers)
  where
  encodeMember m =
    pow 2 (encode m)

decode :: Natural -> HFS
decode = search mempty 0
  where
  search acc _ 0 = acc
  search acc idx n =
    let
      acc'
        -- if the bit at `idx` is set,
        -- add it to the result
        | odd n = Set.insert (decode n) acc
        | otherwise = acc
    -- Look at the next bit
    in search acc' (idx+1) (n/2)
```

In fact, if we use bigints, we could probably use `Natural`{.purescript} as our representation of HFSes.
The main problem is that this numeric encoding is primarily efficient for densely populated sets, which is not what we expect to see.
As soon as you start imposing structure on your sets, they will get very spare within the space of all sets.
For example, consider functions.

## Structure

Letʼs dive into the structure of what this gets us.

### Equality

(This section should be part of logic?)

The other fundamental relation on sets.

Useful because everything preserves equality.

#### Set extensionality

If two sets have the same members, they are equal sets!

#### Leibnizʼs identity of indiscernables

Two sets should be equal when we cannot distinguish between them.
An since we can only talk about membership in sets, set extensionality is exactly the thing.

Although we might ask why only one direction of the membership relation …

### Constructions

#### Numbers


##### Von Neumann ordinals

This is the well accepted definition of ordinals!

```purescript
-- Injection
-- Order preserving?
ordinal :: Natural -> HFS
ordinal 0 = empty
ordinal n =
  let prev = ordinal (n-1) in
  prev `union` singleton prev

unOrdinal :: HFS -> Maybe Natural
unOrdinal set =
  let
    value = size set
    -- We just need to check that all of the members
    -- are valid ordinals too and are strictly
    -- less than this value, since that guarantees
    -- they must be exactly zero through (value-1)
    -- since they are distinct
    isValid = members set # all \member ->
      let
        subValue = unOrdinal member
      in isJust subValue && subValue < Just value
  in if isValid
    then Just value
    else Nothing
```

The fun thing about this, is that if you want the \((<)\) relation, it is given by membership.

\[\llbracket x\rrbracket_{\omega} \in \llbracket y\rrbracket_{\omega} \iff x < y.\]

This lets you prove that \((<)\) is a well-founded ordering on natural numbers by the proof that \(\in\) is well-founded for sets.

##### Zermelo ordinals

This is actually a simpler definition.

```purescript
ordinalZ :: Natural -> HFS
ordinalZ 0 = empty
ordinalZ n =
  let prev = ordinalZ (n-1) in
  singleton prev

unOrdinalZ :: HFS -> Maybe Natural
unOrdinalZ set = case size set of
  0 -> Just 0
  1 | Just member <- unSingleton set ->
    map (1 + _) (unOrdinalZ member)
  _ -> Nothing
```

It has worse performance computationally (not that mathematicians care about that), and the membership relation only models predecessor.

\[\llbracket x\rrbracket_{Z} \in \llbracket y\rrbracket_{Z} \iff x+1 = y.\]

#### Ordered pairs

#### Functions

### Is This Data?

Codecs

https://pursuit.purescript.org/packages/purescript-codec/6.1.0





## Logic

asdf



### Predicates

```purescript
-- Semantic definition
type Predicate = HFS -> Boolean
```




### Relations

Relations are like predicates but they inspect _two_ sets at once, before saying true or false.

That is, they sort of tell you whether they are _related_ **according to the particular relation**.
But the emphasis is on the particular relation: relations can be anything, they donʼt have to model what we would think of as relationships, thereʼs nothing intrinsic to them.

:::Note
Notation for them is a bit funny.

\(x\ R\ y\)

\((x, y) \in R\)
:::

```purescript
-- Semantic definition
type Relation = HFS -> HFS -> Boolean

relationAsPredicate :: Relation -> Predicate
relationAsPredicate rel = \set ->
  case unPair set of
    Nothing -> false
    Just (Tuple left right) -> rel left right

predicateAsRelation :: Predicate -> Relation
predicateAsRelation pred = \left right ->
  pred (pair left right)

-- `predicateAsRelation (relationAsPredicate r) = r`
-- `implies (relationAsPredicate (predicateAsRelation p)) p`

exampleRelations :: Array Relation
exampleRelations =
  -- We can have the relation that is constantly true
  [ tt
  -- Short for this
  , \_ _ -> true
  -- Same with false
  , ff
  , \_ _ -> false
  -- Membership is a relation
  , member
  -- So is equality
  , eq
  ]
```




## Glossary

set

:   A set is a mathematical object where there is a membership relation relationship between sets.
    Being a mathematical object, there is an equality relation between sets too.
    Via extensionality, the membership relation characterizes equality exactly: sets with the same members are equal.
    (This is important since they would behave the same from the perspective of set theory anyways.)

class

:   A class is one step up from a set, it is a “large set”.
    Basically it is predicates on sets? I need to revisit this.

    There is no set of all sets, but there is a class of all sets.
    Set theorists donʼt typically stack this any higher, but you could have a collection of all classes.
    (Type theorists are happy to assume infinite universes of types.)

collection

:   A purposefully informal word to describe things like sets and classes, things which have some form of membership relation.
    This is because sets have a specific placement in set theory (and classes, sort of).
    So sometimes we need other words that donʼt already have that association.


universe

:   A model of set theory.
    In our case, this is the type `HFS`{.purescript}.




## Digressions

Why do mathematicians only use \(f : X \to Y\) notation for functions?
Why donʼt they use \(f \in X \to Y\)?
They do believe in sets of functions, right?

I think part of the reason is that they consider at least the codomain as living outside of the data of the function, so it needs to be ascribed.
But idk, maybe itʼs just a historical quirk.
