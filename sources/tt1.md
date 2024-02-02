# Assumptions
Type theories generally follow similar frameworks, so weʼll assume some basics here.
The mathematical study of type theories is, in one sense, the study of starting with a common agreement for what a basic type theory looks like, and how various choices on top of that interact.
For instance, many choices are in conflict: assuming Axiom K and Univalence would together produce an inconsistent theory – but the assumption when studying type theories is that they want to be consistent.
There are other choices to be made, such as whether there is an impredicative universe.
My favorite example of this is the fact that parametricity (a very nice property of polymorphic functions) fails to hold exactly when excluded middle holds (a convenient property for proofs), at least inside a univalent foundation: https://homotopytypetheory.org/2016/02/24/parametricity-and-excluded-middle/.

<!-- ʼ Π ≡ ℕ → λ Σ ≃ -->

Anyways, for our purposes here, letʼs choose a fairly straightforward type theory – letʼs not make tough choices.
We will have a hierarchy of universes `Type 0 : Type 1 : …`, all of which are predicative and proof-relevant (those are the straightforward choices).
We have pi types and dependent functions, where pi types lie in the largest universe of the input and output: `(Π (a : T : Type n), (P a : Type m)) : Type (max n m)`.
And we will have dependent inductive types – thus we can define sigma types pretty easily:
```lean
inductive Sigma {T : Type n} (P : T -> Type m) : Type (max n m)
| mk : Π (a : T), P a -> Sigma P
```

All inductive types are defined by giving their constructors, and then we receive corresponding recursion/induction principles (used to pattern match/eliminate from a datatype) and computation principles (used to tie the two together).

Thatʼs pretty much all we need!
Oh, oops, I forgot the most interesting part: equality!

```lean
inductive eq {T : Type n} : T -> T -> Type n
| refl : Π {x : T}, eq x x -- notated x = x
```

Again, the choice that equality lives in the same universe as its arguments is the most straightforward choice.
Another choice would be to make it live in `Type 0`, but that gets in the way of univalence!

# Consistency

Now letʼs talk about consistency.
Consistency is a mathematical property, who actually cares, right??
Nope!
Consistency is great to talk about computationally.
Let me show you.

What is consistency?
In a consistent formal system, you should only be able to prove things that are true, not things that are false.
Whatʼs false?
False is defined to be an empty data type (think `Void` in Haskell), this means that if you “pattern match” on it, you have zero cases to prove, and so you can prove anything from it (this is called _ex falso quodlibet_ or the Principle of Explosion or `absurd`).
Additionally, anything that implies false is considered to be false, and anything that implies everything is false, so this is why we sometimes define false as a type which implies everything else, or `forall a. a`.

This last part is the most important: if we were able to determine that *every type* has an inhabitant, that poses a problem mathematically: the theory is inconsistent, it means false is true!
But computationally it also poses a problem: what if we "had" a value of type `forall a. a`?
Would it be possible to compute that value??
No!

```haskell
undefined :: forall a. a
undefined = undefined
```

Thereʼs no way to extract a value of a type, say `Int`, from that – it just sits and spins in an infinite loop.

So in this way, computation and consistency are related in the sense that they both ban infinite loops of this kind, which are essentially meaningless.

# Intuitive Univalence
## (hopefully)
I will try to aim this section at programmers, especially those with a Haskell-like background.

Let me start out by defining two functors in both Haskell and GADT/inductive-like syntax:
```haskell
data List a = Nil | Cons a (List a)
data Tsil a = Lin | Snoc (Tsil a) a
```

```lean
inductive List (a : Type) : Type
| nil : List a
| cons : a -> List a -> List a

inductive Tsil (a : Type) : Type
| lin : Tsil a
| snoc : Tsil a -> a -> Tsil a
```

(Each has two constructors, a nullary one and one that adds an element to an existing list/tsil.)

Whatʼs the difference between these?

One answer is that they have different associativities: `List` naturally associates to the right, and `Tsil` the opposite.
A more obvious answer is that they have different names.
But what does it matter?
Any code that uses one could easily be changed to use the other, with no observable differences.

There are many ways that we could prove the equivalence: we could keep the source order and switch the associativity, or we could swap the order and keep the associativity.
It turns out that the mathematical idea of a bijection or equivalence captures this: a function with an inverse.

The latter equivalence (order-reversing) is captured by the two following functions:
```haskell
list2tsil :: forall a. List a -> Tsil a
list2tsil Nil = Lin
list2tsil (Cons a as) = Snoc (list2tsil as) a

tsil2list :: forall a. Tsil a -> List a
tsil2list Lin = Nil
tsil2list (Snoc as as) = Cons a (tsil2list as)
```

Note that we can read these functions as source transformations, even:
- Take every occurrence of `Nil` and replace it with `Lin`, and vice-versa.
- Take every occurrence of `Cons` and replace it with `(flip Snoc)`, and vice-versa.

(Aside: It turns out that this is just inlining the definition of the functions, and is a form of partial evaluation that functional programming languages are great at.)

The other way is by composing these functions with reversing functions, which take a list and flip it around.
Iʼll omit it since it takes a couple more lines to define and is best explained intuitively, rather than in code.

This reversal would ensure that the cons lists read the same left-to-right when converted to snoc lists, e.g. `Cons 1 (Cons 2 (Cons 3 Nil))` and `Snoc (Snoc (Snoc Lin 1) 2) 3`, but at the cost of flipping the associativity: either the `1` or the `3` will be closed to the root in this example.
Perhaps one might say that the isomorphisms that flip the order from the computerʼs perspective are the opposite of the ones that flip the order from the humanʼs perspective.
Which one is right?
Well, theyʼre both right!
Types can be equal in more than one way!

## Univalence as program transformation
I already alluded to it above, but I would like to make more explicit the idea that we can think of univalence as rewriting programs to operate on a different type than originally thought.

Letʼs take a simple function:
```haskell
diagonal :: List a -> List (List a)
diagonal Nil = Cons Nil Nil
diagonal (Cons a as) = Cons (Cons a as) (diagonal as)
```

By univalence, we can create a similar function that operators on `Tsil`s instead.
How would that look like?
Well, we actually need to pick which isomorphism we are going to use; letʼs use `list2tsil`/`tsil2list`.
For this particular type, we can represent it as a composition, or explicitly write it out:
```haskell
lanogaid = map list2tsil <<< list2tsil <<< diagonal <<< tsil2list

lanogaid :: Tsil a -> Tsil (Tsil a)
lanogaid Lin = Snoc Lin Lin
lanogaid (Snoc as a) = Snoc (lanogaid as) (Snoc as a)
```
We notice that we start by applying `tsil2list` to the input – this corresponds to replacing the pattern matching on `List` with the corresponding pattern matching for `Tsil`.
Then once we get the output of `diagonal`, we need to apply `list2tsil` twice to transform `List (List a) -> Tsil (Tsil a)` (once on the outside, and mapping it on the inside).

This program transformation 

It turns out that if we want to use the reversing isomorphism, we can write the result neatly too:
```haskell
lanogaid :: List a -> List (List a)
lanogaid Nil = Cons Nil Nil
lanogaid (Cons a as) = Cons Nil (map (Cons a) (lanogaid as))
```
(Instead of adding the elements on the head of cons, we wait to add it to the tail, so now we have a larger list each time, instead of a smaller list each time.)

Do you think that you could applying the program transformation for _any_ isomorphism applying to _any_ function?

Well, thatʼs what univalence tells us: we can rewrite any program to use any equivalent type, and isomorphisms are how we prove that two types are equivalent.
Itʼs actually much stronger than programs as we think of them computationally:
All properties will be preserved this way, too, even equalities, once you translate them along their types.

The setup is a little complicated to state formally, but say weʼre given a program of type `F` which mentions some type `T`, and we want it to instead work with type `D`, given an isomorphism `f : T -> D` and `g : D -> T` – how do we do that practically?
It turns out, we just have to follow the types!
If `F'` takes as an input an argument of type `D`, then we just need to apply `g` to that argument to get something equivalent of type `T` to apply in `F`.
If `F'` gives as an output an argument of type `D`, we can get it from the corresponding output of type `T` in `F` by applying `f`.
This was just for functions, but for each functor we can also give an argument why it works:
It might require `map : (a -> b) -> F a -> F b` or `contramap : (b -> a) -> F a -> F b` or `invmap : (a -> b) -> (b -> a) -> F a -> F b` to make it work, or in fact it may only work when we have a true isomorphism (if we need to transfer a proof of equality), but for any type of functor you can define, you can make it work.

Now, maybe it seems hard to believe that _every_ `F` will follow these rules.
This is why we often take univalence to be an axiom.
But any `F` you write down will actually follow these rules, and this means that it should be possible to give a computational interpretation to univalence.


# Univalence vs set theory: where does equality come from?
I just have to write on this topic, itʼs been on my mind a lot.
It is more philosophical and focuses on high-level concepts, but it also assumes a little more background in set theory than the rest of this discussion.

Sets are nice in set theory because they are ridiculously lightweight and universal: everything is a set, with the empty set at the bottom of it all, sets can be constructed out of almost any predicate and contain almost any element (though at some point, and Iʼm never clear when exactly it occurs, one must use “classes” instead of sets, to avoid the usual inconsistency issues), and elements arenʼt constrained to only belong to one particular set, but can instead be used in contexts where different types \[sic.\] of things are expected (as long as you can prove they belong, of course).
And furthermore, equality is very obvious: two sets are equal when they contain the same elements.
But wait up – is that really so obvious?

The first issue with the definition of set equality seems to be that it must be recursive, since sets contain other sets as elements.
But this is sound, since there is only one set that is empty, and it (the one and only empty set) is obviously equal to itself, so we can start there and build our way up to real sets.

The real problem with set equality, though, is that set equality requires that its elements exist as independent entities with a global notion of equality.
It isnʼt a problem _within_ set theory, of course – set theory is consistent (presumably).
But it _is_ a problem _with_ set theory itself, one possible theory among other foundational theories we can choose.
Iʼll say it one more time, I think itʼs a good quote:

> **The biggest problem with set theory** is that equality of sets requires that its elements exist as independent entities (sets themselves) with a global notion of equality.

This global notion of equality gets in the way of identifying sets in natural ways.
Sets that, by rights, ought to be _the same_ (such as sets of three arbitrary but distinct elements), end up being doggedly unable to be identified only because their _elements_ have different identities _globally_.
Itʼs almost like a Platonic ideal of sets as universal objects is baked into the very foundations of set theory, but it couldnʼt be carried through to the end: the sets exist as universal objects, but the details of their construction were foregrounded, and the concepts that they were supposed to represent were cast in deep shadow, almost eclipsed from sight altogether.

I claim that type theory can fix this.
How does it do that?
It actually makes things stricter, in order for them to be looser: an element can only belong to one type now (and which type it belongs to must be syntactically obvious), but now equality only has to be local, within the type.
By making smaller claims about what our types represent, calling them merely local choices in one universe, we open up the system to greater universality in the abstract: knowing that each type is only a local choice makes us free to acknowledge that there are other choices out there, other perspectives, which are also real and valid, even though ours is a true and complete image of the universal concept by itself.

This might seem paradoxical, or ironic at least, but it really isnʼt.
Some kind of compromise must be made, and by retracing oneʼs steps, back up before stepping forward again, one can find their way around the wall and get un-stuck.
So here the decision is that, in order for equality of types to be universal, where like things can really be substituted for each other anywhere.

## Univalence to the rescue!
What is Univalence?
Univalence gives meaning to equality between types (“universe/typal extensionality”) by saying that any two isomorphic types are equal.
This is great for the sematics of category theory, where everything should work up to isomorphism, and this fact is reified in type theory.

It is the very nature of the strict locality of the type system that makes this feasible.
You canʼt distinguish between isomorphic types, so why not make them equal?
In this way, univalence is the maximal(?) expansion of the notion of equality that was hinted at from the beginning: all statements made internally to type theory are consistent with univalent swapping of isomorphic sets.
(Of course, there are certain axioms that can be assumed that contradict that, like Axiom K, which implies that each type only has one equality with itself.)

Proof relevance of equality is necessary here, because once it is no longer obvious how two objects are related (i.e. global identity), it must be specified how they are related.
This is another case where set theory falls short: by having such a weak notion of equality, it forces every equality to be “obvious”.
In constrast, the rich HoTT equality, which captures isomorphisms, allows for both obvious isomorphisms (such as the identity, which means that the types are definitionally equal) and more intricate isomorphisms.

I would say that the richness of considering equality as isomorphisms is essentially motivated by the question: how do I relate two things which seem different?
It would seem very weird, and arbitrary, to designate one equivalence relationship as primary, and all others as extraneous, when one doesnʼt even have an obvious relationship between two (syntactically different) types.
But then it also means that a type may be related to itself in myriad ways – even where there is an obvious equality, there must be others.
It suggests that equality isnʼt as obvious as we have always pretended it to be, and we must consider every relationship carefully.

Type theoretic equality is locally grounded in its type, and so comparing elements between types that are equal actually requires knowledge of how to relate elements of those types, which is exactly what is captured in the proof of equality.

It may seem absurd the implications of this idea, but I think it is very valuable to consider it as a new perspective.
It doesnʼt invalidate what we already know, but suggests there may be more nuance there than meets the eye.
For example, all finite types with size `n` are isomorphic to each other in `n!` ways.
Thatʼs alright, but not consider this: all _countable_ types are isomorphic (equal) to each other!
Okay, that actually wasnʼt so surprising, but what if I phrase it this way: there is only **one** type with countably infinite cardinality.
Almost all of the basic types we consider in daily programming life, are finite, or countable: natural numbers are countable, strings are countable, lists of strings are countable, json (along with just about any other data format) are countable, and theyʼre all the same type!

I want you to sit with that cognitive dissonance for a bit.

All countable types are the same.

So whatʼs the difference, what gives??

Each countable type is merely a representation of some ur-countable type, and they really canʼt be distinguished from each other in any significant way, but they do provide different representations of a countable type, and these different perspectives make working with them different experiences.
We get to make the choice to work with data in whatever form is most convenient for the task at hand, while recognizing that it is the same as any number of similar definitions, and we can exploit this sameness if we find a meaningful isomorphism between two perspectives.

Make no mistake: most isomorphisms wonʼt yield meaningful insights, but hopefully I have convinced you that having them all available to prove types equalities yields a very nice theory.