These are some of the insights Iʼve been musing over by viewing counting and probability through the lens of type theory.
The first part will focus on modelling counting using types, thus the emphasis will be on type-level operations.
The second part will model probability as a monad, focusing on value-level operations, guided by types.

I hope to include more introductory materials on type theory, here and in the above document.
Note that type theory is usually computable, with strong normalization properties, but it doesnʼt have to be, and in particular, the axiom of choice and the law of excluded middle break computability, for the usual reasons.
(Iʼll refrain from wading into constructivity issues, for your sake and mine.)
Similary, type theory is usually very explicit, but the verbosity can be reduced, such as by omitting proofs and inferrable (implicit) arguments.
These conventions bring type theory in line with the way mathematics is conventionally done.

<!-- ʼ -->

# Type arithmetic and counting
The ability of types to model arithmetic is well-known.
[The algebra (and calculus!) of algebraic data types](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types), by Joel Burget, is a good primer in the basic operations of arithmetic at the type level.
Product types result in the product of the cardinalities, sum types in the sum, quite naturally, and functions have the cardinality of the exponential of the output (base) to the input (power): `|A -> B| = |B|^|A|`.
Many of these results are known in set theory too (where product types are interpreted as cartesian products, sum types as disjoint unions), but I think type theory is a more natural setting for them, for reasons I wonʼt go into here.

When we add in dependent types (types that depend on values), we get richer capabilities by representing predicates, propositions, and proofs.
We can form subtypes using `Sigma` like above.

## Counting using types
The goal is to model each important combinatorial function using an appropriate type combinator.
In fact, we will see that this approach actually models the sematics of the objects we are counting in a rich way.

### How to count

First: what do we mean by counting the elements of a type?
Letʼs start with defining a type with a specified number of elements:
```lean
-- `fin n` A type that has `n` inhabitants
def fin : ℕ → Type := λ(n : ℕ), Σ(m : ℕ), m < n
```

It is defined as dependently-typed function that takes a natural number `n` to a subtype of the natural numbers which are less than `n`.
This means that `fin 0` is a type with no inhabitants, and `fin 1` only has one (`⟨0, zero_lt_one⟩`, where the angle brackets denote constructing an element of the subtype with value `0` and `zero_lt_one : 0 < 1` the proof required by the subtype), etc.
In order to count the inhabitants, we want to be able to say that a given type is isomorphic to `fin n` for some `n`:

```lean
@[class] structure fintype (t : Type) :=
(card : ℕ)
(equiv_fin : trunc (t ≃ fin card))
```
(original: https://github.com/leanprover-community/mathlib/blob/d5de80376088954d592a59326c14404f538050a1/src/data/fintype.lean#L14-L20)
(Notes on what finite means constructively: https://ncatlab.org/nlab/show/finite+set)

This is a typeclass; without getting too deep into it, it provides a way to access canonical members of a specific type: in this case, the cardinality `card t` of a type `t` along with a bijection that proves that `t` has as many elements as `fin (card t)` (`trunc` here just means that we donʼt get to inspect the details of the bijection, we can only prove results that are independent of its particular value).
Note that `card t` is unique for each type `t`, if it exists, so we are more or less justified in abusing notation and treating it as a function `Type → ℕ`, ommitting the proof, like in conventional math notation.
(There are type-theoretic reasons why this is okay, regarding inferring typeclass instances and their uniqueness, but I wonʼt go into details here.)

```lean
notation `|` t `|` := card t
```

### Algebraic types
I will state the corresponding types and propositions without any proofs:

```lean
structure prod (α : Type) (β : Type) : Type :=
(fst : α) (snd : β)

notation α × β := prod α β

lemma card_prod (α β : Type) [fintype α] [fintype β] : |α × β| = |α| * |β| .

inductive sum (α : Type) (β : Type) : Type
| inl {} (val : α) : sum
| inr {} (val : β) : sum

notation α ⊕ β := sum α β

lemma card_sum (α β : Type) [fintype α] [fintype β] : |α ⊕ β| = |α| + |β| .

-- functions α → β are built-in and so need no definition

lemma card_exp (α β : Type) [fintype α] [fintype β] : |α → β| = |β| ^ |α| .
```
(definitions adapted from https://github.com/leanprover-community/lean/blob/6ed0977fcbc14354c86240cec7f8ef20842d0b5c/library/init/core.lean)

It turns out this is about all there is for algebraic data types, they all can be made out of these basic building blocks (products, sums, expontentials); we will need a more powerful type system to encode more combinatorial functions.

### Dependent types
(a.k.a. Propositions, predicates, and proofs!)

#### Subtypes of functions

With dependent types, we can start to characterize various subtypes of functions and their numbers:
```lean
def injective {α β : Type} (f : α → β) : Prop := ∀ (a₁ a₂ : α), f a₁ = f a₂ → a₁ = a₂

notation α ↪ β := Σ(f : α → β), injective f

def surjective {α β : Type} (f : α → β) : Prop := ∀ (b : β), ∃ (a : α), f a = b

notation α ↠ β := Σ(f : α → β), surjective f

def bijective {α β : Type} (f : α → β) := injective f ∧ surjective f

notation α ↔︎ β := Σ(f : α → β), bijective f
```
(types from https://github.com/leanprover-community/lean/blob/6ed0977fcbc14354c86240cec7f8ef20842d0b5c/library/init/function.lean)

It turns out that injective functions are counted by [the falling factorial, `n P k`](https://en.wikipedia.org/wiki/Permutation#k-permutations_of_n), with arguments switched like the exponential case:
```lean
def F : ℕ → ℕ
| 0 := 1
| (n+1) := (n+1)*F n

notation n `!` := F n

def P : ℕ → ℕ → ℕ := λ(n k : ℕ), if n ≥ k then n! / (n-k)! else 0

lemma card_injective (α β : Type) [fintype α] [fintype β] : |α ↪ β| = P |β| |α| .
```

Surjective functions donʼt have such a well-known function counting them, but using [Stirling numbers of the second kind, S(n,k)], they are counted by `k!*S(n,k)`, which I will call `SP(n,k)` (this time the arguments are in the same order as the type constructor):
```lean
def S : ℕ → ℕ → ℕ
| 0 0 := 1
| 0 (k+1) := 0
| (n+1) 0 := 0
| (n+1) (k+1) := S n k + (k+1) * S n (k+1)

def SP : ℕ → ℕ → ℕ := λ(n k : ℕ), k! * S n k

lemma card_surjective (α β : Type) [fintype α] [fintype β] : |α ↠ β| = SP |α| |β| .
```

Bijective functions are comparatively simple, Iʼll let `B n k` count them:
```lean
def B : ℕ → ℕ → ℕ
| n n = F n
| _ _ = 0

lemma card_bijective (α β : Type) [fintype α] [fintype β] : |α ↔︎ β| = B |α| |β| .
```

#### Quotients of functions
You might have noticed that weʼre still missing one of the most important combinatorial functions: [the binomial coefficients, `n C k`](https://en.wikipedia.org/wiki/Combination)!
Can we find a suitable datatype to model it?

We can, but it is harder to use subtypes; instead, weʼll need to use another way to reduce the quantity of elements in a base type: quotient types (which are named after their tendency to divide the cardinality of a type, as in group quotients, though it is not always so simple).

(Need to explain how quotient types work in type theory. Mostly itʼs quite similar to set theory, though.)

For the binomial coefficients, weʼll quotient the injective functions by a relation based on permutations of the input:
```lean
def permutation_related {α β : Type} (f g : α ↪︎ β) : Prop :=
  -- compose the underlying functions, not the whole subtype
  ∃(h : α ↔︎ α), f.fn = g.fn ∘ h.fn

def img_from (α β : Type) := Quotient (α ↪︎ β) permutation_related

-- nonstandard notation:
notation α ↬ β := img_from α β

def C : ℕ → ℕ → ℕ := λ(n k : ℕ), P n k / k!

lemma card_img (α β : Type) [fintype α] [fintype β] : |α ↬ β| = C |β| |α| .
```

Because of the nice properties of actions of the group of permutations, it turns out that `|α ↬ β| = |α ↪︎ β| / |α ↔︎ α|`, which is exactly what we wanted: `C |β| |α|`.
(I believe a proof of this would involve showing that `permutation_related` is an equivalence relation that relates each `f` to exactly one `g = f ∘ h` for each `h : α ↔︎ α`, but I havenʼt worked through the details.)

This is all very nice, but `img_from α β` seems a bit odd – what is it really??
It turns out that it is the type of subsets of `β` which are equinumerous with `α`, encoded as an injection `α ↪︎ β` with knowledge of the specific mapping from `α` to `β` hidden, although the image of the function is still available (in fact, the only information available).
This suggests a different encoding, closer to the first wording about subsets of `β` which are like `α`:
```lean
def img_from' (α β : Type) := Σ(p : β → Bool), trunc (α ↔︎ (Σ(b : β), p b = True))
```
That was too abstract, so I want to also show you that it matches the intuition of what “choose” means.
On the one hand, with the subset method, one wants to _choose_ a subset of the elements of `β` which has just as many elements as `α`.
This is like marking a bunch of golf balls with a red marker, and then counting to make sure you marked the right amount of them.
But another way to choose a subset of a type is to specify a function `α → β`, considering `α` the pointing set and `β` is the set of objects of interest, require it to be injective (so that it points at exactly `α` many elements), and also say to ignore the identity of elements in `α`, given that they should only serve the role of pointer.
This is like taking a stack of the right number of stickers, and using them each to mark a different golf ball.
They arrive at the same result: a sized bunch of marked golf balls, but they take different methods to arrive at the same result.

Practically, I think it is more in line with the previous results to still be considering functions `α → β`, even if it is just designating a subset; type-theoretically, it is a good excuse to show how quotient types work.

As is usual with quotient types, in order to obtain information from a term, you have to prove that you are using it in a well-defined manner that respects the equality relation, and it is precisely this restriction which allows <!-- ?? (and even requires) ?? --> the quotient type to identify formerly distinct elements.
(In type theory language, this means that the recursor, through which pattern matching and elimination are defined, requires proofs that the functions for each constructor are well-behaved.)
This is very powerful: you get to redefine what equality means for the type!
(Subject to restrictions that ensure it is well-behaved, of course: equality always needs to be an equivalence relation.)

### Data structures
Of course, we donʼt have to only be considering functions.
It turns out that many combinatorial objects are modelled by list-like structures.
See the Inductive Types file for more info about inductive types.

Letʼs start with lists, which are a great building block for all sorts of containers:
```lean
inductive list (t : Type) : Type
| nil : list
| cons {} : t → list t → list t
```
(original: https://github.com/leanprover-community/lean/blob/6ed0977fcbc14354c86240cec7f8ef20842d0b5c/library/init/core.lean#L298-L300)

One important function is the length function; in fact, any parametric function that looks like `Π {t : Type}, list t → r` for some result type `r`, can be factored through the length function <!-- TODO: cite --> (meaning it must be `f ∘ length`, for some `f : ℕ → r`):
```lean
def length {t : Type} : list t → ℕ
| nil := zero
| (cons _ t) := succ (length t)
```
(original: https://github.com/leanprover-community/lean/blob/6ed0977fcbc14354c86240cec7f8ef20842d0b5c/library/init/data/list/basic.lean#L75-L77)

There are two main directions we can go from here, and they are both orthogonal and compatible, and furthermore they both can be expressed via subtypes and quotients:
1. Get rid of the inherent ordering of the lists, in order to build finite (multi)sets:
    1. As a quotient, this means filtering by the equivalence relation that identifies two underlying lists that are merely permutations of each other.
    2. As a subtype, it is less elegant: it means requiring an ordering on the elements of the list, and asking that all elements of the underlying list are ordered (which is also potentially computationally expensive).
2. Get rid of the possible duplication of elements in the list, in order to build lists without duplicates and plain finite sets:
    1. As a quotient, this means asking that all eliminators ignore the fact that an element of the underlying list may appear more than once.
    2. As a subtype, this means asking that no elements of the underlying list repeats, and this is actually more elegant, since type theory always has a notion of equality on a type.

So we typically see that a multiset is defined as a quotient of a list, and a finite set is defined as a subtype of a multiset (one needs to prove that the property of having no duplicates respects the lack of order of the multiset, of course). And of course, `length` respects the quotient types, so with some (justified) abuse of notation we will use it for all these types.

But, regardless of the particular definitions, we can see that these types mirror the kinds of questions we were asking about with combinatorics:
- Lists of fixed length `n : ℕ` with elements in `t : Type` (often called vectors) are counted by `|t|^n`
- Lists of fixed length `n : ℕ` with elements in `t : Type` without duplicates (perhaps called distinct vectors) are counted by `P |t| n`
- Sets of fixed length `n : ℕ` with elements in `t : Type` are counted by `C |t| n`

Still, the semantics we gave using functions above are more general, in that `n` does not have to be a natural number, but can be the cardinality of any type, meaning their semantics possibly extend to infinite cardinalities too.
For example, we would have to modify the underlying datatypes from inductive lists to coinductive streams in order to pull of a feat like asking for a countable subset of real numbers, but it is clear that the function-based approach works for that.
Whether it is the “right” generalization is a matter of debate, though.

## Counting as data analysis with functions (subtypes and quotients)
Iʼve been saying an incredible number of abstract things. Now I just want to consider an example that came up in class:

> What is the probability that a group of 100 people has one pair of birthday twins, two birthday triplets, and one birthday quadruplet? (Assume that every year has 365 days.)

It is standard to take a combinatorial approach to this problem.
The denominator of the probability is easy: it is `365^100`, which we said was modelled nicely by a function `fin 100 -> fin 365` which means “assign each of 100 people a birthday”.
The trick is figuring out the numerator.
What data type could represent the ?
I claim we can model this by figuring out what kind of data is needed.

The most direct approach would be to figure out how to classify the results of `fin 100 -> fin 365` which fit our desired pattern.
We define a function `groups : (fin n -> fin m) -> (fin m -> set (fin n))` that characterizes outputs by their preimages, and then `countgroups : (fin m -> set (fin n)) -> map ℕ (fin m)` which counts how many of each type (twins, triplets, etc.) occur, and then we use the composition of those functions and ask that their result be `[ 2: 1, 3: 2, 4: 1, 1: 100-2*1-3*2-4*1 ]` (that is, one twin, two triplets, one quadruplet).
This is great from a data computability perspective, but it doesnʼt make it obvious how many of these objects there are (even though itʼs a finite search, theoretically, it would be incredibly large).
```lean
def D := fin 100 -> fin 365

def groups : (fin n -> fin m) -> (fin m -> set (fin n)) .

def countgroups : (fin m -> set (fin n)) -> map ℕ (fin m) .

def desired : map ℕ (fin 100) :=
  [ 2: 1
  , 3: 2
  , 4: 1,
  , 1: 88
  ]

def numerator : Type :=
  Σ(bdays : D), countgroups (groups bdays) = desired
```

Instead, letʼs use our intuition, like we do for classic combinatorics: what information do we need to characterize this type of arrangement?
- We need 4 different birthdays, one for the twins, two for the triplets, and another for the quadruplet: call it `(A,B1,B2,C) : fin 4 ↪︎ fin 365` (we can consider it a 4-tuple, with distinct entries), where `|fin 4 ↪︎ fin 365| = P 365 4`
- We need to pick 2 people to be the twins: call it `a : fin 2 ↬ fin 100`, where `|fin 2 ↬ fin 100| = C 100 2`
- We need to pick 3 different people to be the first triplets: call it `b1 : fin 3 ↬ fin 98`, where `|fin 3 ↬ fin 98| = C 98 3`
- We need to pick 3 different people to be the second triplets: call it `b2 : fin 3 ↬ fin 95`, where `|fin 3 ↬ fin 95| = C 95 3`
- We need to pick 4 other people to be the quadruplets: call it `c : fin 4 ↬ fin 92`, where `|fin 4 ↬ fin 92| = C 92 4`
- We need a further 88 distinct birthdays for the remaining people with unique birthdays: `p : fin 88 ↪︎ fin 361`, where `|fin 88 ↪︎ fin 361| = P 361 88`

This results in a product type as our numerator space `N' := (fin 4 ↪︎ fin 365) × (fin 2 ↬ fin 100) × (fin 3 ↬ fin 98) × (fin 3 ↬ fin 95) × (fin 4 ↬ fin 92) × (fin 88 ↪︎ fin 361)`.

We then define a function `f : N' → D`. Iʼll handwave over the details for now, but it does what should be intuitive: for `((A,B1,B2,C), a, b1, b2, c, p) : N'`, we construct a result `bdays : D` which assigns the bday `A` to the two twins in `a`, `B1` to the triplets in `b1`, `B2` to those in `b2`, and bday `C` to the quaduplets in `c`, and the remaining bdays are assigned according to `p`.

Did you notice the problem, though?
We overcounted by a factor of two!
We made this mistake in class, and it was hard to explain why, but I think it is very clear with this framework now:
we overcount because we can swap the order of the triplets (`B1` with `B2` and `b1` with `b1`) and we get the same birthday assignments.
In symbols, `f((A,B1,B2,C), a, b1, b2, c, p) = f((A,B2,B1,C), a, b2, b1, c, p)`.
That is, the image of `f` (letʼs call it `fD := Σ(bdays : D), ∃(n : N'), f n = bdays`) is only half as large as the input `N'`.

So what we found isnʼt optimal, but it does get us pretty close (since the image of `f` does include all the examples we want, and nothing that doesnʼt belong).
But again, this characterization is less than satifying because we end up with a characterization of the data only as a subset of the denominator, not as a simple product type.
Can we come up with an explicit type that is equivalent?

We almost had it: we just need to discard the ordering of `B1`/`b1` and `B2`/`b2`.
Thereʼs one way to do it easily: quotient `N'` by the relation that identifies inputs sent to the same output:
```lean
def same_output {α β : Type} (f : α → β) : α → α → Prop :=
  λ(a1 a2 : α), f a1 = f a2

def N'' := Quotient N' (same_output f)
```

We could probably prove that `|N''| = |N'|/2`, given the niceness of this relation.
We could even replace it with an explicit characterization: the smallest relation `~` that identifies `((A,B1,B2,C), a, b1, b2, c, p) ~ ((A,B2,B1,C), a, b2, b1, c, p)`.

But I also want to go the next step. (WIP)

```lean
def finset : Type -> Type .

-- Turn a choice into a finset
def b {α β : Type} [fintype α] : (α ↬ β) → finset β .

lemma b_length {α β : Type} [fintype α] : ∀(f : α ↬ β), length (b f) = |α| .

-- Aggregate two finsets
def agg {β : Type} : finset β → finset β → finset β .

notation b1 <> b2 := agg b1 b2

def Day := fin 365
def Person := fin 100

⟨A, a⟩ : (Day \ {}) × (fin 2 ↬ Person \ {})
⟨B1, b1⟩ : (Day \ {A}) × (fin 3 ↬ Person \ a)
⟨B2, b2⟩ : (Day \ {A,B1}) × (fin 3 ↬ Person \ a <> b1)
⟨C, c⟩ : (Day \ {A,B1,B2}) × (fin 4 ↬ Person \ a <> b1 <> b2)
p : (Person \ a <> b1 <> b2 <> c) ↪︎ (Day \ {A,B1,B2,C})
__________________
bdays : Person ↪︎ Day
```

# Probability monad
I havenʼt delved into the existing research much, but it is a well-known fact that probability distributions form a monad.