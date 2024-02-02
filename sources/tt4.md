I think inductive types are one of the most amazing ways of reasoning about mathematical objects and I wish I saw more explanations of them. Iʼll try to do my best here.

# Function types

# Trivial types
For most programmers, trivial types donʼt look so useful at first glance – who would ever need a type that has no constructors!! – but they are actually useful for a couple reasons:
They ensure that the type system is “complete” and able to represent a wide range of types,
and they are useful when you need to plug a type in but want the most minimal thing.

Think of it like this trick question: how would you represent `12` as a product of three integers?
One possible answer would be `3 * 4 * 1`, or maybe `12 * 1 * 1`.
If we didnʼt have `1`, the multiplicative identity, we couldnʼt do this!
Similarly, the trivial types let us plug in something when we absolutely must.
They wonʼt convey any information, but they will fill in the hole.

The two most trivial types are the empty type, with zero inhabitants, and the unit type, with one inhabitant.
Category theorists love trivial types, and they call the empty type the initial object (in the category of sets) and the unit type the terminal object.

```haskell
data Void -- yup, thatʼs all

data Unit = MkUnit
```

# Basic data types: (non-recursive) ADTs
Algebraic Data Types are a term for a manner of imagining new types based on existing types, in a simple pattern of constructors filled with values.

## Product types
Product types store multiple independent pieces of data together.
We can create specific product types, but even better, we can create a generic product type parameterized over its two constituents.

```haskell
data ProductOfTAndD = MkProductOfTAndD T D

data Product a b = MkProduct a b
data Product a b where
  MkProduct :: a -> b -> Product a b
```

A lot of languages have support for records in various ways, which are essentially products with any number of named fields.

## Sum types
Sum types are really cool to work with, especially with pattern matching.

Sum types can represent tagged data, but my favorite part about them is that you donʼt have to care about how they are represented internally.

Like product types, we can think of a sum of specific types or of two generic types.

```haskell
data SumOfTAndD = SumOfT T | SumOfD D

data Sum a b = InL a | InR b
data Sum a b where
  InL :: a -> Sum a b
  InR :: b -> Sum a b
```

## Constructors
In general, we can define a non-recursive ADT to be.

## Pattern matching
Pattern matching is the bread and butter of inductive types.
So far we have seen in GADT syntax the representation of constructors, which allows us to create new values of an ADT, but if we ever want to analyze a value that came from somewhere else, we need pattern matching.

Pattern matching looks at the possible shapes of a value (as dictated by its type) and lets the information contained in the value be used to produce a result.
It turns out that a lot of nested conditionals in other languages can be replaced with pattern matching, and the result is almost always cleaner and easier to read!
```haskell
```

Functional programming languages usually have a syntax for pattern matching that looks a lot like constructing values, but follows a few different rules.
In fact, pattern matching is more restrictive, so not every use of a constructor is a valid pattern match, but most pattern matching is valid as a constructor.

For datatypes with only one constructor (that is, product types), there only needs to be one case to analyze.
A lot of programming languages make this easier to use by allowing it to be inlined as a function argument, even in an anonymous function:
```haskell
useProduct :: Product Int Int -> Int
useProduct (Product i j) = i + j * (i - 1)
-- alternative syntax
useProduct = \(Product i j) -> i + j * (i - 1)
```

## Duality: Sums and Products, Constructors and Pattern matching
This is our first chance for a note on duality.
Duality is a very common and useful pattern to recognize!

Products and sums are a classic example of a duality:
Products require two pieces of information to construct but ,
while sums require only one piece of information but need to handle two cases when pattern matching.

# Infinite (recursive) data types

Natural numbers are the simplest infinite inductive type, and they can be defined as follows:
```lean
inductive nat : Type
| zero : nat
| succ : nat → nat

notation ℕ := nat
```

It turns out that this is an encoding of Peanoʼs axioms as a datatype.
It is the smallest type with an element `zero` and an injective function `succ` that satisfies the required properties, such as injectivity, and the fact that its image is distinct from any other constructor (namely, `zero ≠ succ n` for all `n : ℕ`).
An element of this type looks like `succ (succ (… zero))` (for some finite number of `succ`s), but can [more generally be thought of as “freely generated” by those constructors](http://math.andrej.com/2013/08/28/the-elements-of-an-inductive-type/).
(HITs are where this freely generated bit really matters, but weʼre not there yet.)

How do you use a value of `nat`?
Well, you can still pattern match on it, but itʼs a little more complicated now:
there are infinite possible patterns to try to match!

Weʼll see a little later how to package this up more nicely and make it well-behaved, but for now, letʼs just assume that our language has recursion, so we can call our function as we are defining it:
```haskell
describe :: Nat -> String
describe Zero = "Zero, the additive identity"
describe (Succ Zero) = "One, the multiplicative identity"
describe (Succ (Succ Zero)) = "Two, the smallest prime number" -- my favorite number!
describe (Succ (Succ (Succ Zero))) = "Three, the smallest odd prime number"
describe (Succ (Succ (Succ (Succ n)))) = describe n <> ", plus four"
```
We can pick out specific numbers to test for, or we can remove a few layer of `Succ` and delegate the rest to another function (or the same function, recursively).

# Quantification over types

## Forall quantification

## Parameterized data: functors, HKDs

## Type equality

## GADTs
Generalized Algebraic Data Types are our final step before we get to dependent types, which I consider the ultimate goal of type systems.

GADTs expand the power of parameterized data types with the ability to be by more specific about the parameter.
It essentially adds two more features: existentials and type-equality.
(This means that if you have a type theory that doesnʼt have GADT syntax, but does have existentials and type equality, then you can in fact rewrite the GADTs in terms of those.)

As it has been implemented in Haskell it has a different syntax than normal ADTs, but in fact it is more in line with other functional programming languages and theorem provers, which tend to favor GADT-like syntax only instead of Haskellʼs ADT syntax.
Maybe you can see why.

Again, we specify a list of constructors, but this time we specific not its arguments, but the actual type of the constructor.
For each constructor, we can quantify it however we want, we can take in arguments of any type, and the only restriction is that we have to have the return type be the GADT weʼre constructing applied to some arguments.
This means that the constructor can quantify over types that donʼt appear in the result (existentials) and that the result type can have parameters that are not just arbitrary variables but can be more specific, or even concrete types (type equality).

(insert example)

Two notes:
1. It is actually possible to have existentials in regular ADTs in Haskell.
2. Both existentials and type equality can be encoded in quantified functions, but those encodings are much less convenient to use: `type Existential f = (forall r. (forall a. f a -> r) -> r)` and `type LeibnizEquals a b = forall f. f a -> f b`.

# Dependent quantification
Now we get to the fun stuff: dependent types!

It turns out that many of the restrictions on what have to be types and what things have to be values were unnecessary: they can mix together and coëxist on more-or-less equal footing.
It turns out to be a radical re-imagining of the foundations of programming languages/type theory, and requires a lot more care when designing a compiler, but it actually simplifies a number of things in the theory and syntax.

## General equality, DITs, subtypes, quotient types

## Univalent equality, HITs, propositional truncation

## Induction rules

# More on pattern matching
## Pattern matching in Agda

# More on function types

## Recursion
We punted this question earlier: when is function recursion well-behaved?

It turns out to be a very difficult question, in that we canʼt give a truly satisfactory answer, but we can give one answer that is simple enough and another answer that is effective enough to fill in the gaps.
The reason why it is really difficult is that if we could solve the problem of when recursion is well-behaved, we would be able to solve the halting problem in general, which is impossible to do automatically!
Which is why we either settle for a simple but leaky heuristic, or we pull out the big guns and give a proof of termination.

### Structural recursion
The simplest answer is that we can apply a function recursively only to things that are contained within or “smaller” than the current argument.

It turns out that using recursion principles automatically packages up the recursion in a structural way.

### Well-founded recursion
Sometimes the function isnʼt structurally recursive, and so we need to prove for the compiler that it will in fact terminate.

The general theory of functions is well-founded recursion based on well-founded relations.
The most familiar well-founded relation is the less-than relation on natural numbers:
Thereʼs a base case, 0, and if you start at any natural number and ask for another number that is strictly less than it, you eventually reach that base case (you canʼt go on forever).

(insert definition of well-founded relation.)

How we “prove” that a function terminates is by recursion on this relation, which means that we start by analyzing the argument, and get to recurse in the function only if we can prove our new argument is smaller with respect to the relation than the current argument, otherwise the recursion might not be well-defined (meaning, computationally, that it might loop).
If we cannot find a smaller argument, we must have hit a base case, and should produce a value directly.

(Aside: It turns out that the class of well-founded relations are called ordinals, and they can get [really reaaally reaaaally large](https://johncarlosbaez.wordpress.com/2016/06/29/large-countable-ordinals-part-1/).
Different theories will have larger or smaller ordinals that they can construct.)

## Parametricity
This is actually my favorite part!

## Böhm–Berarducci encoding
This is related to (and often conflated with) Church encodings, but Böhm–Berarducci are more powerful and they specifically address what happens in a typed setting.

So it turns out that the recursion principles we looked at above actually capture all we need to know about a datatype.