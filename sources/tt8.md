<!-- ʼ Π ≡ ℕ → λ Σ ≃ -->

# Type Formers and Cardinality
Letʼs construct some infinite types! We wonʼt get too large, but will cover the [beth numbers](https://en.wikipedia.org/wiki/Beth_number).

This process will require us to first construct ℶ₀, then we can follow a regular construction to get to ℶ₁, ℶ₂, etc.

We will use functions (of course!), along with either a [natural numbers object](https://ncatlab.org/nlab/show/natural+numbers+object) (NNO/ℕ) or recursive data types (ADTs/inductive types). It turns out that more sophisticated extensions, like GADTs or subtypes/quotients, do not influence the particular results here.
As far as the syntax goes, weʼll freely promote cardinalities to types (so 1 is the unital type, 2 is the type of booleans, etc.), and will measure the cardinalities of types with bars: |ℕ| = ℶ₀.
We will similarly use the standard operators (`+`, `*`, and `^`) both for cardinal arithmetic and the corresponding type operators.

Note: in what follows, weʼll be assuming the Axiom of Choice (AC) so the cardinal numbers are well-ordered.

<!-- TODO: verify that HITs cannot construct larger types -->

## Starting Off: Countable Infinity
Where does infinity come from?
Letʼs start with the smallest infinity, which we call countable.
Thereʼs two equivalent answers: natural numbers and ADTs/inductive types.

The natural numbers ℕ are special because theyʼre the smallest infinite type. This can be seen with their construction as an inductive type: one constructor to make it inhabited (`zero : ℕ`), and another to make it infinite (`succ : ℕ → ℕ`). No bells and whistles.

But, if weʼre talking about cardinals (as weʼre doing here), or alternately if we accept Univalence, then all countable cardinals/types are equal, so this fact of the particular construction does not matter.

In fact, it turns out that if you exclude mentioning function types in your inductive definitions, then _all_ recursive data types are countable.

(Quick proof sketch: Assume all existing types are countable. Inductive types have the capabilities of sums, products, and recursion. Sums and products both preserve countability. And recursion also preserves countability, since it constructs the least fixed point, and nothing will push it to be uncountable.)

(Note: this goes for all flavors of ADTs: GADTs and I believe higher inductive types in HoTT too.)

So if you donʼt use function types in your ADTs, I can guarantee they are countable! (And thus serializable and discrete.)

There are thus a lot of equivalent ways to come up with our first infinite cardinal ℶ₀, and hopefully this shows you why it isnʼt an arbitrary decision, although we typically only talk about the natural numbers and countability.

## Stepping Up: The Power of Functions
One way of thinking of the cardinality of a type is that itʼs the number of choices you have to pick an inhabitant of that type.

To pick an 8-bit number, you have 2^8 = 256 numbers to choose from; the type of 8-bit numbers has cardinality 256.

To pick a list of 8-bit numbers, you first have to choose the length `n`, which can be any natural number (so countably many choices), and then you have to fill `n` slots with 8-bit numbers, so 2^8^n choices – but these finite choices donʼt matter, and itʼs a countable number of choices overall: ℶ₀.

To pick a list of natural numbers, choose a length `n`, and then `n` natural numbers to fill the array.
Itʼs a little trickier to see, but this is also a countable type, since we want to compute ℶ₀ × ℶ₀ × … × ℶ₀ (`n+1` times) and it turns out that ℶ₀ × ℶ₀ = ℶ₀ so ℶ₀^`n+1` = ℶ₀.
(This can be seen by constructing a bijection from pairs of natural numbers to natural numbers, which is also useful for proving that integers and rationals are countably infinite too.)

So these constructions only result in countably many choices.
How do we get a larger infinity?!

The answer is, of course, function types!
Functions let you analyze the input and branch on arbitrary distinctions, but that means for _each choice of input_, you must make a choice of output to associate to that input.
This means that instead of a product of finitely many ℶ₀s, we can instead get a product of ℶ₀-many ℶ₀s.
(This makes sense if we consider how (dependent) functions also moonlight as infinite products.)

So if we want to choose a function of type `ℕ → ℕ`, we have to first choose the value of `f 0 : ℕ` which has countably many choices, then we need to repeat this countably many times!
This results in `ℕ → ℕ` having a cardinality of ℶ₀^ℶ₀ which is our long-sought-after uncountable cardinal ℶ₁.

However, it turns out that we can economize on our types, and the cardinality of `ℕ → 2` is enough to reach ℶ₁.
To summarize the previous process, each function was the product (quite literally) of countably many choices, and there were uncountably many other functions we could make with different choices.
The fact that each choice of where `f n` went was also countable is actually immaterial: it suffices to be a binary choice, the emphasis is on how each function incorporates an infinite number of choices in it.
(Note: I think this is just helpful intuition, but weʼll formalize how it works in general and clean up the rough edges in the section on arithmetic.)

Now that weʼve clarified the essential ingredients, we can repeat the construction quite easily: ℶ₂ is the cardinality of `(ℕ → 2) → 2`, ℶ₃ is the cardinality of `((ℕ → 2) → 2) → 2`, and in general, ℶₐ₊₁ = 2^ℶₐ.

Another way of viewing this is as the powerset construction: ℶₐ₊₁ = ℙ(ℶₐ).

### Whatʼs Covered and Whatʼs Missed?
If we assume the generalized continuum hypothesis (GCH) and the axiom of choice (AC), beth numbers are essentially the only cardinalities we can construct with this iterated function/powerset construction.
(There will still be larger cardinals that can be constructed otherwise, with .)

Without GCH, there will be other cardinals in between the beth numbers, but we canʼt give them an explicit construction.
(But when has that ever stopped destructive mathematicians? :wink:)

Without AC, the cardinals arenʼt even well ordered so we canʼt say much, other than that there are cardinals other than the beth numbers that we canʼt really relate to but arenʼt really distinguishable either.
(This last part might be wrong.)

Here are some examples of types/sets with the cardinality of each beth number:

### ℶ₀
- The type of natural numbers ℕ, of course
- Also the type of integers ℤ and the type of rationals ℚ
- The type of lists with elements of countable type

### ℶ₁
- The powerset of any type of cardinality ℶ₀ (i.e. countable type)
- The type of sequences of any countable type
- The type of real numbers ℝ
	- Note that there are various constructions of the real numbers, but at some point they each have to use a function or powerset construction to increase the cardinality to ℶ₁, and further steps must not decrease back to ℶ₀.
	- A naïve view of real numbers as infinite sequences of decimal digits (with a couple corner cases) captures well the intuition that there has to be a process that generates these additional choices.
	- The construction of ℝ via Cauchy sequences is a quotient of a subtype of sequences of rationals (ℕ → ℚ), those sequences which are Cauchy-convergent (intuitively meaning that they eventually bunch together to point to a real number) quotiented by the relation that identifies two real numbers when the difference of their Cauchy sequences goes to 0. This assumption of Cauchy-convergence and the further quotienting does not rule out enough functions to affect the cardinality.
	- The Dedekind construction starts with powersets of rationals (ℚ → 2) and subtypes them to ensure they are well-formed Dedekind cuts, again without changing the cardinality.
- More remarkably, the cardinality of the type of continuous functions from ℝ to ℝ is also ℶ₁. (Removing the continuity restriction would of course give a cardinality of ℶ₂.)

More examples can be formed either with the powerset construction, or by using functions, as elaborated on in the next section

## Cardinal Arithmetic
```haskell
-- A model of the cardinality of a type:
-- Either finite with the specified number of inhabitants,
-- or infinite, with the number of inhabitants as indexed by the beth numbers
data Card
  = Finite Nat
  | Beth Nat

instance Ord Card where
  compare (Finite a) (Finite b) = compare a b
  compare (Finite _) _ = LT
  compare _ (Finite _) = GT
  compare (Beth a) (Beth b) = compare a b

-- On finite arguments, sum and product work as usual,
-- otherwise, take the larger argument
sumCard :: Card -> Card -> Card
sumCard (Finite a) (Finite b) = Finite (a + b)
sumCard a b = max a b
prodCard :: Card -> Card -> Card
prodCard (Finite a) (Finite b) = Finite (a * b)
prodCard a b = max a b

-- funCard a b is the cardinality of (a -> b)
-- (the flipped version would be the exponential b ^ a)
-- The main rule of interest is the last one
funCard :: Card -> Card -> Card
funCard (Finite 0) _ = Finite 1 -- initial object, b ^ 0 = 1
funCard _ (Finite 1) = Finite 1 -- terminal object, 1 ^ a = 1
funCard _ (Finite 0) = Finite 0 -- 0 ^ a = 0 (a /= 0)
funCard (Finite 1) b = b -- b ^ 1 = b (b /= 0)
funCard (Finite a) (Finite b) = Finite (b ^ a)
funCard (Finite _) (Beth b) = Beth b -- Bb ^ n = Bb
funCard (Beth a) (Finite _) = Beth (a + 1)
  -- n ^ B_a = B_(a+1) (n >= 2)
funCard (Beth a) (Beth b) = Beth (max (a+1) b)
  -- B_b ^ B_(b+n) = B_(b+n+1)
  -- B_(a+n) ^ B_a = B_(a+n)
```