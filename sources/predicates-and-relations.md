What is a proposition?

A __proposition__ is a statement of a question that you may be able to answer affirmatively, with a proof, or negatively, with a disproof.

(A __disproof__ of a proposition \(T\) is a proof of its __negation__, written as \(\neg T\) “__not__ \(T\)” or as \((T \implies \bot)\) “\(T\) implies false”.)

Not all questions have answers!

It is common to say that a proposition is either true or false.
But a system of logic may be too weak to prove a ‘true’ proposition.
Or the question may be __independent__ of the system and not regarded as ‘true’ or ‘false’, just independent.

Gödelʼs incompleteness theorem gives concrete examples of propositions that a system of logic cannot prove or disprove, because they encode some statement about consistency of the logic itself.^[A consistent system of logic is one that never proves a contradiction. Gödelʼs incompleteness theorem says this is impossible to prove for moderately interesting theories, without making use of more powerful assumptions.]
These are far from the only examples of independent propositions; they are just the most talked about.

Constructivism or intuitionism.

Classical logic.

So while a proposition may look like a Boolean value, especially in classical logic.

One more thing before we move on:
the __collection of all propositions__ may be denoted \(\mathrm{Prop}\) or \(\Omega\).

(__Collection__ is a deliberately vague word that can mean a type or a set or a class, or another similar notion in a theory. A __class__ is a predicate regarded as a collection, for handling collections that are “too large” to be a set, such as “the class of all sets” (the always-true predicate) or “the class of all sets that contain the empty set”.)

:::{.Example box-name="Examples"}

- If \(T\) is a proposition, we write \(T : \mathrm{Prop}\) or \(T \in \Omega\).
- For \(T : \mathrm{Prop}\), \(\neg T : \mathrm{Prop}\): its negation is also a proposition.
- Classical mathematics takes the stance that for all \(T : \mathrm{Prop}\), either \(T = \top\) (\(T\) is true) or \(T = \bot\) (\(T\) is false).
  That is, \(\Omega\) is equal to the set \(\{\,\top,\; \bot\,\}\).
  Be warned: just because it is true internally does not mean it is true externally.
  Not every proposition is provable (external truth), even though the theory believes every proposition is.

:::

---

A __predicate__ \(P\) is a proposition about something: if \(x\) is a thing, \(P(x)\) is a proposition.
A fruitful way to think about this is as a function: \(P : X \to \mathrm{Prop}\).
This is the approach that type theory takes!

We say that “\(x\) __satisfies__ \(P\)” when \(P(x)\) holds.
This can also be written \(x \in P\) (spoilers!).

:::{.Example box-name="Examples"}

- “is true” is a predicate on Booleans

- “is even” is a predicate on natural numbers: “2 is even” and “4 is even” are true propositions, while “1 is even” is a false proposition.

  There are many ways of defining this, depending on what system you are working in.
  One way is \(P(x) \coloneqq 2 \mid x\) using the “divides by” relation.
  Or you can encode it algorithmically, \(P(x) \coloneqq (\operatorname{rem}(x, 2) \stackrel{?}{=} 0)\).

  In a dependent type theory, this would look like

  <details class="Details">

  <summary>Example definitions</summary>

    ```{.agda data-lang=""}
    rem : Nat -> Nat -> Nat
    {- implemetation omitted -}

    -- Define via `rem`
    IsEven : Nat -> Prop
    IsEven n := (rem n 2) == 0

    -- Define recursively, via negation
    IsEven : Nat -> Prop
    IsEven zero = ⊤
    IsEven (succ n) = ¬ (IsEven n)

    -- Define through Bool and lift to Prop
    isEven : Nat -> Bool
    isEven zero = Bool.false
    isEven (succ n) = Bool.true

    IsEven n = (isEven n) == Bool.true

    -- As an inductive type
    inductive IsEven (n : Nat) : Prop where
      zeroIsEven : IsEven zero
      nextEven (m : Nat) : IsEven m -> IsEven (succ (succ m))
    ```

  Mathematicians would phrase this last one as “the smallest predicate \(\mathrm{IsEven}\) satisfying \(\mathrm{IsEven}(0)\) and \(\mathrm{IsEven}(m) \implies \mathrm{IsEven}(m + 2)\) for all \(m \in \N\).
  (“smallest”, being code for inductive, means that if \(\mathrm{IsEven}(x)\) is not provable from those assumptions, then \(\mathrm{IsEven}(x)\) is false.)

  </details>

:::

---

A __relation__ \(R\) is a predicate over two things: if \(x\) and \(y\) are things, \(R(x,y)\) is a proposition.
This can also be written \((x, y) \in R\) or infix like \(x R y\), which sets my Haskell syntax senses tingling but is widely used for relations.
Again, this can be thought of as a function, either curried \(R : X \to Y \to \mathrm{Prop}\) or uncurried \(R : X \x Y \to \mathrm{Prop}\).


:::{.Example box-name="Examples"}

- The equality relation is the most fundamental: \(R(x,y)\) defined by \(x = y\).
- Inequality relations like \(\le\) and \(\gt\) are also relations.

:::

:::Warning
There is no rule that the things ‘related’ by a relation are actually related in any intuitive or meaningful sense.
For example the relation defined by \(R(x, y) \coloneqq \top\) is always true: it ‘relates’ everything under the sun, even the most ‘unrelated’ things.
:::


## Set Theory

Set theory is based on first-order logic.

## Type Theory

## Univalent Foundations / Homotopy Type Theory


<!-- ʼ -->


