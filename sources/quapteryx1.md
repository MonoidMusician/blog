---
title: "On the SKI Combinator Calculus"
subtitle: "Quapteryx Part I: Introduction"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/05/10
---

For a long time Iʼve been fascinated by the idea that every number could be a program, every file could be executable.
There would be no decoding errors, no bad bits or bytes, it would just interpret everything equally – regardless of whether you intended it or not, really.

Sure thereʼs Gödel numberings of various systems, but they are pretty artificial, and they are usually not surjective: some numbers just arenʼt programs! Sorry computer.

:::centered
_Gödel numbering: an encoding only a mathematician could love._
:::

Where Gödel numberings are ugly, the corresponding deal for finite set theory is elegant: I already made [HatStack](hatstack.html), a stack-based language for hereditarily finite set theory, where every piece of data is just a natural number and these natural numbers *are* sets, bitwise.

It uses the [Ackermann coding](https://en.wikipedia.org/wiki/Hereditarily_finite_set#Ackermann_coding), whose premise is really simple: \(x \in y\) just means that the bit numbered \(x\) is set in the number \(y\): \((2^x \band y) \ne 0\).
Every set is just the bitfield of its children, who (as numbers) are just the bitfield of *their* children – hence, *hereditarily* finite:

\[y = \bigcup\limits_{x \in y} \{ x \} = \sum_{x \in y} 2^x.\]

But whatʼs the answer for programs? Say, for a nice lambda calculus?
Is there a similar encoding that is surjective and has nice properties?

Well, Iʼve known for a long while that there was one proposed answer: Jot, and its parent language Iota.

Every few months I would open the wiki page on [Jot](https://en.wikipedia.org/wiki/Iota_and_Jot#Jot), reference it in some conversation of nerdery.
A kind of restless fascination lingered in the background...
But I never seriously engaged with it, until I nerdsniped myself one day.

I did a deep dive into Jot for several months, figuring out exactly what made it tick, before determining that it was way too unwieldy for my purposes.

Instead, I realized that I could make a really efficient evaluator using SKI calculus a little more directly, using a *qauternary* encoding (base 4) and calling in Quapteryx.
I wanted to target WASM for several reasons, and I tried hand writing WASM (well, the textual format WAT), before deciding that that, too, was too unwieldy.
So I translated my WASM to C by hand, fixed a few bugs and actually got it working, and then I scrapped it for an actually efficient evaluator.

[My journey into Jot](quapteryx2.html) is the subject of the next chapter, and [Quapteryx](quapteryx3.html) is documented in the other chapters.

However, most readers will probably benefit from the background that occupies the rest of this chapter.

## SKI Combinators

SKI combinators are a distillation of the essence of untyped lambda calculus.

These are pure combinators: this means that they take a fixed number of arguments and return them applied to each other.
Of these, \(I\) is the identity combinator, the identity function we all know and love; \(K\) is the constant combinator that evaluates to its first argument; and \(S\) is a little strange, but it is “lifted function application”, related to the `<*>`{.haskell} operator of the `Applicative (->)`{.haskell} instance in Haskell/PureScript: it shares an argument \(z\) between its first two arguments \(x\) and \(y\), before applying their results together.

\[
\begin{array}{ll}
Ix   &=_\beta x \\
Kxy  &=_\beta x \\
Sxyz &=_\beta (xz)(yz) \\
\end{array}
\]

:::Note
The identity combinator \(I\) is actually redundant here, since \(SKy\) has the exact same behavior: \(SK \approx_\eta I\) because \[SKyz =_\beta (Kz)(yz) =_\beta Kz(yz) =_\beta z =_\beta Iz.\]

We could omit the \(I\) combinator, but it is useful for optimizations, keeps things tidy, and having the three combinators together makes things work out nicely in WASM/C, as you will see later.
:::

:::Warning
Donʼt take my comparison to `<*>`{.haskell} too literally.
The `Applicative`{.haskell} instance for functions is very particular, it is **typed**, where the combinator calculus is very much **untyped**.

The type of `<*>` is `(<*>) :: f (x -> y) -> f x -> f y`{.haskell}, and specialized to functions, `(i -> x -> y) -> (i -> x) -> (i -> y)`{.haskell}.

One way of explaining both `<*>`{.haskell} and \(S\) is that they are “lifted” function application: in this case, lifted over functions itself.
:::

<details>
<summary>Details on beta and eta equivalence</summary>
Note that the SK reduction rules are sufficient to have a Turing-complete combinator calculus, but they need to be expanded with some pretty weird rules to fully match the behavior of the lambda calculus.

From the Computer Science StackExchange, [“Combinator equivalent to eta conversion”](https://cs.stackexchange.com/questions/57361/combinator-equivalent-to-eta-conversion#57371), out of _The Lambda Calculus: Its Syntax and Semantics_ by Henk Barendregt:

\[
\begin{array}{ll}
A.1 & K = S(S(KS)(S(KK)K))(K(SKK)) \\
A.2 & S = S(S(KS)(S(K(S(KS)))(S(K(S(KK)))S)))(K(K(SKK))) \\
A.3 & S(S(KS)(S(KK)(S(KS)K)))(KK)=S(KK) \\
A.4 & S(KS)(S(KK)) = \\
    & \qquad S(KK)(S(S(KS)(S(KK)(SKK)))(K(SKK))) \\
A.5 & S(K(S(KS)))(S(KS)(S(KS))) = \\
    & \qquad S(S(KS)(S(KK)(S(KS)(S(K(S(KS)))S))))(KS) \\
A.6 & S(S(KS)K)(K(SKK)) = SKK
\end{array}
\]

Where \(A.1\text{--}A.5\) give a theory of beta equivalence isomorphic to lambda calculus, and \(A.3\text{--}A.6\) give a theory of beta–eta equivalence isomorphic to lambda calculus with eta.
</details>

As a Haskell data type,

```haskell
data SKI = S | K | I | SKI :@ SKI

-- Identity combinator
reduce (I :@ x) = x
-- Konstant combinator
reduce ((K :@ x) :@ y) = x
-- <*>-like combinator
reduce (((S :@ x) :@ y) :@ z) = (x :@ z) :@ (y :@ z)
-- Any other sequence does not reduce at the top
-- level, though it may reduce further down
reduce x = x
```

There are [many other combinators](https://www.angelfire.com/tx4/cus/combinator/birds.html), a lot of which Raymond Smullyan gave colorful (bird!) names to in _To Mock a Mockingbird_.

:::Note
“(Beta) reduction rules” are called that because they describe how one combinator “reduces” when applied to its arguments.

However this “reduction” is not always a reduction in size, and SKI combinators are Turing complete: some expressions do not reduce to a normal form, and may even grow infinitely large! [And it is [undecidable](https://en.wikipedia.org/wiki/Halting_problem) which ones do reach a normal form.]
:::

## Basis sets for combinator calculus

:::Bonus
To read more about the topic of this section, see the question [“Basis sets for combinator calculus”](https://cs.stackexchange.com/questions/57507/basis-sets-for-combinator-calculus) on the Computer Science StackExchange.
:::

The SK combinators form a minimal basis set: the \(K\) combinator drops its second argument, and the \(S\) combinator performs a couple functions including duplicating arguments (since \(z\) appears twice) and changing their order (since \(z\) occurs before \(y\) once) and chaining functions, and these are enough to emulate any lambda calculus function.

Other combinators can also form minimal sets: the BCKW combinators isolate their functions more readily.

\[
\begin{array}{ll}
Bxyz &=_\beta x(yz) \\
Cxyz &=_\beta xzy \\
Kxy  &=_\beta x \\
Wxy  &=_\beta xyy \\
\end{array}
\]

Here, \(K\) is familiar, \(W\) performs the function of duplicating an argument and \(C\) swaps two arguments, and \(B\) forms the function of chaining functions (composition): \(Bxy = x \circ y\).

Think about it like Boolean logic: you can take a set of a couple Boolean operators and recover every Boolean function.

But the big difference is that Boolean logic has one-point minimal sets: the “universal gates” NAND and NOR.
Combinator calculus does not have this: apparently at least two “things” are necessary to get all the behavior we need.

## Iota

Enter the iota combinator \(\iota\). It is an **improper** combinator: it has a reduction rule but its reduction returns *other* combinators (not just its arguments).

\[
\iota f =_\beta fSK = (fS)K
\]

In particular, it makes the \(S\) and \(K\) combinators available to its “callback” \(f\) (its lone argument).

:::Details
This can, of course, be encoded in SK combinators.
The idea of applying a callback with two arguments is captured by the [“Vireo”](https://en.wikipedia.org/wiki/Vireo) combinator, \(Vxyz =_\beta zxy\), whose encoding is this monstrosity: \[((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((SK)K))))K)).\]
So \(\iota = VSK\).
But this reduces to \(S(S(SKK)(KS))(KK)\), so thatʼs a little better at least.
And \(SKK \approx_\eta I\), so \(\iota = S(SI(KS))(KK)\) if we let ourselves use that combinator.
:::

There are other one-point bases, probably infinitely many, but this one is nice and small enough, and it is familiar since it relates to \(S\) and \(K\).

The iota combinator has nice properties: you recover \(I\) easily as \((\iota \iota) =_\beta SSKK =_\beta (SK)(KK) \approx_\eta I \).
From there it forms a nice 5-cycle by iterating \((\iota \_)\):

\[
\begin{array}{lll}
\iota (\iota \iota) &\approx_\eta ISK &=_\beta SK \\
\iota (\iota (\iota \iota)) &\approx_\eta ISKSK &=_\beta K \\
\iota (\iota (\iota (\iota \iota))) &\approx_\eta ISKSKSK &=_\beta S \\
\iota (\iota (\iota (\iota (\iota \iota)))) &\approx_\eta ISKSKSKSK &=_\beta SSK \\
\iota (\iota (\iota (\iota (\iota (\iota \iota))))) &\approx_\eta ISKSKSKSKSK &=_\beta SKS \\
\end{array}
\]

:::Note
Okay, technically \(SK(KK) \ne SKS\), but they behave the same for all inputs: they are eta-equivalent!
(This is part of why eta-equivalence is nice.)

So technically it does not form a 5-cycle until you start from \(\iota (\iota \iota) =_\beta SK\), which after five iterations of \((\iota \_)\) gets back to \(\iota (\iota (\iota (\iota (\iota (\iota (\iota \iota)))))) =_\beta SK\).
:::

Being an improper combinator is kind of a dealbreaker: you canʼt compute reductions of a language with just \(\iota\).
You have to resort to inserting \(S\) and \(K\) and evaluating with those.

… Or can you?

:::centered
_Read on at [Quapteryx Part II: Ojo: a Bitflipped Jot, and a Real Flop](quapteryx2.html) or jump to the good stuff at [Quapteryx Part III: Quaternary Combinators](quapteryx3.html) and beyond._
:::
