---
title: "Ojo: a Bitflipped Jot, and a Real Flop"
subtitle: "Quapteryx Part II: Impractical Encoding"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/05/10
---

:::centered
_See [Quapteryx Part I: On the SKI Combinator Calculus](quapteryx1.html) for an introduction._
:::

## Iota, Jot, Unlambda

We ended with the iota combinator last time. The language Iota is just the language of the iota combinator and function application, expressed in a prefix manner:

```bat
iota ::= "1" | "0" iota iota
```

```haskell
data Iota = Iota | App Iota Iota

iota2SKI :: Iota -> SKI
iota2SKI (App f x) = f :@ x
iota2SKI Iota = ((S:@((S:@((S:@K):@K)):@(K:@S))):@(K:@K))
```

Itʼs, uh, *very* difficult to implement an evaluator for Iota directly, much better to just translate it to SK calculus and evaluate it there. More on that later!

### Prefix Parens

So Iota gets cute with using `1`{.st} and `0`{.st}, but we should understand `0`{.st} as a prefix application marker.

[Unlambda](https://en.wikipedia.org/wiki/Unlambda) uses the backtick operator `` ` ``{.op} for this purpose.
It will be useful sometimes to use this for clarity. (Ironic, talking about clarity with Unlambda.)

Here is another way to think of it: `0`{.st} or `` ` ``{.op} are _left_ parentheses, **if** you print every reasonable parenthesis and not just the ones required for disambiguation.

That is, if you have a minimal printer (not a pretty printer) like this:

```haskell
print :: Iota -> Text
print Iota = "ι"
print (App f x) = "(" <> print f <> print x <> ")"
```

Then you get to Iota notation by replacing every `(` with `0`{.st}, every `ι` with `1`{.st}, and deleting every `)`.

Trailing parentheses are redundant in this notation, because each open parenthesis is followed by exactly two operands, and the leaves are single characters.

### Jot

Although Iota gets cute with using `1`{.st} and `0`{.st}, not every bitstring is a valid Iota program: only well-formed bitstrings are ([i.e.]{t=}, where every `0`{.st} is followed by two operands).

Jot is a language where *every* bitstring is a valid program.

However, I wonʼt show you Jot!
Everything feels backwards about Jot.
Instead weʼll talk about Ojo.

### Ojo

Ojo is bitflipped Jot.
I named it that partly because it *looks* like a bitflipped Jot: the vertical letters are replaced with oʼs, and the o is replaced with a vertical letter.
Also because iota “ι” is the equivalent of the letter “i”, which sounds like “eye” in English, which means “ojo” in Spanish… too cute to resist.

Anyways, Ojo is defined on *all* bitstrings, and is translated to the SK calculus via a function that we write as brackets (… itʼs a mathematician/logician thing, it means “semantic translation of the thing inside”).

```bat
ojo ::= "" | ojo "0" | ojo "1"
```

\[
\begin{array}{llll}
[]   &\triangleq (SK)K     &\approx_\eta & I \\
[w1] &\triangleq (([w])S)K &\triangleq   & \iota([w]) \\
[w0] &\triangleq S(K([w])) &=_\beta      & B([w]) \\
\end{array}
\]

Here, the right column is the semantic idea of each thing.
Itʼs pretty funny because each bit we take off of the *right* can be thought of as a *prefix* function, applied to the rest of the translation.^[This is not exactly true, because it takes additional beta reductions to get from \(B([w])\) to \(S(K([w]))\), but this is pretty irrelevant tbh, at the level of granularity we will be considering.]

In Haskell,

```haskell
data Ojo = Blank | And0 Ojo | And1 Ojo

ojo2SKI :: Ojo -> SKI
ojo2SKI Blank = I
ojo2SKI (And0 w) = ((ojo2SKI w) :@ S) :@ K
ojo2SKI (And1 w) = S :@ (K :@ (ojo2SKI w))

data Bit = Bit0 | Bit1

prefixFunction :: Bit -> SKI
prefixFunction Bit0 = iota2SKI Iota
prefixFunction Bit1 = (S:@(K:@S)):@K -- B = S(KS)K
```

Anyways, I really think this is the way that Ojo should be encoded.

My syntactic arguments for this choice are:

- `1`{.st} represents iota nicely!
- and `0`{.st} resembles the composition operator \(\circ\), which fits because thatʼs what \(B\) is, after all.
  \[
    \begin{array}{llll}
    (f \circ g)(x) &\triangleq& f(g(x)),\\
    Bfgx &=_\beta& f(g(x)).
    \end{array}
  \]

And my semantic points are:

- Extra zeros on the front are eta-irrelevant, so it gets us closer to our goal of every natural number being a program,
- and `0`{.st} in Ojo now corresponds to `0`{.st} in Iota,

But youʼll have to read the next section to see the justifications for these!

## Mechanics of Ojo

### Leading zeros

It turns out that leading zeros in Ojo are basically irrelvant.

If you reduce strictly according to the SKI beta reduction rules, \(BI =_\beta S(K(I))\) does not reduce any further,
and you need to get as far as two extra arguments before it reduces to parity with \(I\): \(BIxy =_\beta (KIy)(xy) =_\beta Ixy =_\beta xy\).

(This is what eta-equivalence means, btw: two expressions \(f\) and \(g\) are eta-equivalent (\(f \approx_\eta g\)) if they “behave the same”: there is some number of arguments \(n\), such that \(f a_1 a_2 \dots a_n =_\beta g a_1 a_2 \dots a_n\) reduce the same no matter what \(a_1, a_2, \dots, a_n\) are.)

To be sure, it is a bit questionable to rely on eta-equivalence here.
We arenʼt going to make a system that fully respects eta-equivalence, itʼs mostly about beta reductions.

But itʼs true *in theory*, and thatʼs important at least sometimes!

### The “Stack”

I claimed that `0`{.st} acts in Ojo like it does in Iota, but this is surprising: `0`{.st} is translated from the end in Ojo and maybe be followed by anything, whereas in Iota it is a prefix that enforces a well-formed parse structure.

Explaining this is a little harder, because it is actually muddled by Ojo/Jot.
But here is an attempt:

I claim that there is a bitstring `<ι>` such that if you take a valid Iota string and replace every `1`{.st} with `<ι>` (and keep all the `0`{.st}s the same!), then it is a Iota string that is beta–eta equivalent to the original.

Unfortunately, the first annoyance of Ojo is that, even though iota is fundamental to its construction, itʼs hard to *get* iota on this metaphorical stack: the encoding of `<ι>` is very complicated.

One such encoding is `00000001110000000111001010000110000011100001100011`{.st}. Augh! Itʼs 50 bits! (… this might even be minimal, Iʼm not quite sure.)

We can annotate what is going on here, using the Unlambda backtick for clarity:

```
ι = S(SI(KS))(KK)
00000001110000000111001010000110000011100001100011
``S-------``S-------I----`K----S-------`K----K----
((S       ((S       I)   (K    S)))    (K    K))
```

As you can see, the encoding of each \(S\) on the stack is 8 bits, and \(K\) is 5 bits: it is incredibly inefficient.
These are related to the iterated iota we talked about already, and you could continue it for \(I\) to get an encoding with 14 bits, but it is much more efficient to just do \(I = (SK)(SK)\) in 5 bits (which behaves the same as \(SKK\)).

Hereʼs a Haskell program, although it is a little weird:

```haskell
iota2ojo :: Iota -> Ojo
iota2ojo root = go root Blank
  where
    go :: Iota -> (Ojo -> Ojo)
    go (App x y) = And0 >>> iota2ojo x >>> iota2ojo y
    go Iota =
      -- 00000001110000000111001010000110000011100001100011
      And0 >>> And0 >>> ... >>> And0 >>> And1 >>> And1
```

Anyways, you might be starting to see the pattern here: `1`{.st} pushes two items onto the metaphorical argument stack, \(S\) and \(K\), and `0`{.st} is responsible for popping two items off of the stack, applying them together, and pushing that back onto the stack.
`1`{.st} is net 2 items, and `0`{.st} is net −1 items.

The annoyance that `1`{.st} pushes *two* items on the stack is honestly a dealbreaker for anything involving Ojo.
Everything is much simpler to explain in Quapteryx, where each nonzero quaternary digit pushes exactly one of \(S\), \(K\), or \(I\) onto the stack, and the zero digit behaves the same as Ojo.

### But but but …

So this is weird because on the one hand, zeros donʼt matter, and on the other hand, they are the glue holding everything together.

:::Key_Idea
There is a way to normalize any Ojo number/bitstring into an “operand” which behaves as one item on the metaphorical stack.
:::

Relatedly, we want to demonstrate that it really is a combinatory calculus: we should be able to define function application!

:::Key_Idea
If both `X`{.st} and `Y`{.st} are already well-formed operands, then `0XY`{.st} is too, and represents their application.

But in general, the leading zeros of `X`{.st} and `Y`{.st} can be ignored, and then there is a precise amount of zeros to place between the two bitstrings to get it to work, based on the bits of `Y`{.st}: `(2 * Y.#1s - Y.#0s) - 1`.

:::: {.Details box-name="Annoying Details"}
The complicated bit is if there are two many zeros towards the end of the bitstring `X`{.st} (a stack underflow, essentially).
I wonʼt cover it exhaustively here, but basically you have to reify the `0`{.st}s as `B`{.st}s on the stack.
Then the above works out.
::::
:::

## Evaluation

:::Warning
These evaluation rules are a little bit fudged: they are up to beta–eta equivalence, but not in any consistent manner, just whatever happened to be convenient.
:::

There are some bitstrings involving the identity combinator that we can just delete entirely, wherever we see it in the bitstring.

```
[w000101] = [w]
[w000100011] = [w]
[w000000000011111] = [w]
[w000000001110001100011] = [w]
```

To do more, we need to figure out how terms are encoded on the stack: a grammar of operands.

Since `1`{.st} pushes two terms and `0`{.st} decreases stack size by 1, then `<P(n)>` must satisfy the equation `n = 2 * #1s - #0s`, and furthermore the stack must not drop to zero (e.g. `001`{.st} cannot be a trailing string).

```
-- Push 1 onto the stack
-- `^0(1|00(?1)(?1)|0(?1)0(?1))$`
<P1> = '0' <P2>
-- Push 2 onto the stack (ambiguous grammar...?)
-- `^(1|00(?1)(?1)|0(?1)0(?1))$`
<P2> = '1' | '0' '0' <P2> <P2> | '0' <P2> '0' <P2>
```

Next we need two helpers for splitting apart composite terms (an annoyance unique to Ojo/Jot):

```
fst :: <P2> -> <P1>
fst '1' = '00000111' -- S
fst ('0' <fa:P2> '0' <gb:P2>) = '0' <fa:P2>
fst ('0' '0' <fa:P2> <bz:P2>) = '0' '0' <fa:P2> (fst <bz:P2>)

snd :: <P2> -> <P1>
snd '1' = '00011' -- K
snd ('0' <fa:P2> '0' <gb:P2>) = '0' <gb:P2>
snd ('0' '0' <fa:P2> <bz:P2>) = snd <bz:P2>
```

So now we can finally tackle the classic beta reductions of \(S\) and \(K\).
These are just sketch notes I took, nothing too formal.

```
-- So now we can reduce `K` = fst
[w0000011] = \mn. [w](Kmn) = \mn. [w]m
[w0000011<m:P1><n:P1>] = [w<m:P1>]
-- Split iota into `S K` and drop the `K`
-- [w00000111] = [w](KSK) = [w]S = [w00000111] -- oh.
-- Split iota into `S K` after `<m:P1>` and drop the `S`
[w0000011<m:P1>1] = [w<m:P1>]K = [w<m:P1>00011]
-- Split the annoying <P2> = 00<P2><P2> case
[w000001100<fa:P2><bz:P2>] = [w00<fa:P2>0000011<bz:P2>]

-- And we can reduce `KI` = `SK` = snd, which drops one from the stack
[w00001100101] = [w](KI) = [w0]KI = [w](SK) = [w01]
[w001<m:P1>] = [w]I = [w00101] -- not necessarily a reduction
[w0001<mn:P2>] = [w]n = [w<snd mn:P1>] -- not necessarily a reduction
-- Split iota into `S K` and drop the `S`
-- [w00011] = [w](SKSK) = [w]K = [w00011] -- oh.
-- [w0011] = [w](SKS)K = [w]IK = [w0010100011] -- <P2> -> <P1><P1>
-- Split the annoying <P2> = 00<P2><P2> case
[w00100<fa:P2><bz:P2>] = [w001<bz:P2>]

-- And `S`, I guess
[w00000000011111] = [w](SSKSK) = [w](SKS) = [w]I = [w00101]
[w000000000011111] = [w]
[w00000000111<mn:P2><o:P1>]
  = [w](Smno) = [w]((mo)(no))
  = [w00<fst mn:P1><o:P1>0<snd mn:P1><o:P1>]
[w00000000111<m:P1><no:P2>]
  = [w00<m:P1><snd no:P1>0<no:P2>]
[w00000000111<mn:P2><op:P2>]
  = [w](Smno)p = [w]((mo)(no))p
  = [w00<fst mn:P1><fst op:P1>0<snd mn:P1><op:P2>]
```

## Deficiencies

Anyways, uhh, by now you can see why Ojo/Jot arenʼt really worth being implemented.

The “reduction rules” are absolutely absurd and thereʼs no way to implement them performantly, they offer absolutely no insight.

The fact that `1`{.st} pushes two things onto the stack complicates the grammar incredibly.

The fact that the iota is so hard to reproduce on the stack is also annoying.

_[&c.]{t=} [&c.]{t=}_

*However*, the core ideas are sound – even attractive: every natural number is a program, use `0`{.st} as prefix application, and reductions are possible by searching through the string with a grammar.

Quapteryx builds on these ideas for a really efficient implementation, hopefully.

:::centered
_Read on at [Quapteryx Part III: Quaternary Combinators](quapteryx2.html)._
:::
