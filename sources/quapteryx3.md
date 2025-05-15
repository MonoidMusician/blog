---
title: "Quaternary Combinators"
subtitle: "Quapteryx Part III"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2025/05/11
# Yes i wrote the last two posts in a day
---

:::centered
_See [Quapteryx Part I: On the SKI Combinator Calculus](quapteryx1.html) and [Quapteryx Part II: Ojo: a Bitflipped Jot, and a Real Flop](quapteryx2.html) for introduction/background on this topic._
:::

Quaternary combinators.

If binary is base 2, then quaternary is base 4.
Just as binary has bits (digits) and bitstrings (sequences of digits), quaternary has [crumbs](https://en.wikipedia.org/wiki/Quaternary_numeral_system#Relation_to_binary_and_hexadecimal) and crumbstrings:

> By analogy with *byte* and *nybble*, a quaternary digit is sometimes called a *crumb*.

Quapteryx, archaeopteryx.

Like Ojo, Quapteryx can be defined by a suffix-based translation to SK combinators.
Unlike Ojo, it is simpler to define and *much* simpler to work with: one digit for each combinator, and `0`{.st} to glue them together.

<figure>

\[
\begin{array}{ll}
[]   &\triangleq I \\
[w0] &\triangleq S(K([w])) \\
[w1] &\triangleq [w](I) \\
[w2] &\triangleq [w](K) \\
[w3] &\triangleq [w](S) \\
\end{array}
\]

<figcaption>
The translation of the empty string is the I combinator.
The translation of a trailing zero is S applied to K applied to the translation of the rest of the string.
The translation of each trailing digit 1, 2 or 3 is the translation of the prefix applied to the combinator I, K, or S, respectively – we will think of these as being pushed to the metaphorical stack of pending function arguments.
</figcaption>
</figure>

In this way it mirrors the syntax of Iota (and the pure combinator fragment of Unlambda) more precisely, bridging the gap between Iota and Jot.
Well-formed/atomic “operands” are defined by this simple grammar:

<figure>

```
<operand> ::= "0" <operand> <operand> | "1" | "2" | "3"
```

<figcaption>
An operand is one of these productions: 1, 2, or 3 alone, or 0 followed by two operands.
</figcaption>
</figure>

And the translation of arbitrary quaternary combinators into well-formed operands just needs to count crumbs and use this one fact to deal with zeros that “underflow” the stack ([e.g.]{t=} trailing zeros): \([w0] =_\beta [0302w]\).

For other zeros, those that are followed by enough operands, we have the key idea that `0`{.st} operates as function application on a stack:

\[[w0\vec x\vec y] =_\beta [w]( [\vec x] [\vec y] ).\]

For example, we can get \((SK)\) on the stack by having two items \((S)\) and \((K)\) on the stack, and then using `0`{.st} to apply them together:

\[[w032] \triangleq [w0](S)(K) =_\beta [w](SK).\]

-------

The _really_ cool fact is that each combinator is represented by its *arity*: \(I\) takes one argument, \(K\) takes two, and \(S\) needs three to compute.

This means that (at least in well-formed operands), seeing beta reductions is trivial: just count the zeros before each nonzero digit: `01x`{.st} reduces, `002xy`{.st} reduces, `0003xyz`{.st} reduces.

As discussed before, \(I\) is technically redundant in this picture: \(I \approx_\eta SKx \approx_\eta SKK\), or `1 = 00322`{.st} in Quapteryx.
However, including \(I\) is important for efficiency for several reasons:

- Most importantly, having quaternary combinators is better for computers than ternary combinators: everything stays nicely aligned to bits.
- It fills out the arity–digits correspondence very nicely.
- \(Ix\) is easy to identify as a trivial reduction, whereas \(Sxyz\) is a nontrivial reduction in general, so having \(I\) lets us spot more performance opportunities, potentially.

## Evaluation in Theory

Letʼs talk about the theory of SKI combinators using our syntactic model of crumbstrings.

We have these beta reduction rules, which are are allowed to apply anywhere in the string to generate a reduced string, as long as `<x>`, `<y>`, and `<z>` are well-formed operands:

<figure>

```
01<x:operand> = <x:operand>
002<x:operand><y:operand> = <x:operand>

0003<x:operand><y:operand><z:operand>
  = 00<x:operand><z:operand>0<y:operand><z:operand>
```

\[
\begin{array}{rl}
[01\vec x] &=_\beta [\vec x] \\
[002\vec x\vec y] &=_\beta [\vec x] \\
[0003\vec x\vec y\vec z] &=_\beta [00\vec x\vec z0\vec y\vec z] \\
\end{array}
\]

<figcaption>
“01x” beta reduces to “x”, “002xy” reduces to “x”, and “0003xyz” reduces to “00xz0yz”.
</figcaption>
</figure>

If the first nonzero digit is irreducible (not enough following operands, which corresponds to not enough leading zeros for a well-formed operand), then the operand is in “Weak Head Normal Form” ([WHNF]{t=}).
If all digits are irreducible, then it is in Normal Form ([NF]{t=}).
The normal form, if it exists, is unique for an operand: any reduction sequence that terminates finds that same normal form when it becomes irreducible.

We can describe these properties on [AST]{t=}s (Abstract Syntax Trees, represented as Haskell data types):

```haskell
-- A well-formed operand is a binary tree
-- with combinators at its leaves
data Operand
  = Q0 Operand Operand
  | Q1
  | Q2
  | Q3

-- Only the top level of a WHNF node needs
-- to be evaluated to an irreducible operand
-- (Weak Head Normal Form)
data WHNF
  = WHNF_I0
  | WHNF_K0
  | WHNF_K1 Operand
  | WHNF_S0
  | WHNF_S1 Operand
  | WHNF_S2 Operand Operand

-- Every level of a Normal Form node needs
-- to be evaluated to an irreducible operand
data NF
  = NF_I0
  | NF_K0
  | NF_K1 NF
  | NF_S0
  | NF_S1 NF
  | NF_S2 NF NF
```

:::Note
The tradeoff between the string representation and the [AST]{t=}s is that in the [AST]{t=}, you have to drill down into the `Q0`{.haskell.dt} nodes to see if there is a reduction, but then the arguments are already nicely separated for you, whereas in the string representation, you just have to scan from left to right, and then you have to parse out the arguments from the remaining bitstring (and check that there are enough, if it isnʼt guaranteed to be a valid operand string already).
:::

So hopefully you see that Weak Head Normal Form isnʼt so scary after all, and Normal Form is just a recursive version of it.
And “recursive” in the model of crumbstrings means that it is applied everywhere in the string.

Later we will come up with a stack-based algorithm for efficiently computing [WHNF]{t=} (it sort of sits and “spins” on the leftmost nonzero digit always), and we see how we will have to follow the tree structure to apply it to get the full [NF]{t=}.

### Evaluation Order

There are many ways to apply reduction rules.
You could choose a unique reduction order for every string if you wanted.
Or pick a reduction totally at random each other.

However, there are only a few coherent strategies for choosing reductions that are generally used.

Right-to-left evaluation is the simplest to explain: this is “applicative order evaluation”, where arguments (on the right) are reduced before their functions (on the left) are “called” with their evaluated result.
This corresponds to strict programming languages, and is very common and familiar.

This is the simplest, and relatively performant, but it has a big downside: for arguments that are eventually discarded, it has performed unnecessary work evaluating them, and worst of all, it can get trapped in senseless nontermination.
A lot of the “recursion combinators” that are commonly used for SKI calculus do not terminate under applicative order evaluation.

Left-to-right evaluation (to [WHNF]{t=} and then recursively to [NF]{t=}) solves this problem: all programs that *do* terminate, terminate under left-to-right evaluation.
(Of course this is an [undecidable](https://en.wikipedia.org/wiki/Halting_problem) proposition.)

“Aha! This is lazy evaluation, the dual of strict evaluation” you might say.
Not so fast – we have to be careful.

True lazy evaluation uses *thunks* to share the results of evaluation: when a function duplicates an argument (in our case: `Sxyz = (xz)(yz)` duplicates `z`), it ensures it is stored in a thunk so work can be shared efficiently. Once the thunk is evaluated the first time (partially or fully), its value is updated, and the other occurrences of the thunk can then use this value to avoid duplicating work. And if the thunk is never demanded, no work is done to evaluate it.

Naïve left-to-right evaluation does not do this, there is no concept of a thunk.
Instead each argument that is duplicated is re-evaluated from scratch every time.
This is very inefficient for several reasons (not only the time inefficiency of duplicating the work, but the space inefficiency of duplicating many arguments before evaluating them), but at least it still finds normal forms for all terminating programs like lazy evaluation does.
This is sometimes called “normal order evaluation” in the literature.

------

So these are the three main evaluation strategies: right-to-left or “applicative order”, left-to-right or “normal order”, and lazy evaluation.

For Quapteryx, we want to generally implement lazy evaluation, for its nice termination properties, but we wonʼt implement it precisely correctly: we may cut some corners (such as not duplicating words that only contain trivial reductions, or evaluating those trivial reductions early).

### Bithacking

This is the transition from theoretical evaluation to practical evaluation.

The grammar of Quapteryx is so well-behaved that we can implement things really efficiently using bitwise operations.
Itʼs what inspired me to actually implement it, and to chose lower-level languages for the task.

But the caveat here is that we donʼt want to end up with packed crumbstrings for our only representation.
Copying crumbstrings (especially since it is unaligned memory!) is quadratically inefficient, and not being able to use thunks misses the whole point of it.
So this is just the groundwork for an efficient evaluator, not the end result.

#### Redexes

A “redex” is a place where you can apply a reduction: a *reducible expression* within the larger expression tree.

In our case, since we are looking at well-formed operands in this evaluator, we have a very simple way to identify redexes: it is a nonzero digit preceded by that many zeros! Thus its arity is satisfied.

Thus redexes always occur at `01`{.st}, `002`{.st}, and `0003`{.st}.
These redex heads are followed by one, two, and three arguments respectively, and we have to parse those out of the crumbstring: but thatʼs work that we only have to do once weʼve identified the redex, which is pretty simple to do.

In fact, it is so simple that we can scan a whole machine word for reductions at once, in about 23–27 operations (regardless of the size of the word^[so in theory this could operate on vector registers, though I donʼt know yet whether that would lead to any speedups!]).

Itʼs simpler to start with a function that checks for one type of redex: in this case, `S` redexes (because they represent nontrivial evaluation).
This can be done in ~12 operations.

```C
// All bits set
static const word word_max = ~0;
// Lower bit of each crumb
static const word lower = word_max / 0b11;

// Coalesce each nonzero crumb (pair of bits) into its lower bit.
word nonzeros(word crumbs) {
  return lower & (crumbs | (crumbs >> 1));
}

// Calculate if there are any `S` redexes exclusively, since `I` and `K` are
// not really a problem for duplicated work. Assumes `leadingZeroBits = 3`
// (or that the word is well-formed).
word nontrivial_redexes(word crumbs) {
  word S;
  // Any nonzero crumb means that the
  // next 3 crumbs cannot be `S` redexes
  S = crumbs >> 2 | crumbs >> 4 | crumbs >> 6;
  // Any any crumb that is not `0b11 = 0q3`
  // cannot be an `S` redex
  S |= crumbs ^ (0b11 * lower);
  // The redexes are then the crumbs where
  // this `S` is zero, so we coalesce them
  // and we give them the value `0b11 = 0q3`
  return ~(0b11 * nonzeros(S));
}
```

Triplicating this lets us share some work and brings out the patterns:

```C
static word redexes(u8 leadingZeroBits, word crumbs) {
  word I, K, S;
  I = 0 | crumbs >> 2;
  K = I | crumbs >> 4;
  S = K | crumbs >> 6;
  switch (leadingZeroBits >> 1) {
    case 0:
      I |= ~(word_max >> 2);
      K |= ~(word_max >> 4);
      S |= ~(word_max >> 6);
      break;
    case 1:
      K |= ~(word_max >> 2);
      S |= ~(word_max >> 4);
      break;
    case 2:
      S |= ~(word_max >> 2);
      break;
    default:
      break;
  }
  S |= crumbs ^ (0b11 * lower);
  K |= crumbs ^ (0b10 * lower);
  I |= crumbs ^ (0b01 * lower);
  word ruled_out = (I | I >> 1) & (K | K >> 1) & (S | S >> 1);
  return crumbs & ~(0b11 * (lower & ruled_out));
}
```

#### Grammar

We also want to do some bithacking for the grammar of operators.
Especially once weʼve identified a redex, we need to scan ahead to identify all of its arguments.

We traverse the `<operand>` grammar by keeping track of an expected number of remaining operands.
Each `0`{.st} crumb adds one, and each nonzero crumb (`1`{.st}, `2`{.st}, or `3`{.st}) subtracts one.

The bithacking to keep track of this is really efficient, just 6 operations:

```C
// Number of bits in a word
static const word word_size = 8*sizeof(word);
// Number of *crumbs* in a word
static const word word_crumbs = word_size / 2;


s8 deltaExpecting(word crumbs) {
  return 2 * popcnt(nonzeros(crumbs)) - word_crumbs;
}
```

`word nonzeros(word)`{.c} is above. `word popcnt(word)`{.c} is short for “population count”: the number of bits that are `1`{.dv} in the word, also known as the [Hamming weight](https://en.wikipedia.org/wiki/Hamming_weight).

:::Warning
Note that this might be backwards from what you expect: `s8 deltaExpecting(word crumbs)`{.c} is expected to be *subtracted* from expecting: `expecting -= deltaExpecting(word);`{.c}
 This is so that it is easier to compare them as unsigned numbers.
:::

The next challenge is to identify where inside a word does the expected number of remaining operands drop to zero: when is the expression *complete*.

There are sufficient conditions to *not* be a complete expression: after leading zeros, the value of `expecting` is `expecting + #leading0s`{.c}, so if the population count of the word overall is less than that, it never reaches zero.

We could technically do a kind of binary search, and/or skip through contiguous zeros, but I have not implemented that yet.

```C
u8 reachesZero(word expecting, word crumbs) {
  word _nonzeros = nonzeros(crumbs);

  if (expecting + (clz(_nonzeros) >> 1) > popcnt(_nonzeros))
    return 0;

  u8 bit = word_size - 2;
  while (expecting += (0b1 & (_nonzeros >> bit) ? -1 : 1)) {
    if (!bit) return 0;
    bit -= 2;
  }
  return word_size - bit;
}
```

We combine these to traverse through a whole crumbstring, for example.

```C
// Calculate the length of the first operand of the passed crumbstring, which is
// more efficient if `known_words != 0` is passed (otherwise it has to examine
// each crumb).
ptrbit ptrbitlength(const word* crumbstring, word known_words) {
  word i = 0; word exp = 1; u8 offset = 0;
  // Quickly scan through `known_words - 1`, using just
  // `deltaExpecting` and not `reachesZero`
  while (i < known_words - 1) {
    exp -= deltaExpecting(crumbstring[i++]);
    assert(exp != 0);
  }
  // Fall through, in case `known_words` was too short.
  // (This is used in `ap_heap_cat`.)
  while (!offset) {
    // See if it reaches zero during the current word
    offset = reachesZero(exp, crumbstring[i]);
    if (!offset) {
      // If not, we need to continue with the next word
      // and adjust the expected number of operands
      exp -= deltaExpecting(crumbstring[i++]);
      assert(exp != 0);
      continue;
    }
  }
  // It needs to be even since it is a bit index of a crumb
  assert((offset & 1) == 0);
  // `i` denotes the index of the word that reached zero
  return (ptrbit) { .ptr = sizeof(word) * i, .bit = offset };
}

// Length of an operand in bits in a crumbstring.
word bitlength(const word* crumbstring, word known_words) {
  return 2 * ptrbit2crumbs(ptrbitlength(crumbstring, known_words));
}
```

## WASM

So I mentioned that I originally wanted to target WASM (WebAssembly), and write it by hand to boot.
One of the main reasons for this choice was that I like web programming, and I use PureScript for my blog, so it makes sense to try out WASM and see if it really is as performant as it says it is.
Plus it is a nice sandboxed format that can be run anywhere.

### WASM by hand

I wrote most of the first naïve evaluator by hand, in the standardized WebAssembly Text format (WAT).
I actually got it working at least somewhat: it was able to handle simple reductions just fine, but I couldnʼt get it to handle the factorial function I wanted to “prove” it with.
Debugging it was pretty painful, and it was hard to look back at it and want to jump back in.

And there was no way I was going to implement a real evaluator instead of the naïve evaluator, which really did operate on crumbstrings:

- it would search through the string from left-to-right, copying irreducible parts;
- then it would find a redex, parse its arguments off of the crumbstring,
- it would then reassemble the crumbstring with the redex evaluated: copying the arguments from the input to the new positions in the output;
- finally, it would finish copying the input to the output, and then memcpy the output back over the input.

Yeah. Incredible slow for large reductions, but pretty fast for small ones.^[I didnʼt bother to measure its performance exactly, since it was buried in JavaScript test helpers that surely dominated the evaluation time by doing their own string parsing.]

### To the C!

It turns out that porting it to C wasnʼt too difficult.
I found and fixed a couple bugs along the way: soon enough, it was working just as well as the previous one did.
And after a bit more debugging I even got the factorial function working, a huge success!

I honestly didnʼt bother trying to find the bugs back in the WASM: some would have been obvious, but others were just mysteriously fixed and I was okay with that.

And running it in C was barely a compromise: the interface of the WASM module changed, since clang, but I was able to keep it bare-bones in other ways: no stdlib, no malloc, just clang/LLVM builtins.

I did that with the help of this blog post (beautiful blog layout btw): [“Compiling C to WebAssembly without Emscripten”](https://surma.dev/things/c-to-webassembly/).
The basic command is `clang --target=wasm32 -nostdlib`{.sh}, with some tweaks along the way.^[I ran into strange bugs without `-Wl,--export-all`, so I guess you kind of have to use that. It looked like the optimizer was assuming that the global arrays were zeroed out.]
This gets you from C to LLVM to WASM about as lightweight and minimally as possible: you have pretty good control over the instructions you are writing, and you benefit from the C typechecking (especially structs!) and LLVM optimizations (like inlining and constant folding).

It passed the basic tests I threw at it, but performance of the factorial function dropped off steeply, due to all of the copying and inefficient evaluation:

+-------------------------+-----------------+-------+-------------------+
| Test                    | Total&nbsp;time | Tests | Ratio             |
+:========================+================:+:======+==================:+
| `factorial(0)`{.nowrap} | 6.21ms          | x18   |                   |
+-------------------------+-----------------+-------+-------------------+
| `factorial(1)`{.nowrap} | 21.94ms         | x18   | 3.5x&nbsp;longer  |
+-------------------------+-----------------+-------+-------------------+
| `factorial(2)`{.nowrap} | 354.50ms        | x18   | 16.2x&nbsp;longer |
+-------------------------+-----------------+-------+-------------------+
| `factorial(3)`{.nowrap} | 462.44ms        | x1    | 23.5x&nbsp;longer |
+-------------------------+-----------------+-------+-------------------+

`factorial(4)` fails to evaluate properly, apparently, and I do not feel like debugging it, but it took over 15 seconds to reach the wrong answer, which was a fairly small expression, so it was probably indicative of the rough runtime.

_insert acknowledgement that these numbers do not deserve this level of reported precision_

#### Aside: Church numerals and factorials

If you are curious, here is an encoding of the factorial function in Quapteryx:

```{.st .wrap}
00030200300322003220030200330200300322003222003020030203003003003003220202032022032030030232003020033020030200302030200330203203020030200330222003020033020030200302030200302030032220300322222
```

This is adapted from [“A Combinatory Compiler”](https://crypto.stanford.edu/~blynn/lambda/sk.html) by Ben Lynn, which also compiles combinator calculus to WASM, but in a different way!

The normal form of Church-encoded natural numbers look like `032`{.st}, `0030030232032`{.st}, `00300302320030030232032`{.st}, and so on, or `n*"0030030232" + "032"`{.python}.
This is \((SB(...(SB(SK))))\), where `0030232`{.st} is the encoding of the \(B\) combinator: function composition.

That is, once you apply an argument \(f\) to it, it reduces to \(Bf(Bf(...Bf(I)))\), or the \(n\)-fold composition of \(f\).

***However***, you need to be careful: just because the expression is evaluated, doesnʼt mean that the Church number is in normal form!
Because of the lack of eta-equivalence in this implementation of combinator calculus, it will evaluate to an expression with the same behavior but spelled a different way in the combinators.

For example, the normal form of `factorial(0)` is `0032032`{.st} \((SK)(SK) \approx_\eta I\), not `0030030232032`{.st} \((SB)(SK)\).
The normal form of `factorial(1)` is 3359 crumbs *long*, `factorial(2)` is 2573 crumbs long (not sure why it is shorter), `factorial(3)` is 3559 crumbs long, and the rest grow comparatively slowly from there.

Thus my tests went through various irreducible values of `m` and `n` to evaluate `factorial(3)(m)(n)` and ensure that it evaluated to `0m0m0m0m0m0mn` each time, and so on.

:::centered
_Read on at [Quapteryx Part IV: Efficient Quapteryx Evaluator](quapteryx4.html)._
:::

