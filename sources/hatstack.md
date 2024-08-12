---
title: HatStack
subtitle: "A stack based, concatenative language for [Hereditarily Finite Sets](hereditarily_finite_sets.html)"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
preroll: '<span data-widget="Parser.Main.HFS"></span>'
---

<script src="assets/js/hfs.js"></script>

HatStack is a unityped stack language, kind of like Forth!
There is only one type: Hereditarily Finite Sets, represented as arbitrary-precision natural numbers.
Except we are sneaky, and it has multiple representations under the hood:

```purescript
data HFS
  = NumLike Base BigNat
  | SetLike (Set HFS)
data Base = Bin | Dec | Hex
```

This is both good for preserving user intent (helping you remember if you intended a datum to be a number or set, maybe) and necessary for efficiency, indeed, feasibility.

For example, `{{{{{{{}}}}}}} 2 65536 ^. ==`{.hatstack} evaluates to true (in infix notation: ``({{{{{{{}}}}}}} == 2 ^ 65536)`), meaning that
even relatively small sets like these would require huge `BigInt`{.js}s to display, due to the [power tower](https://en.wikipedia.org/wiki/Tetration) nature of the numeric representation.
And in fact, `2 65536 ^.`{.hatstack} and `1 65536 <<.`{.hatstack} are both special-cased to compute as the single set `{65536}`{.hatstack} instead of a natural number with 65537 bits.

## Documentation

The implementation consists of:

- A stack of stacks of HFSes
  - Braces `{…}`{.hatstack} and brackets `[…]`{.hatstack} add a new stack, so they can keep track of how many items were added.
  - `.0`{.hatstack}, `.1`{.hatstack}, [etc.]{t=} refer to the previous stack while `$0`{.hatstack}, `$1`{.hatstack}, [etc.]{t=} refer to the current stack
- Functions (procedures) and variables.
- A control stack to keep track of constructs like `if…(else)…end`{.hatstack} and `begin…while…end`{.hatstack}.
- Current error/skipping state, [etc.]{t=}

It is *literally* concatenative and interpreted: there is no compile-time checking, all the control flow constructs interact on the control flow stack.

Even set notation `{…}`{.hatstack} and stack notation `[…]`{.hatstack} are concatenative!

:::{.Key_Idea box-name="Key Idea: Lists on the Stack"}
Since we have access to the stack, we often encode lists on it as a length (at the top of the stack) followed by that number of elements.

For example, `{} {{}} 2 {#}`{.hatstack} tells `{#}`{.hatstack} to pop `2`{.hatstack} and construct a new set out of the next **2** elements on the stack, [viz.]{t=} `{}`{.hatstack} (the empty set) and `{{}}`{.hatstack} (a singleton set).
This makes the set `{ {{}}, {} }`{.hatstack}.
:::

### Operators

There are binary operators, which pop two and push one, and n-ary operators which pop their arity, pop that many operands, and push the result.
(In general it is common for.)
Some operators will throw an error if they are called as 0-ary, since they do not have an identity.

There are three syntactic categories:

1. Symmetric operators like `+`{.op} and `==`{.op}.
2. Sided operators like `.-`{.op}/`-.`{.op} and `.\`{.op}/`\.`{.op}, where the `.`{.op} indicates which side the top of the stack goes to.
3. Orderings like `<`{.op} and `|=`{.op}.

There are [N-ary]{t=} versions of the symmetric and sided operators: `#+`{.op}/`+#`{.op} and `#-`{.op}/`-#`{.op} and so on.
For symmetric ones, the two operators are equivalent, but for sided operators you probably want to use the left-sided variants like `#-`{.op}, which means “repeatedly subtract the lower items from the top of the stack”, instead of `-#`{.op} which is an alternating subtraction.

<details class="Details full-width">
<summary>Complete list of operators</summary>
:::{widget="Parser.Main.HFS.ops"}
:::
</details>

### Data and Stack Manipulation

`{…}`{.hatstack}, `#{…}`{.hatstack}, `[…]`{.hatstack}, `#[…]`{.hatstack}.

### Control Flow

All blocks end with the `end`{.hatstack} keyword!
As mentioned above, there is no compile-time checking for these constructs, they just interact on the stack in a way that works with these conventions.

- `begin…end`{.hatstack} (no-op, does **not** loop)
- `begin…while…end`{.hatstack} (loops), with `continue`{.hatstack} and `break`{.hatstack}
  - `while`{.hatstack} pops the condition, `end`{.hatstack} loops back to `begin`{.hatstack}
  - This gives you both `while () {}`{.c} and `do {} while()`{.c} and an unholy mix of the two!
- `if…end`{.hatstack} and `if…else…end`{.hatstack}
  - `if`{.hatstack} pops the condition
- `def…end`{.hatstack} with `return`{.hatstack}
- `try…catch…end`{.hatstack} with `throw`{.hatstack} and `rethrow`{.hatstack}
- `recover…end`{.hatstack} with `rethrow`{.hatstack} (maybe scuffed??)

### Builtins/stdlib

`pack`{.hatstack} (`{#}`{.hatstack})
:   Turn a stack-list into a set.

`unpack`{.hatstack} (`$#`{.hatstack})
:   Turn a set into a stack-list, with the smallest member on top (after the length of the set).

`single`{.hatstack} (`{.}`{.hatstack}, `1 .<<`, `2 .^`{.hatstack})
:   Create a singleton set (pop one, push).

`dup`{.hatstack} (`[.0]`{.hatstack} or `1#[.0 .0]`{.hatstack})
:   Duplicate the top item on the stack.

`swap`{.hatstack} (`2#[.0 .1]`{.hatstack})
:   Swap the top two items.

`rot`{.hatstack} (`3#[.1 .0 .2]`{.hatstack})
:   Pull the third item to the top.

<!-- bury -->

`Set`{.hatstack}, `Nat`{.hatstack}/`Num`{.hatstack}, `Bin`{.hatstack}, `Dec`{.hatstack}, `Hex`{.hatstack}, `HFS`{.hatstack}
:   Type hints: set a preferred display style for the HFS at the top of the stack.
    `Set`{.hatstack} is shallow while `HFS`{.hatstack} is deep.
    `Nat`{.hatstack}/`Num`{.hatstack} preserve the current base, but default to hexadecimal.

    :::Warning
    Do not turn deep sets into numbers!
    HFSes with depth greater than 6 are (probably) too large to fit into a `BigInt`{.js}.
    :::

`false`{.hatstack} (`0`{.hatstack}, `{}`{.hatstack})
:   Another empty set.

`true`{.hatstack} (`1`{.hatstack}, `{{}}`{.hatstack})
:   A nice non-empty set.
