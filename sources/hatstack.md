---
title: HatStack
subtitle: "A stack based, concatenative language for [Hereditarily Finite Sets](hereditarily_finite_sets.html)"
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
preroll: '<span data-widget="Parser.Main.HFS"></span>'
---

<script src="assets/js/hfs.js"></script>

HatStack is a unityped stack language, kind of like Forth!
There is only one type: [Hereditarily Finite Sets](hereditarily_finite_sets.html), represented as arbitrary-precision natural numbers using the [Ackermann coding](https://en.wikipedia.org/wiki/Hereditarily_finite_set#Ackermann_coding).
Except we are sneaky, and it has multiple representations under the hood:

```purescript
data HFS
  = NumLike Base BigNat
  | SetLike (Set HFS)
data Base = Bin | Dec | Hex | Oct | Qua
```

This is both good for preserving user intent (helping you remember if you intended a datum to be a number or set, maybe) and necessary for efficiency, indeed, feasibility.

For example, `{{{{{{{}}}}}}} 2 65536 ^. ==`{.hatstack} evaluates to true (in infix notation: `({{{{{{{}}}}}}} == 2 ^ 65536)`), meaning that
even relatively small sets like these would require huge `BigInt`{.js}s to display, due to the [power tower](https://en.wikipedia.org/wiki/Tetration) nature of the numeric representation.
And in fact, `2 65536 ^.`{.hatstack} and `1 65536 <<.`{.hatstack} are both special-cased to compute as the singleton set `{65536}`{.hatstack} instead of a natural number with 65537 bits.

## Documentation

The implementation consists of:

- A stack of stacks of HFSes
  - Braces `{…}`{.hatstack} and brackets `[…]`{.hatstack} add a new stack, so they can keep track of how many items were added.
    They differ in [important ways](#data-and-stack-manipulation), make sure to read up!
  - `.0`{.hatstack}, `.1`{.hatstack}, [etc.]{t=} refer to ***a*** previous stack while `$0`{.hatstack}, `$1`{.hatstack}, [etc.]{t=} refer to the current stack.
    Nested brackets `[…]`{.hatstack} increment the stack that `.0`{.hatstack} [et al.]{t=} refer to, while nested braces `{…}`{.hatstack} keep it the same.
- Functions (procedures) and variables.^[It is more appropriate to call them procedures, since they are not higher-order functions.]
- A control stack to keep track of constructs like `if…(else)…end`{.hatstack} and `begin…while…end`{.hatstack}.
- Current error/skipping state, [etc.]{t=}

It is *literally* concatenative and interpreted: there is no compile-time checking, all the control flow constructs interact on the control flow stack.

Even set notation `{…}`{.hatstack} and stack notation `[…]`{.hatstack} are concatenative!

:::{.Key_Idea box-name="Key Idea: Length-Headed Lists on the Stack" #length-headed-lists}
Since we have access to the stack, we often encode lists on it as a length (at the top of the stack) followed by that number of items.

For example, `{} {{}} 2 {#}`{.hatstack} tells `{#}`{.hatstack} to pop `2`{.hatstack} and construct a new set out of the next **2** items on the stack, [viz.]{t=} `{}`{.hatstack} (the empty set) and `{{}}`{.hatstack} (a singleton set).
This makes the set `{ {{}}, {} }`{.hatstack}.
:::

### Data and Stack Manipulation

There are eight grouping operators, of various usefulness:

- the brackets `#[`{.hatstack}, `[`{.hatstack}, `]`{.hatstack}, `]#`{.hatstack}, for manipulating the stack
- the braces `#{`{.hatstack}, `{`{.hatstack}, `}`{.hatstack}, `}#`{.hatstack}, for building sets

Each opening operator creates a new stack on top of the existing stacks, though their semantics differ.
Each closing operator will close off the stack, and they must correspond to the same shape of operator (you cannot mix and match braces and brackets).

They are used in combination with variables `.0`{.hatstack}, `.1`{.hatstack}, [et al.]{t=} to bring in data from the outside stacks.
You may also use the variables `$0`{.hatstack} and so on to refer to the *current* stack, whatever that is, just like you can at the top level.
(So `$0`{.hatstack} always means `dup`{.fu}, for example.)

#### Stack manipulation

The stack manipulation operators are really cool.
As a warmup, `swap`{.fu} can be expressed as `2 #[ .0 .1 ]`{.hatstack}.
We can describe its execution on the stack `{} {{}}`{.hatstack} like so:

- Queue the deletion of `2`{.hatstack} items from the stack, namely `{}`{.hatstack} and `{{}}`{.hatstack}
- Create a new stack
- Push `.0`{.hatstack} from the previous stack, this refers to `{{}}`{.hatstack}
- Push `.1`{.hatstack} from the previous stack, this refers to `{}`{.hatstack}

  :::Note
  Notice that the indexing has not changed, since we pushed to a different stack than we are referencing!
  This is why it is so cool.
  You can just say what you want, without having to think about how each operation will affect the indices you are referring to!
  :::
- Delete the queued items `{} {{}}`{.hatstack} from the previous stack
- Pop the new stack, appending its items `{{}} {}`{.hatstack} onto the previous stack (thus the opposite order of before ~)

#### Set creation



#### Quickref

`#[`{.hatstack}
:   Immediately pop a number and queue deletion of that many items from the stack.
    Push a new stack and make the previous stack referenced by `.0`{.hatstack} and so on.

` [`{.hatstack}
:   Push a new stack and make the previous stack referenced by `.0`{.hatstack} and so on.

`] `{.hatstack}
:   Run the queued deletions and pop the new stack and append its items to the previous stack.

`]#`{.hatstack}
:   Run the queued deletions and pop the new stack and append its items to the previous stack.
    Also push the length of the popped stack, to tell you how many items were pushed.

    That is, this operator is meant for creating [length-headed lists](#length-headed-lists).

`#{`{.hatstack}
:   Immediately pop a number and immediately float that many items, who will appear as members of the final result.
    Then push a new stack, but do **not** reference the previous stack with `.0`{.hatstack} [et al.]{t=} (unless it was the only stack!).

` {`{.hatstack}
:   Push a new stack, but do **not** reference the previous stack with `.0`{.hatstack} [et al.]{t=} (unless it was the only stack!).

`} `{.hatstack}
:   Pop the new stack and push a single item, being the set of items from the popped stack along with any elements floated by `#{`{.hatstack}.

`}#`{.hatstack}
:   Additionally push the length of the popped stack.
    Equivalent to `} dup count`{.hatstack}.

#### Implementation

### Operators

There are binary operators, which pop two and push one, and n-ary operators which pop their arity, pop that many operands, and push the result.
(In general it is common for functions to take a variable number of arguments in this way, see [length-headed lists](#length-headed-lists).)
Some operators will throw an error if they are called as 0-ary, since they do not have an identity.

There are three syntactic categories:

1. Symmetric operators like `+`{.op} and `==`{.op}.
2. Sided operators like `.-`{.op}/`-.`{.op} and `.\`{.op}/`\.`{.op}, where the `.`{.op} indicates which side the top of the stack goes to.
3. Orderings like `<`{.op} (\(<\)) and `=|`{.op} (\(\subseteq\)).

<details class="Details full-width">
<summary id="list-of-binary-operators">List of Binary Operators</summary>
:::{widget="Parser.Main.HFS.ops_binary"}
:::
</details>

There are [N-ary]{t=} versions of all of the operators: `#+`{.op}/`+#`{.op} and `#-`{.op}/`-#`{.op} and so on.
For symmetric ones, the two operators are equivalent, but for sided operators you probably want to use the left-sided variants like `#-`{.op}, which means “repeatedly subtract the lower items from the top of the stack”, instead of `-#`{.op} which is an alternating subtraction.

Most of them are implemented as stack folds (of the natural associativity: top of the stack is evaluated first and the tail is folded in).
Additionally, there are some special cases:

- `#==`{.op} checks that *all* operands are equal,
- `#!=`{.op} checks that *no* operands are equal,
- `#><`{.op} returns the *only* nonzero operand, or zero,
- and the ordering relations like `#<`{.op}, `#@`{.op}, and `#=>`{.op} apply transitively: `#<`{.op} checks if the list is ascending from tail to head (for consistency of `2#<`{.hatstack} with `<`{.op}), and so on.

<details class="Details full-width">
<summary id="list-of-n-ary-operators">List of N-Ary Operators</summary>
:::{widget="Parser.Main.HFS.ops_n_ary"}
:::
</details>

### Control Flow

All blocks end with the `end`{.hatstack} keyword!
As mentioned above, there is no compile-time checking for these constructs, they just interact on the stack in a way that works with these conventions.

- `begin…end`{.hatstack} (no-op, does **not** loop)
- `begin…while…end`{.hatstack} (loops), with `continue`{.hatstack} and `break`{.hatstack}
  - `while`{.hatstack} pops the condition, `end`{.hatstack} loops back to `begin`{.hatstack}
  - This gives you both `while () {}`{.c} and `do {} while()`{.c} and an unholy mix of the two!
- `if…end`{.hatstack} and `if…else…end`{.hatstack}
  - `if`{.hatstack} pops the condition
- `match…end`{.hatstack} and `match…else…end`{.hatstack}
  - `match`{.hatstack} acts like `$1 == if`{.hatstack}: it pops the top of the stack and takes the branch if it matches the item, which it leaves on the stack for another match (and also the branch body)
- `def FUN…end`{.hatstack} with `return`{.hatstack}
- `set VAR`{.hatstack}: pops a value and sets `VAR`{.fu} to that value, `VAR`{.fu} will push that value ([e.g.]{t=} `dup set VAR`{.hatstack} is the same as `set VAR, VAR`{.hatstack})
- `alias OLD NEW`{.hatstack}: `NEW`{.fu} now refers to `OLD`{.fu}
- `try…catch…end`{.hatstack} with `throw`{.hatstack} and `rethrow`{.hatstack}
- `recover…end`{.hatstack} with `rethrow`{.hatstack} (maybe scuffed??)
- `exit`{.hatstack}

### Builtins/stdlib

`Set`{.hatstack}, `Nat`{.hatstack}/`Num`{.hatstack}, `Bin`{.hatstack}, `Qua`{.hatstack}, `Oct`{.hatstack}, `Dec`{.hatstack}, `Hex`{.hatstack}, `HFS`{.hatstack}
:   Type hints: set a preferred display style for the HFS at the top of the stack.
    `Set`{.hatstack} is shallow while `HFS`{.hatstack} is deep.
    `Nat`{.hatstack}/`Num`{.hatstack} preserve the current base, but default to hexadecimal.

    :::Warning
    Do not turn deep sets into numbers!
    HFSes with depth greater than 6 are (probably) too large to fit into a `BigInt`{.js}.
    :::

#### Sets

`single`{.fu} (`{.}`{.hatstack}, `1#{}`{.hatstack}, `1 .<<`{.hatstack}, `2 .^`{.hatstack})
:   Create a singleton set (pop one, push one).

`pack`{.fu} (`{#}`{.hatstack})
:   Turn a stack-list into a set.

`unpack`{.fu} (`#`{.hatstack})
:   Turn a set into a stack-list, with the smallest member on top (after the length of the set).

`powerset`{.fu} (`𝒫`{.hatstack})
:   Return the set of all subsets of the argument, of size \(2^n\).

#### Booleans

<!-- 22A5 -->
`false`{.fu}, `⊥`{.hatstack} (`0`{.hatstack}, `{}`{.hatstack})
:   An empty set.

<!-- 22A4 -->
`true`{.fu}, `⊤`{.hatstack} (`1`{.hatstack}, `{{}}`{.hatstack})
:   A nice non-empty set.

`not`{.fu}, `¬`{.hatstack}
:   Returns `1`{.hatstack} if `0`{.hatstack}, else `0`{.hatstack}.

#### Ordered pairs and maps

Finite maps are encoded as [graphs of functions](https://en.wikipedia.org/wiki/Graph_of_a_function#Definition): a set of pairs indicating the mapping from domain to range.

`pair`{.fu}, `>-`{.hatstack}
:   Make an ordered pair, where the top item of the stack `$0`{.hatstack} is considered the first element of the pair, and the second item `$1`{.hatstack} the second element.

    :::Note
    To make a map (finite function), create a set of pairs, where the first elements form the domain and the second elements form the range.

    For example, the map `{ 1: 5, 4: 7 }`{.py} can be written as,
    ```hatstack
    {
      5 1 pair
      7 4 pair
    }
    ```

    This can be applied with `1 apply`{.hatstack} and `4 apply`{.hatstack}, and any other `apply`{.hatstack} will fail.

    Note that it looks backwards!
    :::

`unpair`{.fu}, `-<`{.hatstack}
:   Destruct an ordered pair, putting the first element on top of the stack with the second element below it.
    Will throw if its argument is not an ordered pair.

`apply`{.fu}, `@@`{.hatstack}
:   Apply an argument `$0`{.hatstack} to the finite map `$1`{.hatstack}, looking it up.
    Will throw if it discovers the map is invalid, or if the argument is not in the map.

#### Stack shuffling

`dup`{.fu} (`$0`{.hatstack}, `[.0]`{.hatstack} or `1#[.0 .0]`{.hatstack})
:   Duplicate the top item on the stack.

`swap`{.fu} (`2#[.0 .1]`{.hatstack})
:   Swap the top two items.

`rot`{.fu} (`3#[.1 .0 .2]`{.hatstack})
:   Pull the third item to the top.

<!-- bury -->

#### Stdlib source

<!-- :::{widget="Parser.Main.HFS.stdlib"}
::: -->

```hatstack {load-from=assets/misc/stdlib.hatstack}
```

### Glossary

stack–items
list–items
set–members
number
map
function
pair–elements

### Algebra


