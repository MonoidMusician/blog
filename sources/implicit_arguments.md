---
title: User Operators with Implicits & Overloads
subtitle: In bidirectional type checkers
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Okay, let me start off by saying I’m not a huge fan of bidirectional type theory.
It is not a satisfying explanation of typechecking for me, I feel like you run into its limitations way too quickly.^[I feel like I can say this now that I’ve worked on getting [PolyTT](https://github.com/ToposInstitute/polytt) off the ground!]

However, it is quite useful, especially for quickly getting started with a type theory implementation.
It has a specific philosophical tack, which is commendable (“redexes need annotations”).
And it supports some nice features.

In particular it lets you hack in cheap overloads and coercions (both fake and real) – but in a limited manner.^[By fake coercion, I mean a coercion that happens only in the elaborator for specific syntax – “elaboration hacks” IOW. Real coercions operate on arbitary terms, when actual and expected types are different but coercible, but these require more systematic incorporation into the theory.]

Overloads, in this context, include operators that morally take implicit arguments.
Of course, for builtins, the typechecker can do whatever it want, but a common feature is that it is used for resolving types that would be implemented as implicit arguments.

However, these overloads and coercions are limited to _builtin_ types and functions.
They need to be baked into the implementation.

In particular, if you’re implementing a dependent type theory and choose a naïve bidirectional type checking algorithm, you’re limiting implicit arguments to be only for builtins.

And any user of that theory is going to be frustrated at the verbosity of it.
And maybe a little miffed at the unfairness of it.

How come the implementer keeps the cool tricks to herself?!

Let’s try to fix this.
How can we give the user access to implicit arguments for their own functions too?

As usual with type theory, it requires being more disciplined and systematic.
Formalizing the unstated conventions underpinning common practice – this is the bread and butter of type theorists!

## Game plan

Our first observation comes from a page stolen out of the builtins’ book:

:::Key_Idea
Require that user-defined operators are fully applied enough, in order to infer implicits.
:::

What does this mean, “enough”?
We’ll need to do some work to figure it out.

But what it accomplishes is clear:

Knowing that we have some arguments applied already lets us _syn_ them, and use the resulting type information to fill in the implicits.

:::{.Note}
I will be using _syn_ and _chk_ as verbs freely.
These are the synthesize and check modes of bidirectional type theory.
:::

Thus we would say that we need to be able to _syn_ enough arguments to determine every implicit the user asked for.
The remaining arguments can (and should) be _chk_​ed – and since normal function arguments are also _chk_​ed, this means they can be unapplied arguments.

:::Note
I won’t be addressing how to parse user-defined operators.
That’s a difficult enough problem on its own.

Just note that I’ll include prefix operators that look like normal function application (separated by a space), but differ in requiring a certain number of arguments to be applied.
In fact, these are the easiest to parse, and the best source of examples.
:::

We can also flip the tables and infer implicits if we start in _chk_ mode in the first place!

In fact, this is strictly better: if we are typing the (partially-applied, or not) binding in _chk_, we know the types of the arguments _and_ the return type.

Thus the main improvement in this case is (wait for it): emulating mixed-mode typing for functions.
(Having some arguments in _syn_, and determining some information from the expected type `T` of the whole partially applied function `f a b : T`.)

Which is why you shouldn’t even be using bidirectional typing in the first place! Urgh!
(Unification handles this exact problem more elegantly. And it forces you to make your coercions coherent!)

Thus we’ve already gone from “let’s add implicits to bidirectional typing” to “let’s emulate unification except worse”.

In particular, one way that it is worse is that we have to commit to modes up-front: we can’t decide to _try_ infering this argument as _syn_, then fall back to infering something else and returning to _chk_ it, etc.

The other way that it is worse is that we can’t provide partial types to anything.

So we’ll see what we’re able to make of it.

### Inferring patterns of _syn/chk_ for arguments

Examples:

```{.agda .fake}
id {A : Type} : ~syn:A → ~syn:A
iter {A : Type} : ~chk:(n : Nat) → ~syn:(A → A) → ~syn:(A → A)

const_Tt {A : Type} : Π ~chk:(B : Type), ~syn:A → ~syn:(B → A)
const_tT {A : Type} : ~syn:A → ~syn:(Π (B : Type), (B → A))
const_t {A B : Type} : ~chk:(A → B → A)
const_tt {A B : Type} : ~syn:A → ~syn:B → ~syn:A

compose {A B C : Type} : ~syn:(B → C) → ~syn:(A → B) → ~syn:(A → C)
compose {P Q R : Poly} : ~syn:(Q ⇒ R) → ~syn:(P ⇒ Q) → ~syn:(P ⇒ R)
compose : ~syn:Poly → ~chk:Poly → ~syn:Poly


```

```{.agda .fake}
~syn:(A → B)
===============
~chk:A → ~syn:B
```

Rule -1: if an argument mentions none of the implicit parameters, it is in _chk_.
Rule 0: we typecheck strictly from left to right

#### Starting in _syn_

#### Starting in _chk_

As mentioned above, we have more flexibility in _chk_ mode.

### Picking apart types

One word: injectivity.

#### Dependent types

Eurgh.

#### Faking it

For some implicits, they may not be used directly.
E.g. `(a -> b) -> (fin n -> a) -> (fin n -> b)`.
Ugh but that gets weird semantically though.
Parameters.

Sounds like open research.

Or QTT (Quantitative Type Theory), with quantity 0.

### Overloads

Now that we’re picking apart types, we might as well throw in overloads.

```{.agda .fake}
compose {A B C : Type}
  : ~syn:(B → C) → ~syn:(A → B) → ~syn:(A → C)
  : ~syn:(B → C) → ~chk:(A → B) → ~chk:(A → C)
  : ~chk:((B → C) → (A → B) → (A → C))
compose {P Q R : Poly}
  : ~syn:(Q ⇒ R) → ~syn:(P ⇒ Q) → ~syn:(P ⇒ R)
  : ~syn:(Q ⇒ R) → ~chk:(P ⇒ Q) → ~chk:(P ⇒ R)
  : ~chk:((Q ⇒ R) → (P ⇒ Q) ⇒ (P ⇒ R))
compose
  : ~syn:Poly → ~chk:Poly → ~syn:Poly
  : ~chk:Poly → ~chk:Poly → ~chk:Poly
  : ~chk:(Poly → Poly → Poly)
```

The main rule is that before we are able to narrow down the overload, we must be able to tell what mode to run in.

Then we want to narrow it down in a sensible way.
Either we want to outlaw overlapping instances, or we want to match in order of most specific to least (toposort the partially-order DAG).
Certainly by the end of matching, we want to be left with one unambiguous thing.

In this case, if we are checking a partially-applied `compose`{.agda .fake} in _syn_ mode, we know that the first argument needs to be _syn_​ed, regardless of which overload.
If it ends up having type `Poly`{.agda .fake}, we can actually _chk_ the next argument against `Poly`{.agda .fake} (and synthesize `Poly`{.agda .fake} for the return type).
If it is one of the other overloads, the next argument needs to be _syn_​ed as well (to cover the missing implicit `A`{.agda .fake} or `P`{.agda .fake}),^[This is where mixed-mode would be especially handy lol: we could tell the typechecker we are expecting a function or polynomial morphism respectively (one bit of info), and in particular a function into type `B`{.agda .fake} or polynomial morphism into poly `Q`{.agda .fake} respectively (second bit of info).] at which point we know enough to synthesize the return type.

## Implementation

## Philosophy

### Mixed-mode

Where I rant about unification.
