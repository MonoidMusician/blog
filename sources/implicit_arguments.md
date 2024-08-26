---
title: User Operators with Implicits & Overloads
subtitle: In bidirectional type checkers
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Okay, let me start off by saying Iʼm not a huge fan of bidirectional type theory.
It is not a satisfying explanation of typechecking for me, I feel like you run into its limitations way too quickly.^[I feel like I can say this now that Iʼve worked on getting [PolyTT](https://github.com/ToposInstitute/polytt) off the ground!]

However, it is quite useful, especially for quickly getting started with a type theory implementation.
It has a specific philosophical tack, which is commendable (“redexes need annotations”).
And it supports some nice features.

In particular it lets you hack in cheap overloads and coercions (both fake and real) – but in a limited manner.^[By fake coercion, I mean a coercion that happens only in the elaborator for specific syntax – “elaboration hacks” in other words. Real coercions operate on arbitary terms, when actual and expected types are different but coercible, but these require more systematic incorporation into the theory.]

Overloads, in this context, include operators that morally take implicit arguments.
Of course, for builtins, the typechecker can do whatever it want, but a common feature is that it is used for resolving types that would be implemented as implicit arguments.

However, these overloads and coercions are limited to _builtin_ types and functions.
They need to be baked into the implementation.

In particular, if youʼre implementing a dependent type theory and choose a naïve bidirectional type checking algorithm, youʼre limiting implicit arguments to be only for builtins.

And any user of that theory is going to be frustrated at the verbosity of it.
And maybe a little miffed at the unfairness of it.

How come the implementer keeps the cool tricks to herself?!

Letʼs try to fix this.
How can we give the user access to implicit arguments for their own functions too?

As usual with type theory, it requires being more disciplined and systematic.
Formalizing the unstated conventions underpinning common practice – this is the bread and butter of type theorists!

## Game plan

Our first observation comes from a page stolen out of the builtinsʼ book:

:::Key_Idea
Require that user-defined operators are fully applied enough, in order to infer implicits.
:::

What does this mean, “enough”?
Weʼll need to do some work to figure it out.

But what it accomplishes is clear:

Knowing that we have some arguments applied already lets us _syn_ them, and use the resulting type information to fill in the implicits.

:::{.Note}
I will be using _syn_ and _chk_ as verbs freely.
These are the synthesize and check modes of bidirectional type theory.
:::

Thus we would say that we need to be able to _syn_ enough arguments to determine every implicit the user asked for.
The remaining arguments can (and should) be _chk_​ed – and since normal function arguments are also _chk_​ed, this means they can be unapplied arguments.

:::Note
I wonʼt be addressing how to parse user-defined operators.
Thatʼs a difficult enough problem on its own.

Just note that Iʼll include prefix operators that look like normal function application (separated by a space), but differ in requiring a certain number of arguments to be applied.
In fact, these are the easiest to parse, and the best source of examples.
:::

We can also flip the tables and infer implicits if we start in _chk_ mode in the first place!

In fact, this is strictly better: if we are typing the (partially-applied, or not) binding in _chk_, we know the types of the arguments _and_ the return type.

Thus the main improvement in this case is (wait for it): emulating mixed-mode typing for functions.
(Having some arguments in _syn_, and determining some information from the expected type `T` of the whole partially applied function `f a b : T`.)

Which is why you shouldnʼt even be using bidirectional typing in the first place! Urgh!
(Unification handles this exact problem more elegantly. And it forces you to make your coercions coherent!)

Thus weʼve already gone from “letʼs add implicits to bidirectional typing” to “letʼs emulate unification except worse”.

In particular, one way that it is worse is that we have to commit to modes up-front: we canʼt decide to _try_ infering this argument as _syn_, then fall back to infering something else and returning to _chk_ it, etc.

The other way that it is worse is that we canʼt provide partial types to anything.

So weʼll see what weʼre able to make of it.

### Inferring patterns of _syn/chk_ for arguments

We write `~mode1:I → ~mode0:O`{.agda data-lang=Pseudo} for a non-dependent function `I → O`{.agda data-lang=Pseudo} that, when typechecked in _mode0_, typechecks its argument in _mode1_.
We write the dependent version as `Π ~mode1:(i : I), ~mode0:O(i)`.

Since normal function applications _chk_ their argument, we have this equation for reasoning about modes (the only equation I can think of?):

```{.agda data-lang=Pseudo}
~syn:(A → B)
===============
~chk:A → ~syn:B

~syn:(Π (a : A), B(a))
===============
Π ~chk:(a : A), ~syn:B(a)
```

That is, if we can _syn_ a function type (in result position), it is equivalent to _chk_​ing its arguments and _syn_​ing its result.
Thus the argument does not need to be provided, and so we will prefer the first notation to indicate that.

Itʼs instructive to consider a bunch of examples to get our bearings:

```{.agda data-lang=Pseudo}
id {A : Type} : ~syn:A → ~syn:A
iter {A : Type} : Π ~chk:(n : Nat), ~syn:(A → A) → ~syn:(A → A)

const_Tt {A : Type} : Π ~chk:(B : Type), ~syn:A → ~syn:(B → A)
const_tT {A : Type} : ~syn:A → ~syn:(Π (B : Type), (B → A))
const_t {A B : Type} : ~chk:(A → B → A)
const_tt {A B : Type} : ~syn:A → ~syn:B → ~syn:A

compose {A B C : Type} : ~syn:(B → C) → ~syn:(A → B) → ~syn:(A → C)
compose {P Q R : Poly} : ~syn:(Q ⇒ R) → ~syn:(P ⇒ Q) → ~syn:(P ⇒ R)
compose : ~syn:Poly → ~chk:Poly → ~syn:Poly
```

And we come up with some rules for figuring out the modes of each argument:

- Rule 1: We typecheck strictly from left to right (this could be improved with fancy heuristics but eh).
- Rule 2: If an argumentʼs type mentions none of the remaining implicit parameters, it is in _chk_.
- Rule 3: Based on types we know (if the result is in _chk_ they we know that up front, otherwise we will be _syn_​ing arguments), we fill in the implicits that can be uniquely determined in that argumentʼs type.
  See [picking apart types] for more details.
- Rule 4: We have to know all of the implicits by the end of this process.
  If there are implicits that are not uniquely determined from the types of the explicit arguments, they cannot be implicits.

An important edge case to keep in mind: there will be some times where an argument needs to be synthesized, because we donʼt know all of its implicits, but yet it does not uniquely determine any implicits ([e.g.]{t=} if there is a complex type function based on the implicit).

#### Starting in _syn_

If the type of the whole application is in _syn_, then we have to determine the implicits by synthesizing some arguments, and the rest can be in _chk_.

#### Starting in _chk_

As mentioned above, we have more flexibility in _chk_ mode.

First of all, it should be the case that you can _chk_ the whole type (with nothing applied) to fill in implicits.

### Picking apart types

One word: injectivity.

Injective type constructors uniquely determine their arguments.

#### Dependent types

Eurgh.

Actually I donʼt think itʼs quite so bad.

But still have the obvious issue of data in types.

#### Faking it

For some implicits, they may not be used directly.
[E.g.]{t=} `(a -> b) -> (fin n -> a) -> (fin n -> b)`.
Ugh but that gets weird semantically though.
Parameters.

Sounds like open research.

Or QTT (Quantitative Type Theory), with quantity 0.

### Overloads

Now that weʼre picking apart types, we might as well throw in overloads.

```{.agda data-lang=Pseudo}
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
In particular, we want to outlaw overlapping instances.
(It would be feasible to match in order of most specific to least (toposort the partially-order DAG), but this means that new overloads could change the meaning of code checked under previously-defined overloads.)
Certainly by the end of matching, we want to be left with one unambiguous thing.

In this case, if we are checking a partially-applied `compose`{.agda data-lang=Pseudo} in _syn_ mode, we know that the first argument needs to be _syn_​ed, regardless of which overload.
If it ends up having type `Poly`{.agda data-lang=Pseudo}, we can actually _chk_ the next argument against `Poly`{.agda data-lang=Pseudo} (and synthesize `Poly`{.agda data-lang=Pseudo} for the return type).
If it is one of the other overloads, the next argument needs to be _syn_​ed as well (to cover the missing implicit `A`{.agda data-lang=Pseudo} or `P`{.agda data-lang=Pseudo}),^[This is where mixed-mode would be especially handy lol: we could tell the typechecker we are expecting a function or polynomial morphism respectively (one bit of info), and in particular a function into type `B`{.agda data-lang=Pseudo} or polynomial morphism into poly `Q`{.agda data-lang=Pseudo} respectively (second bit of info).] at which point we know enough to synthesize the return type.

## Implementation



## Philosophy

### Mixed-mode

Where I rant about unification.

Like, consider overloaded numeric literals and operators.
`0 + n`{.agda data-lang=PolyTT} is going to fail without an annotation, but `n + 0`{.agda data-lang=PolyTT} would succeed.
Thus the operator is commutative, but typechecking it isnʼt!
(Yeah, yeah, depending on how you implement `+`{.agda data-lang=PolyTT} thatʼll be a beta-redex, but the point stands.
The same issue occurs with `= {A : Type} : A → A → Type`{.agda data-lang=PolyTT}, for example.)

Itʼs really tempting to do speculative typechecking, and trying to _syn_ an argument but fall back to _syn_​ing another if it is not a _syn_​able argument.
But that just gets to be ridiculous.
