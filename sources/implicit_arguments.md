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

### Inferring patterns of _syn/chk_

### Picking apart types

### Overloads
