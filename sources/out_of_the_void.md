---
title: Out of the Void
subtitle: Eta for Absurd Functions
toc: false
---

http://strictlypositive.org/Ripley.pdf

```pudding
@data Void.

@data (=) @{T : Type} : Π (t0 & t1 : T). Type
| refl @{T : Type} @{t : T} : t = t

absurd : Π @{O : Type}. Void -> O
absurd @{O} ().

allAbsurd : Π @{O : Type} (f1 & f2 : Void -> O). f1 = f2
allAbsurd @{O} f1 f2 := refl @{t := absurd @{O}}

@data Bool
| tt : Bool
| ff : Bool

always_tt := λ @{I : Type} (i : I). tt
always_ff := λ @{I : Type} (i : I). ff

allAbsurd @{O := Bool} (always_tt @{I := Void}) (always_ff @{I := Void})

@assume lie : Void

always_tt @{I := Void} lie = tt#[lie : Void] = ff#[lie : Void] = always_ff @{I := Void} lie
```

A new term former `t#[p : T]` that affixes to the base term `t` some expressions `p` that may be (spurious) proofs of `Void`.

The computation rule for lambdas sticks it onto the result obtained from substitution:

```
body[x := v] = result
----------------------------------------
(λ (x : T). body[x])(v) = result#[v : T]
```

Then we can detect when the context has collapsed, applying the previously-elusive congruence through absurd functions:

```
T = Void
---------------
t1#[p : T] = t2
```

This means that the term does not matter, `t1#[p : Void]` might as well be written `_#[p : Void]` or `absurd p` or so on.

Or we can detect if we need to no longer worry about it,

```
T apart-from Void
-----------------
t1#[p : T] = t1
```

This gets us back to canonicity: every `Bool` in a closed context reduces to either `tt` or `ff`.

Now I think we need one more detail: possibly-void proofs are not only sticky but also infectious.
So the congruence rule gets that behavior.

```
t1 = t2
---------------------------
t1#[p1 : T1] = t2#[p2 : T2]  ##[p1 : T1, p2 : T2]
```

So I think it ends up being something like a co-context, stratifying the infectious affixes by their level of free variables.
Stuff stuck on bound variables will get deleted in a WHNF lambda abstraction.

## Formal presentation?

Maybe it wants to be a program, and not horizontal bar rules ...

\[
  \Gamma \vdash t : T \Leftarrow \Upsilon
\]

\[
  \Upsilon_1 \bigcup \Upsilon_2
\]

\[
  \Upsilon_1 \bigcup (\Upsilon_2 \cup p_2) = (\Upsilon_1 \bigcup \Upsilon_2) \cup p_2
\]

\[
  (\Upsilon \cup p) \smallsetminus x = (\Upsilon \smallsetminus x) \text{\ if\ } x \in p \text{\ else\ } (\Upsilon \smallsetminus x) \cup p
\]

Delete any affixes that depend on \(x\).

\[
\begin{gather*}
  \Gamma, x : T \vdash b : R \Leftarrow \Upsilon
\\\hline
  \Gamma \vdash (\lambda (x : T).\ b) : (\Pi (x : T).\ R) \Leftarrow \Upsilon \smallsetminus x
\end{gather*}
\]

Remember each place that one _could_ apply absurd congruence.

\[
\begin{gather*}
  \Gamma \vdash f : \Pi (x : T).\ R \Leftarrow \Upsilon_1
  \qquad
  \Gamma \vdash v : T \Leftarrow \Upsilon_2
\\\hline
  \Gamma \vdash f(v) : R[x \coloneqq v] \Leftarrow (\Upsilon_1 \bigcup \Upsilon_2) \cup v : T
\end{gather*}
\]

In an absurd context, everything is equal

\[
\begin{gather*}
\Gamma \vdash T \equiv 0
\\\hline
\Gamma \vdash x \equiv y \Leftarrow \Upsilon_1 \cup p : T \cup \Upsilon_2
\end{gather*}
\]

And for efficiency, you can delete any entries \(p : T\) where \(T\) is apart from \(0\).

## So?

Does this solve the issue?

Well, from the first presentation (with the tags), sort of:

There is no missing path between `tt = ff` in a context with `lie : Void` because there is only a path between `tt#[lie : Void] = ff#[lie : Void]`, and `tt#[lie : Void]` does not reduce to `tt`.

(Does this require directed equations? maybe.)

In the second presentation (with the context of constraints), even more sort of?:

This is no missing path between `tt = ff` because those are not equatable under empty constraints.
They are only equatable under a context that generates a constraint `##[lie : Void]`.

Now, maybe you argue I moved the goalposts.
That might be true.

But it is an algorithm that explains why `always_tt lie = always_ff lie` holds.
