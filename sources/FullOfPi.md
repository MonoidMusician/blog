---
title: Full of Pi
---

Failed Agda sketch:

https://gist.github.com/MonoidMusician/4d57dfbb982d3b2d849f0e730b6648d6

On paper, this is a really tiny theory.
There is no need to formalize a separate substitution relation or deal with metaterms \(e[x]\) marked as dependent on certain variables in context.
The only binder is \(\lambda\), and \(\Pi\) is a bare type constructor on its own, just like the type universes \(U_n\).

## Theory

### Syntax

\[
\begin{gather*}
\begin{align*}
e, T \Coloneqq & \ x \\
  |   & \ U_n \\
  |   & \ \lambda(x : T).\ e \\
  |   & \ \Pi \\
  |   & \ e_1 \cdot e_2 \\
\end{align*}
\qquad
\begin{align*}
\Gamma \Coloneqq & \ \varnothing \\
        |  & \ \Gamma, x:T \\
\end{align*}
\end{gather*}
\]

A very simple theory of expressions: variables, universes, lambdas, gestalt Pi, and function application.

Function application is left-associative as usual (\(f\cdot x\cdot y = (f\cdot x)\cdot y\)) and \(\lambda\) binds all the way to the right ([`BlockArguments`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html) style).

If you want to be extra formal, you could formalize de Bruijn indices instead of variable matching.
But then you need to handle contexts and shifting like Dhall does.

\[
\begin{align*}
x,y,z \Coloneqq 0\ |\ x^\prime
\end{align*}
\]

### Reductions

Hmm it seems like it may be best to formalize three relations: Weak-Head Normal Form ([WHNF]{t=}), full reduction (big-step semantics), and definitional equality.

As a preview: typing a function application requires knowing that the function is a function type ([WHNF]{t=}s to \(\Pi\cdot \_\cdot \_\)) and that the input is compatible (congruence, by any means necessary/available).

The feasibility of the theory rests on that one rule and what, exactly, its premises are.
Is the theory circular, is it tractable, or is it impossible to prove anything nice?

Weak-Head Normal Form ensures that we can be maximally lazy about reduction, which translates to maximally parsimonious about assumptions about typing and so on.

Big step semantics will be nice only for theorems later: it is not directly relevant to typing.

We can call the relations \(\whnfto\) ([WHNF]{t=} reduction), \(\mapsto\) (big-step reduction), and \(\equiv\) (definitional equality), and generically (any of them), \(\rArr\).

### Congruences

\[
\begin{gather*}
\frac{}{U_n \rArr U_n}U{\rArr} \\
\frac{\qquad}{x \rArr x}x{\rArr} \\
\frac{}{\Pi \rArr \Pi}\Pi{\rArr} \\
\\
\frac{}{\lambda(x : T_1).\ e_1 \whnfto \lambda(x : T_2).\ e_2}\lambda{\whnfto} \\
\\
\frac{T_1 \mapsto T_2 \qquad e_1 \mapsto e_2}{\lambda(x : T_1).\ e_1 \mapsto \lambda(x : T_2).\ e_2}\lambda{\mapsto} \\
\\
\frac{T_1 \equiv T_2 \qquad e_1 \equiv e_2}{\lambda(x : T_1).\ e_1 \equiv \lambda(x : T_2).\ e_2}\lambda{\equiv} \\
\end{gather*}
\]

The base constructors all reduce normally, but the WHNF relation stops at lambdas where big-step reduction and congruence continue recursing.

#### Substitution/beta-reduction

You probably want to formalize substitution as its own thing, but this minimalism was kind of cute. Just read \((\lambda (x : T).\ e_1)\cdot e_2\) as \(e_1[x \coloneqq e_2]\).

##### Constant and identity substitutions

\[
\begin{gather*}
\frac{}{(\lambda(x : T).\ U_n)\cdot e \rArr U_n}U\beta \\
\frac{}{(\lambda(x : T).\ \Pi)\cdot e \rArr \Pi}\Pi\beta \\
\frac{}{(\lambda(x : T).\ y)\cdot e_1 \rArr y}x{\ne}\beta \\
\\
\frac{e_1 \rArr e_2}{(\lambda(x : T).\ x)\cdot e_1 \rArr e_2}x{=}\beta \\
\end{gather*}
\]

That is, for each relation \(\whnfto\), \(\mapsto\), and \(\equiv\), the identity substitution recurses on the same relation.

##### Lambda-in-lambda substitution

If the bound variables match, they shadow each other, so it is no longer a substitution, just a reduction (if not looking for [WHNF]{t=}).

\[
\begin{gather*}
\frac{\lambda(x : T_2).\ e_1 \mapsto e_3}{(\lambda(x : T_1).\ \lambda(x : T_2).\ e_1)\cdot e_2 \mapsto e_3}\lambda{=}\beta \\\\
\frac{}{(\lambda(x : T_1).\ \lambda(x : T_2).\ e_1)\cdot e_2 \whnfto \lambda(x : T_2).\ e_1}\lambda{=}\beta \\\\
\end{gather*}
\]

If they are distinct, then the substitution needs to apply both to the bound type \(T_2\) and the inner expression \(e_1\).
For big step semantics, this involves recursive applications of \(\mapsto\), but for [WHNF]{t=}, it just embeds the substitution nodes in the result.

\[
\begin{gather*}
\frac{(\lambda(x : T_1).\ e_1)\cdot e_2 \mapsto e_3 \qquad (\lambda(x : T_1).\ T_2)\cdot e_2 \mapsto T_3 \qquad \lambda(y : T_3).\ e_3 \mapsto e_4}{(\lambda(x : T_1).\ \lambda(y : T_2).\ e_1)\cdot e_2 \mapsto e_4}\lambda{\ne}\beta \\\\
\frac{}{(\lambda(x : T_1).\ \lambda(y : T_2).\ e_1)\cdot e_2 \whnfto \lambda(y : (\lambda(x : T_1).\ T_2)\cdot e_2).\ (\lambda(x : T_1).\ e_1)\cdot e_2}\lambda{\ne}\beta \\\\
\end{gather*}
\]

##### Inner application substitution

Push the substitution through both sides of the inner application, then let it further reduce.
For [WHNF]{t=}, suspend the substitution on the right.

\[
\begin{gather*}
\frac{(\lambda(x : T).\ e_1)\cdot e_3 \mapsto e_4 \qquad (\lambda(x : T).\ e_2)\cdot e_3 \mapsto e_5 \qquad e_4\cdot e_5 \mapsto e_6}{(\lambda(x : T).\ e_1 \cdot e_2)\cdot e_3 \mapsto e_6}{\cdot}\beta \\
\\
\frac{(\lambda(x : T).\ e_1)\cdot e_3 \whnfto e_4 \qquad e_4\cdot ((\lambda(x : T).\ e_2)\cdot e_3) \whnfto e_6}{(\lambda(x : T).\ e_1 \cdot e_2)\cdot e_3 \whnfto e_6}{\cdot}\beta \\
\end{gather*}
\]

### Definitional equality

Based on the reduction rules, there is a definitional equality \(e_1 \equiv e_2\) which is reflexive, symmetric, and transitive, with \(e_1 \whnfto e_2\) or \(e_1 \mapsto e_2\) both implying \(e_1 \equiv e_2\).

To obtain the rules for definitional equality, replace \(\mapsto\) in each judgment above with \(\equiv\) and add the rules \[\frac{}{e \equiv e}\operatorname{refl} \qquad \frac{e_1 \equiv e_2}{e_2 \equiv e_1}\operatorname{symm}.\]

### Typing

Note that this is a strange typing relation: it generates unreduced “types”!
It requires lots of theorems to show it behaves well, but I believe it is possible?

\[
\begin{gather*}
\frac{}{\Gamma \vdash U_n : U_{n+1}}U{:}\\
\frac{}{\Gamma, x : T, \Gamma^\prime \vdash x : T}x{:}\\
\\
\frac{\Gamma, x : I \vdash e : O}{\Gamma \vdash (\lambda(x : I).\ e) : \Pi\cdot I\cdot \lambda(x : I).\ O}\lambda{:}\\\\
\frac{\Gamma \vdash e_1 : F \qquad \Gamma \vdash e_2 : I_2 \qquad F \whnfto \Pi\cdot I_1\cdot O \qquad I_1 \equiv I_2}{\Gamma \vdash e_1 \cdot e_2 : O\cdot e_2}{\cdot}{:}
\end{gather*}
\]

\[
\frac{}{
    \begin{align*}
    \Gamma \vdash \Pi : \Pi\cdot U_n\cdot \lambda(I : U_n).\ \Pi & \cdot(&& \Pi\cdot I\cdot \lambda(x : I).\ U_m) \\ &\cdot \lambda(f : && \Pi\cdot I \cdot \lambda(x : I).\ U_m).\ U_{n \lor m}
    \end{align*}
}\Pi{:}
\]

The type of a lambda is Pi applied to both the domain and the lambda expressing the codomain type family (obtained by typing the body \(e\) in the context of the argument \(x : I\).).
Note that, _a priori_, we have no idea that this is well typed at all!

In the application rule, Pi no longer has a binder: we just apply the output type family \(O\) to the input \(e_2\) directly.
Again, we do not know _a priori_ if this is well typed or even a universe, that has to be derived.

Finally Pi is a gestalt term and has its own type.
That type necessarily is defined in terms of Pi.
But I do not believe this is _actually_ circular, it is just annoyingly tricky to show that it would work out.

:::Note
Note that the only rule that matches on a type is the application rule \({\cdot}{:}\) -- nothing else imposes constraints on the shape of types.
It is therefore the only typing rule that requires definitional equality: we let it infer an unreduced type \(P\) and then check that it is equivalent to a dependent function type \(\Pi\cdot T\cdot R\) via the .
:::


## Helpers

### Sugar for Pi

Introduce syntactic sugar for \(\Pi\) as a binder, so we do not have to repeat the input type \(I\).

\[
\Pi(x : I).\ O \Deltaeq \Pi\cdot I\cdot \lambda(x : I).\ O,
\]

so \(\Pi\) now has the type \[\ddot{\Pi} \Deltaeq \Pi(I : U_n).\ \Pi(f : \Pi(x : I).\ U_m).\ U_{n \lor m}\]

which we will further abbreviate as \(\ddot{\Pi}\), with dots to remind us that it is a type: \(\Pi : \ddot{\Pi}\).

### Sugar for non-dependent functions

\[I \to O \Deltaeq \Pi(\_ : I).\ O\]

I guess you would want to formalize binders as “either a name, or an underscore to ignore it”

### Sugar for type reduction

\[
\frac{\Gamma \vdash e : T_1 \qquad T_1 \whnfto T_2}{\Gamma \vdash e : T_1 \whnfto T_2}
\]

(this should be a double bar)

### Sugar for typed term reduction

\[
\frac{\Gamma \vdash e_1 : T_1 \qquad T_1 \equiv T \qquad e_1 \rArr e_2 \qquad \Gamma \vdash e_2 : T_2 \qquad T_2 \equiv T}{\Gamma \vdash e_1 \rArr e_2 : T}
\]

(again, double bar: a judgment defined by only one rule, so it is equivalent to its premises)

## Derivations

[TODO]{t=}

### Type of pi is in normal form



### Pi once ~~removed~~ applied

\(\Pi\) obviously has a Pi type with domain \(U_n\), so we know we can apply it to some \(T : U_n\),

\[
\frac
{\Gamma \vdash T : U_n}
{\Gamma \vdash \Pi\cdot T : (\lambda (A : U_n).\ \Pi(f : \Pi(a : A).\ U_m).\ U_{n \lor m})\cdot T}
\]

which reduces to

\[\Pi_T \Deltaeq \Pi(f : \Pi(a : T).\ U_m).\ U_{n \lor m}\] (a plain substitution \([A := T]\)), which we will call \(\Pi_T\) for the sake of brevity.

### Pi twice applied

\[
\frac
{\Gamma \vdash T : U_n \qquad \Gamma \vdash R : P \qquad P \equiv \Pi\cdot T\cdot U_m}
{\Gamma \vdash \Pi\cdot T\cdot R : (\lambda(f : \Pi(a : T).\ U_m).\ U_{n \lor m})\cdot R}
\]

which of course reduces to \(U_{n \lor m}\).

## Theorems

Type of things you would want to prove to show that this system makes sense.

[TODO]{t=}

### Normal forms

A term \(t\) is in Weak-Head Normal Form if \(t \whnfto t\) (it WHNF reduces to itself) and is in Normal Form if \(t \mapsto t\) (it big-step reduces to itself).

Theorem: all terms in NF are in WHNF; if \(t \mapsto t\) then \(t \whnfto t\).

### Type of type

If \(\Gamma \vdash e : T\), then \(\Gamma \vdash T : U_n\) for some \(n\).

### Type preservation

If \(\Gamma \vdash e_1 : T_1\), \(T_1 \mapsto T\), and \(e_1 \mapsto e_2\), then \(\Gamma \vdash e_2 : T_2\) such that \(T_2 \mapsto T\).

In particular, if \(\Gamma \vdash T_1 : U_n\) and \(T_1 \mapsto T_2\), then \(\Gamma \vdash T_2 : U_n\).
