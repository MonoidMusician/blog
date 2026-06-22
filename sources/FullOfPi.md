---
title: Full of Pi
---

Failed Agda sketch:

https://gist.github.com/MonoidMusician/4d57dfbb982d3b2d849f0e730b6648d6

## Theory

### Syntax

\[
\begin{gather*}
\begin{align*}
e \Coloneqq & \ x \\
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

### Reductions

\[
\begin{gather*}
\frac{}{U_n \mapsto U_n}U{\mapsto} \\
\frac{\qquad}{x \mapsto x}x{\mapsto} \\
\frac{}{\Pi \mapsto \Pi}\Pi{\mapsto} \\
\\
\frac{T_1 \mapsto T_2 \qquad e_1 \mapsto e_2}{\lambda(x : T_1).\ e_1 \mapsto \lambda(x : T_2).\ e_2}\lambda{\mapsto}
\end{gather*}
\]

The constructors all reduce normally.

#### Substitution/beta-reduction (unscoped)

(You probably want to formalize substitution as its own thing, but this minimalism was kind of cute. Just read \((\lambda (x : T).\ e_1)\cdot e_2\) as \(e_1[x \coloneqq e_2]\).)

\[
\begin{gather*}
\frac{}{(\lambda(x : T).\ U_n)\cdot e \mapsto U_n}U\beta \\
\frac{}{(\lambda(x : T).\ \Pi)\cdot e \mapsto \Pi}\Pi\beta \\
\\
\frac{e_1 \mapsto e_2}{(\lambda(x : T).\ x)\cdot e_1 \mapsto e_2}x{=}\beta \\
\frac{}{(\lambda(x : T).\ y)\cdot e_1 \mapsto y}x{\ne}\beta \\\\
\frac{\lambda(x : T_2).\ e_1 \mapsto e_3}{(\lambda(x : T_1).\ \lambda(x : T_2).\ e_1)\cdot e_2 \mapsto e_3}\lambda{=}\beta \\\\
\frac{(\lambda(x : T_1).\ e_1)\cdot e_2 \mapsto e_3 \qquad \lambda(y : T_2).\ e_3 \mapsto e_4}{(\lambda(x : T_1).\ \lambda(y : T_2).\ e_1)\cdot e_2 \mapsto e_4}\lambda{\ne}\beta \\\\
\end{gather*}
\]
\[
\frac{(\lambda(x : T).\ e_1)\cdot e_3 \mapsto e_4 \qquad (\lambda(x : T).\ e_2)\cdot e_3 \mapsto e_5 \qquad e_4\cdot e_5 \mapsto e_6}{(\lambda(x : T).\ e_1 \cdot e_2)\cdot e_3 \mapsto e_6}{\cdot}\beta
\]

Subsitution--reduction splits into five interesting cases:

- The identity substitution, when we have focused to the targeted variable, and the constant substitution, when it is a different variable.
- Lambda shadowing, which is no longer a substitution, just a reduction.
- The sketchy lambda-in-lambda rule (no shadowing), which substitutes the outer variable in the body, before returning it to the lambda to potentially reduce.
- The application-in-lambda rule, which substitutes on both sides of the application, and then reduces the application itself.

### Typing

\[
\begin{gather*}
\frac{}{\Gamma \vdash U_n : U_{n+1}}U{:}\\
\frac{}{\Gamma, x : T, \Gamma^\prime \vdash x : T}x{:}\\
\\
\frac{\Gamma, x : A \vdash e : B}{\Gamma \vdash (\lambda(x : A).\ e) : \Pi\cdot A\cdot \lambda(x : A).\ B}\lambda{:}\\\\
\frac{\Gamma \vdash e_1 : \Pi\cdot T\cdot R \qquad \Gamma \vdash e_2 : T}{\Gamma \vdash e_1 \cdot e_2 : R\cdot e_2}{\cdot}{:}
\end{gather*}
\]

\[
\frac{}{
    \begin{align*}
    \Gamma \vdash \Pi : \Pi\cdot U\cdot \lambda(A : U_n).\ \Pi & \cdot(&& \Pi\cdot A\cdot \lambda(a : A).\ U_m) \\ &\cdot \lambda(f : && \Pi\cdot A \cdot \lambda(a : A).\ U_m).\ U_{n \lor m}
    \end{align*}
}\Pi{:}
\]

The type of a lambda is Pi applied to the domain and the lambda expressing the codomain dependent on the input.
Note that, a priori, we have no idea that this is well typed at all!

In the application rule, Pi no longer has a binder: we just apply the result type to the input directly.
Again, we do not know a priori if this is well typed or even a universe, that has to be derived.

Finally Pi is a gestalt term and has its own type.
That type necessarily is defined in terms of itself.
But I do not believe this is _actually_ circular, it is just annoyingly tricky to show that it would work out.


## Derivations

[TODO]{t=}

## Theorems

Type of things you would want to prove to show that this system makes sense.

[TODO]{t=}

### Type of type

If \(\Gamma \vdash e : T\), then \(\Gamma \vdash T : U_n\) for some \(n\).

### Type preservation

If \(\Gamma \vdash e_1 : T_1\), \(T_1 \mapsto T\), and \(e_1 \mapsto e_2\), then \(\Gamma \vdash e_2 : T_2\) such that \(T_2 \mapsto T\).

In particular, if \(\Gamma \vdash T_1 : U_n\) and \(T_1 \mapsto T_2\), then \(\Gamma \vdash T_2 : U_n\).
