---
title: (Extra)Natural Transformations
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
date: 2026
---

<details class="Details">
<summary>Background / Conventions</summary>

A category \(\mathcal C\) consists of a collection of objects, \(X, Y, Z, \ldots : \mathcal C\), and a collection of morphisms indexed by those objects: for \(X, Y : \mathcal C\), the collection \(f, g, h, \ldots : X \to Y\) of morphisms^[or arrows or maps or yet other terms] from \(X\) to \(Y\).
It comes with a notion of associative composition \(f \then g\) when the target of \(f\) is the same as the source of \(g\): \(f : X \to Y\) and \(g : Y \to Z\).^[This is often written in the other direction, as \(g \circ f\), but I prefer forwards composition: it is easier to think about with the direction morphisms are written.]
And it comes with identities, \(id_X : X \to X\).^[Sometimes this morphism is notated as just \(X\).]

A functor \(F : \mathcal C \to \mathcal D\) is the notion of structure-preserving morphism between the categories themselves.
(An endofunctor is a functor with the same source and target categories.)

Specifically, \(F\) will map objects of \(\mathcal C\) to objects of \(\mathcal D\), written \(F(X) : \mathcal D\), and map morphisms in a compatible manner, written \(F(f) : F(X) \to F(Y)\) for \(f : X \to Y\).^[Yes the same notation \(F({-})\) is used for this, but it does not cause issues since morphisms and objects are distinct syntactic categories, essentially. Even when \(id_X\) is abbreviated as \(X\): the functor laws make it coherent no matter if \(F(X)\) is interpreted as \(id_F(X)\) or \(F(id_X)\).]
It needs to preserve composition and identities from \(\mathcal C\) to \(\mathcal D\): \(F(f \then g) = F(f) \then F(g)\) and \(F(id_X) = id_{F(X)}\).

</details>

A natural transformation is the natural notion of morphism between functors, \(m : F \to G\), where \(F, G : \mathcal C \to \mathcal D\) are parallel functors between any two categories.

This takes some explaining, especially because \(F\) and \(G\) are not objects in the base categories \(\mathcal C\) or \(\mathcal D\) themselves, but functors that map objects to objects and morphisms to morphisms between these categories.
Only once they are applied to a parameter \(X : \mathcal C\), they yield an object in \(\mathcal D\).

So a natural transformation has components, which explains how to apply the transformation to actual objects in \(\mathcal D\) to get an actual morphism in \(\mathcal D\).
And those components need to satisfy a property (the “naturality square”) that says how they work together.

How it is conventionally taught is that a natural transformation has components indexed by objects \[m_X : F(X) \to G(X),\] and satisfies a naturality square for any \(f : X \to Y\) in the source category \(\mathcal C\) \[m_X \then G(f) = F(f) \then m_Y : F(X) \to G(Y)\] saying you can apply the natural transformation \(m_X : F(X) \to G(X)\) first and then map \(G(f) : G(X) \to G(Y)\), or you can map first with \(F(f) : F(X) \to F(Y)\) and then use the transformation (at object \(Y\) this time) \(m_Y : F(Y) \to G(Y)\).

Formally, Iʼve always found this a weird thing to ask for: why ask for morphisms indexed by objects?
Every other construction in category theory maps objects to objects, morphisms to morphisms, categories to categories, functors to functors … why are natural transformations the odd one out?
To be fair, the point of the naturality square is that the choice of object does not matter.

Still, I think an alternate exposition clarifies this and has some nice points in its favor.

:::Key_Idea
Letʼs index the components of the natural transformation by *morphisms* instead of objects: \[m_f : F(X) \to G(Y)\text{, for\ } f : X \to Y.\]
:::

This component incorporates the mapping \(f : X \to Y\) into its mapping of the functors \(F \to G\), producing the composite \(m_f : F(X) \to G(Y)\).
The natural transformation now maps morphisms to morphisms, which makes a lot of sense.

Accordingly, the naturality square now says that any mapping of \(F\) _or_ \(G\) can be incorporated into the component itself: \[F(f) \then m_g \then G(h) = m_{f \then g \then h}.\]

Interestingly, with the convention that the identity morphisms \(id_X\) can be abbreviated by its object, as just \(X\), then \(m_X\) continues to mean the same thing!

And speaking in terms of programming, while `forall x. f x -> g x`{.haskell} is a convenient type signature to write, the fused implementation `forall x y. (x -> y) -> f x -> g y`{.haskell} is probably more efficient: if the function is deconstructing the structure of `f`{.haskell} to build the structure of `g`{.haskell}, it already has access to the elements `x`{.haskell} to map to `y`{.haskell}.
(Of course, it could just embed the structure of `f`{.haskell} into `g`{.haskell}, in which case the `fmap`{.haskell} is still necessary.)


## Extranatural Transformations

The next step of generality.
(There are also (strong) dinatural transformations.)

Extranaturality can apply when a category and its opposite appear in the same functor, either on the input or on the output functor, but not both.

So to take an example demonstrating naturality along with extranaturality on both the input and output sides: let \(F : A \x B \x B^{op} \to D\) and \(G : A \x C \x C^{op} \to D\).^[Note: with extranaturality, the functors do not need to have the same number of parameters.]

The transformation on arrows falls out like this.

\[
\begin{alignat*}{4}
  & u &&: a_1\;&\to\;&a_2&&: \mathcal A\\
  & v &&: b_1\;&\to\;&b_2&&: \mathcal B\\
  & w &&: c_1\;&\to\;&c_2&&: \mathcal C\\
\end{alignat*}\\
\begin{alignat*}{1}
  \hline
  & n_{u,v,w} : F(a_1, b_1, b_2) \to G(a_2, c_2, c_1)
\end{alignat*}
\]

You can see how it takes an arrow for each category.

I think I will just state the same naturality condition for this setup; it is not so complicated.

Take three composable triples: \(u\) in \(\mathcal A\), \(v\) in \(\mathcal B\), and \(w\) in \(\mathcal C\).

\[
\begin{alignat*}{5}
  & a_0 &\overset{u_0}{\longrightarrow}\ & a_1 &\overset{u_1}{\longrightarrow}\ & a_2 &\overset{u_2}{\longrightarrow}\ & a_3 \ &:\ &\mathcal A \\
  & b_0 &\overset{v_0}{\longrightarrow}\ & b_1 &\overset{v_1}{\longrightarrow}\ & b_2 &\overset{v_2}{\longrightarrow}\ & b_3 \ &:\ &\mathcal B \\
  & c_0 &\overset{w_0}{\longrightarrow}\ & c_1 &\overset{w_1}{\longrightarrow}\ & c_2 &\overset{w_2}{\longrightarrow}\ & c_3 \ &:\ &\mathcal C \\
\end{alignat*}
\]

Then mapping over \(F\) and \(G\) can be incorporated into the extranatural transformation.

\[
\begin{align*}
  F(u_0, v_0, v_2) \then n_{u_1,v_1,w_1} \then G(u_2, w_0, w_2) = n_{(u_0 \then u_1 \then u_2),(v_0 \then v_1 \then v_2),(w_0 \then w_1 \then w_2)} : F(a_0, b_0, b_3) \to G(a_3, c_3, c_0)
\end{align*}
\]


## Relational Parametricity

Relational Parametricity is a really important notion.
It is generally thought in the context of a type theory: proving things about how the structure of types is reflected in the behavior of terms, from the vantage point of a metatheory.

Naturality is a less-general version that is phrased in terms of category theory, where everything is a morphism and relations besides equality of morphisms are not available.

Both concepts capture the idea that a mapping operates uniformly over some index, such as the objects \(X\) in the components of the natural transformation \(\forall X. F(X) \to G(X)\).

In fact, relational parametricity is powerful enough to imply parametricity: this is why every safe function `forall x. f x -> g x`{.haskell} in Haskell (or another parametrically polymorphic language) _is_ a natural transformation, with no other conditions needed.^[Assuming that the functors in question are also parametrically polymorphic.]

Relational parametricity was indeed originally studied in the context of System F (the basis of Haskellʼs type system).
Unfortunately, relational parametricity has been slower to be systematically applied to dependent type theories, despite the fact that they love to be relationally parametric too.^[Certain axioms, like the Law of Excluded Middle, will break parametricity and computation; but generally the base theory, like [MLTT]{t=}, is parametrically polymorphic.]

Anyways, enough waffling about it.
Letʼs get into the details.

The core idea of relational parametricity is to intepret types as relations.
Quantification over a type becomes quantification over a relation, which allows the user to prove something about how that type is used within that quantification – if, indeeed, it is used parametrically.

Relational parametricity can show that every term of type \(\forall X.\ X \to X\) is equivalent to the identity function.
But the first nontrivial example would be the term of type \(\forall X.\ X \to X \to X\), a polymorphic function with two arguments.

Intuitively, a function with this type must choose one of its arguments to return.
It doesnʼt have special knowledge of the type \(X\) with which to invent some other \(X\) to return!
Furthermore, its choice must be uniform: it must make the same choice no matter what type it is instantiated at.
This is what the relational part of relational parametricity enables, talking about the functionʼs behavior at different instantiations, otherwise you could just use subsets.

So letʼs see how relational parametricity formalizes this argument!

Relational parametricity maps the type \(X\), as a token, to a relation; in fact, a relation over whatever types one wants.
Thus let \(X^L, X^R\) be those types and \(\mathbb X : X^L \x X^R \to \mathrm{Prop}\) be the relation between them in the metatheory.^[In set theory, you would say \(\mathbb X\) is a subset of the set \(X^L \x X^R\), picking out the terms that are related. That is, \(\mathbb X \in \mathscr{P}(X^L \x X^R)\).]

\[\mathbb X(x^L, x^R)\]

The universal quantifier \(\forall\) (“forall”) is mapped to quantification over the arbitrary types and the arbitrary relation between them: \(\forall X\) in the type theory becomes quantification \(\forall (X^L, X^R, \mathbb X)\) in the metatheory.
The usage of a type \(X\) is mapped to the relation that represents it, and the function arrow \(\to\) is mapped to implication: if the relation holds of the input, then it holds for the output.

Thus the application of relational parametricity to \(\forall X.\ X \to X \to X\) gives a relation that relates two functions \(f^L, f^R\) of that type when the following proposition is satisfied:
\[
\begin{align*}
  & \forall (X^L, X^R : \mathrm{Type}).\ \forall (\mathbb X : X^L \x X^R \to \mathrm{Prop}).\ \\
  & \forall (x_0 : X^L \x X^R).\ \mathbb x_0^L \mathrel{\large \mathbb X} x_0^R \implies \\
  & \forall (x_1 : X^L \x X^R).\ \mathbb x_1^L \mathrel{\large \mathbb X} x_1^R \implies \\
  &\quad f^L_{X^L}(x_0^L)(x_1^L) \mathrel{\large \mathbb X} f^R_{X^R}(x_0^R)(x_1^R)
\end{align*}
\]

#.  For any types \(X^L\) and \(X^R\) and any relation \(\mathbb X\) between them,
#.  For any pair of inputs \(x_0 : X^L \x X^R\), if they are related by \(\mathbb X\),
#.  For any pair of inputs \(x_1 : X^L \x X^R\), if they are also related by \(\mathbb X\),
#.  Then the outputs of \(f^L\) and \(f^R\), instantiated to the respective types \(X^L\) and \(X^R\) and applied at the respective left and right inputs, are also related by \(\mathbb X\).

Proving that a certain \(f\) is parametric amounts to showing that this relation holds for \(f^L\) and \(f^R\) both being \(f\).
(Proving that a whole type theory is parametric is saying that this relation is reflexive.)

Using sets instead of relations, it would look much simpler:
\[
\begin{align*}
  & \forall (X : \mathrm{Type}).\ \forall (\mathbb X : X \to \mathrm{Prop}).\ \\
  & \forall (x_0 : X).\ x_0 \in {\large \mathbb X} \implies \\
  & \forall (x_1 : X).\ x_1 \in {\large \mathbb X} \implies \\
  &\quad f_{X}(x_0)(x_1) \in {\large \mathbb X}
\end{align*}
\]

This is sufficient for proving that the output must come from one of the inputs, but does not relate the function behavior across instantiations.
That is the power reserved for relational approaches.

<!-- ʼ -->

