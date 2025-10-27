Please contribute terms that you want to be explained or want to explain!

_Note: I provide references to resources such as nLab for concreteness, but that should not be your first stop for learning about concepts, since it is a wiki written more as concise reference notes for experts than introductory materials. Nevertheless it is helpful to browse. The 1Lab is written in a more accessible style, but still, there is only so much that individual articles can teach without spinning into a web of all the related concepts._

## Type formers / semantics of types

Pi types and sigma types
:   The two main dependent type formers.
    Pi types are also called: dependent functions, dependent products, and the forall quantifier.
    Sigma types are also called: dependent pairs, dependent sums, and the _proof-relevant_ exists quantifier.
    (_Logical_ existence is the propositional truncation of sigma, where the particular witness (the first projection) of the property (the second projection) is quotiented out, so that all *mere* proofs of existence are equal.)

Mere proposition
:   A proposition whose proofs are all equal.
    See the HoTT book for more (I believe that is where it was introduced) (recommended reading for sure).

    Note that mere propositions cannot be fully erased: they still can contain computational content, and not just for runtime complexity, but also for termination, and potentially other stuff?

    [nLab](https://ncatlab.org/nlab/show/mere+proposition), [1Lab](https://1lab.dev/1Lab.HLevel.html)

Propositional truncation
:   Forms a new type, a proposition, where all inhabitants of the original type are considered equal proofs of its propositional truncation.
    A quotient by the totally connected relation (also called a codiscrete relation).

    [1Lab](https://1lab.dev/1Lab.Truncation.html), [nLab](https://ncatlab.org/nlab/show/propositional+truncation)

Subtypes (uwu)
:   This is a sigma type where the second projection is a proposition.
    The picture is that it is like a subset in set theory, but the witness of the property of belonging-to-the-subset has to be explicit, even if it is unique up to equality.

    [1Lab](https://1lab.dev/1Lab.Type.Sigma.html#paths-in-subtypes), [nLab](https://ncatlab.org/nlab/show/subtype)

Quotient types
:   Quotient types are the dual of subtypes: we speak of types quotiented by a (reflexive, symmetric, transitive) *relation*.
    (Other fields like group theory use specialized quotients, often by algebraic structures, where it does act like integer division actually.)
    This relation *identifies* elements of the quotient: inhabitants of the original type map to inhabitants of the quotient type, but they can only be told apart if the relation between them is false.

    Quotient types require more work to integrate into a type theory: they cannot really be built out of existing primitives of MLTT (without size issues).
    The type theory of Lean has some wonkiness due to quotients operating over definitional propositions: this leads to failures of the transitivity of definitional equality, for example.

    [nLab](https://ncatlab.org/nlab/show/quotient+type)

Type universes
:   Denoted by \(U\) or `Type` (or `Set`, unfortunately), these are the “type of types”: they allow mentioning types as first-level concepts in a dependent type theory, enabling quantification (among other things).
    For example, `Nat` (the type of natural numbers) is a type because `Nat : Type` is a valid typing judgment.

    The axiom `Type : Type` makes the theory inconsistent, so universes are typically put into a countable hierarchy: `Nat : Type 0 : Type 1 : Type 2 : ...`, but [many other shapes of stratification are possible](https://favonia.org/files/mugen.pdf).
    This is where the notion of “size” comes from: types in `Type 0` are called “small”, and everything else is called “large” (lol). (Thanks set theorists.) (But for real, the notion is a lot clearer in type theory.)
    Below `Type 0` there is sometimes an impredicative universe `Prop` of propositions.

    This is not the only way that type universes may vary, though.
    Other type universes may serve to contain special types, like the interval type `I` in cubical Agda is contained in its own universe [`IUniv : SSet₁`](https://1lab.dev/Prim.Interval.html).
    There may be several parallel hierarchies, like with 2LTT: the universe hierarchy `U0,m` of the “base” type theory, and the universe hiearchy `U1,n` of the “meta” type theory.

    [nLab](https://ncatlab.org/nlab/show/type+universe)

## Flavours of type theory

MLTT
:   Martin-Löf Type Theory, a formulation of dependent type theory used as basis for most other intrinsically-typed theories.
    It offers type universes, equality types, pi types, sigma types, basic types like 0 (void), 1 (unit), and 2 (bool), and natural numbers and/or W-types depending on whom you ask, but it notably does not include quotients or general inductive types.
    MLTT is relatively ambivalent to the semantics of its equality type: one can either create Homotopy Type Theory from it, by adding univalence to force the equality type to represent isomorphisms, or more classical theories where all proofs of equality are unique (thus every type is a homotopy set).

    [nLab](https://ncatlab.org/nlab/show/Martin-L%C3%B6f+dependent+type+theory)

CIC
:   The Calculus of Inductive Constructions (CIC), the successor to CoC (Calculus of Constructions).

    This is basically what Rocq and Lean implement: [The Type Theory of Lean](https://github.com/digama0/lean-type-theory/releases).

HoTT
:   Homotopy Type Theory, using the univalence axiom (equalities are not necessarily trivial, isomorphisms between types are equalities in HoTT) to model infinity-groupoids.
    Importantly, equality retains its property of substitution.

    [HoTT Book](https://homotopytypetheory.org/book/), [nLab](https://ncatlab.org/nlab/show/homotopy+type+theory)

Cubical Type Theory
:   Cubical Type Theory implements builtin equality with a synthetic interval type (a type `I` with `0: I` and `1: I` being *non-discrete* points of `I`: they are distinct computationally, semantically equal).
    Cubical type theory explains HoTT computationally (that was a big result and a great success for HoTT as a foundation of mathematics) but has various complexities in implementation and usage.

    Cubical type theory also provides Higher Inductive Types (HITs).
    Since the interval used to formulate equalities is a type, it can participate as an argument to constructors, which provides syntax for equality constructors in HITs, although it needed formal justification as to why it actually works out beyond mere syntactic tricks.

    [nLab](https://ncatlab.org/nlab/show/cubical+type+theory)

OTT
:   Observational Type Theory, an alternative formulation of equality that computes on type formers.

    [Paper: Observational Equality, Now!](https://people.cs.nott.ac.uk/psztxa/publ/obseqnow.pdf), [nLab](https://ncatlab.org/nlab/show/observational+type+theory)

2LTT
:   Two-level type theory started with [adding a conventional metalanguage to Homotopy Type Theory](https://arxiv.org/abs/1705.03307), and was expanded to encompass more setups in [Staged Compilation with Two-Level Type Theory](https://andraskovacs.github.io/pdfs/2ltt.pdf).
    The idea is that two or more type theories with different properties can live together and interact in the same framework: in the former paper, it is to give Homotopy Type Theory a metalanguage with UIP (thus feeling more like programming); and in the latter, it is used to give staging of computation between the two layers, among other applications.
    Each level of the type theory is a fully featured dependent type system, although there may still be duplication between levels if level polymorphism is not built into the language.

DTT
:   Dependent Type Theory (not specific). (As far as I know, nobody has tried naming their theory this?)

System F
:   One of the standard *non-dependent* type theories, the basis of Haskell and many other functional programming languages.
    The exact impredicativity of System F is incompatible with dependent type universes, although this is usually not a huge obstacle for adapting useful programs from one theory to the other in practice.

Other flavours/implementations:

[sixten](https://github.com/ollef/sixten)/[sixty](https://github.com/ollef/sixty) (datatypes with unboxed representation polymorphism: sixten is System F-like while sixty has dependent types),
[Dhall](https://github.com/dhall-lang/dhall-lang/tree/master/standard) (a dependently typed language (essentially a core language) intended for configuration files),
[cooltt](https://github.com/RedPRL/cooltt) (cubical type theory implementation),
[epigram](http://www.e-pig.org/),

## Implementation techniques

De Bruijn index vs level
:   The two ways of numbering variables, giving them canonical names to avoid the problems of alpha-equivalence and capture-avoiding substitution.
    Index zero is the most recently bound variable (innermost scope), while *level* zero is the first bound variable (outermost scope).
    Pronounced [də ˈbrœyn].

Normalization by evaluation
:   Starting with an evaluation algorithm for type theory (one that uses closures to handle lambdas and such), it is extended to support partial evaluation by adding neutrals (terms whose evaluation is stuck on a variable or a typed hole), and then turned into a complete normalization algorithm by recursing under binders using neutrals to represent variables whose value is unknown.
    This yields an algorithm for normalization that avoids problems such as repeated substitution and shifting that would be present if operating on the raw syntax of terms directly.

    Not all type theories can be implemented this way, but it is one of the standard techniques for theories that can be.
    Since dependent type checking is interleaved with evaluation over *open* terms, having such an algorithm that does not involve code generation or a virtual machine interpreter and such is essential.

    Links: [GitHub: Elaboration Zoo in Haskell](https://github.com/AndrasKovacs/elaboration-zoo), [List of resources](https://github.com/etiamz/NbE-resources/blob/master/README.md)

Abstract Syntax Tree
:   The syntax of a programming language encoded as a tree-shaped data type (e.g. an ADT in Haskell, inductive types in proof assistants), where all the semantic nodes are easily distinguished.
    This is the representation that all of the algorithms work with: typechecking, evaluation, et cetera.

## Properties of type theories

consistency

soundness

canonicity

impredicativity (... good luck)

propositional resizing [1Lab](https://1lab.dev/1Lab.Resizing.html)

## Philosophy of type theory

Propositions as types


