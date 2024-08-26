---
title: MonoidMusicianʼs Type Theory Musings
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Welcome to my (un)scrambled thoughts on all things from the intersection of homotopy type theory, functional programming, and set theory!
Thereʼs going to be a lot of terminology being thrown around, but donʼt be intimidated: hopefully Iʼll either describe the term, link to a definition/explanation, or youʼll be able to figure it out from context (or just skip it!).
I do believe the best way to be clear is to be precise with language and to use established terminology, but because I am synthesizing pretty disparate areas of knowledge, most people wonʼt know all the terms.

My goal with these are to raise some new perspectives on common themes, and cross-pollinate these fields with each other.
Iʼd love it if more mathematicians knew what inductive types were (theyʼre so useful!) and could see how closely their existing terminology is reflected in type theory.
Similarly, programmers will be astounded to know how quotients and subtypes can formalize the intuitions that theyʼve been working with for years, and make it rigorous enough for a proof assistant to understand and verify.
And of course, everyone can learn a thing or two about functions, these wonderful objects.
And I would love it if Univalence was slightly less scary.

But most of all, I believe type theory is an exceptional extensible framework for conceptualizing some of the most important fundamentals questions at all levels of formality:

- What is _data_?
- What is _computation_?
- What constitutes a _proof_?
- How do we talk about _infinity_?
- What is _equality_?
- What is a _function_?
- What role do _axioms_ play in determining the properties of a formal system?
- How can we make math _usable_?

## What is Type Theory?
Type theory is an incredibly powerful tool for formalizing mathematical and computational thoughts.
Type theory has the semantics of mathematics wrapped up in the syntax of programming, but of course mathematicians can still work in type theory will less formalized notation, and programmers can focus on the computational aspects of type theory to their heartsʼ content.

Type theory has a straightforward syntax that is typically specified in a formal way, often with the intent of being machine-parsable.
(I probably wonʼt get into the syntax we use for specifying type theories, unless thereʼs interest.)
The syntax is incredibly precise and can also be concise (particularly with anonymous functions!).
I believe having this formal foundation is a great boon for readability and teachability.

At the heart of type theory is the **type judgment** `t : T`, read as “the term `t` inhabits (or has) type `T`”.
The user does not get to choose what type a term has, it is simply a consequence of the rules of the language, based on the syntactic structure of the term (also depending on what names are in scope, of course).
Typically it is thought of as having a unique type, although certain parts of the expression may be elided resulting in the appearance of an ambiguous type, with the details being understood by convention.
(This happens in math notation all the time!)

Type theoryʼs power is offered by functions, above all.
A **function** `f` from type `A` to `B`, i.e. `f : A -> B`, is simply a term `b : B` that has variable `a : A` in scope.
Note that we often use `f a` for function application (instead of the more verbose `f(a)`), and that every variable in scope has a specified type.

The special sauce of dependent type theory is the **dependent functions**, where the output type can depend on the value of the input term(!). So given a type `A`, a type family `B : A -> Type`, a dependent function `g : Π(a : A), B(a)` is specified by giving a term `b : B(a)` with `a : A` in scope.
Note that the type of the output depends on the value of the input!
This is the machinery that powers the use of dependent type theory in proof assistants, and other sophisticated applications.

Beyond functions, type theory is quite extensible and modular, and throughout this document I will cover various extensions.
Just like common mathematics usage, thereʼs a set of agreed upon conventions as to what is standard and what notation is used.
Furthermore thereʼs also some abuse of notation, where a term is technically ill-typed but understood to refer to a different but related term (a fair number of these exceptions could actually be formalized as coercions that are elaborated based on expected types).

In addition, there are a lot of extensions that donʼt change the semantics of the type theory, but make it nicer to work with as a proof/programming language.
These are things like modules, type classes, tactics.

I love to explain parts of the process involved in analyzing type theory (particularly type inference!), but itʼs not necessary to understanding how to use it (just know that things are, for the most part, on solid intellectual ground, with decades of research and practical use behind it).

## But Iʼm Used to Set Theory?!?
Understandable!
Most of modern mathematical parlance is phrased in terms of set theory, when foundational concerns or clarity is necessary, and set theory is overwhelmingly taught to give intuitions for mathematical objects and operations.
But type theory is more or less equivalent, and I think it can be clearer once you get a hang of it.

The biggest difference is, of course, that you cannot prove that a term inhabits a type!
Instead, youʼll need to use a function, bijection/equivalence, or equality to transport from one type to a related type.
For subtypes, youʼll need a proof to construct a term of the subtype from the term of the parent type.

Table: Comparing terminology:

|set-theoretic|type-theoretic|
|--|--|
|set (e.g. “the set of natural numbers” or “the set of prime numbers”)|type or subtype (e.g. “the type of natural numbers” or “the subtype of prime numbers”)|
|subset|subtype or powerset (yes, itʼs not called powertype)|
|family of sets|type family or function from a specific type (the index) to the proper type of all types, e.g. `T -> Type`|
|set of equivalence classes|quotient|

Other differences:

- We often use spaces as function application, e.g. `f a` reads “apply argument `a` to function `f`” (where in math notation we would use `f(a)`).
  Since functions have primary importance, and all functions are unary, the parentheses are a lot of extra noise and not necessary.
  This convention is particularly common in functional programming parlance (e.g. Haskell does function application like this).
  However, we will often write tuples as `(x, y)`, which does look a lot like applying multiple arguments to a function (but is more versatile): `f (x, y)`.

# In this document
