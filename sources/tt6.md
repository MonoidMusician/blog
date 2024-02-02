# The purpose of unification
Unification is a key part of typechecking: itʼs how the type checker proves two things are the same.
The trick about unification is that it has to work with syntactic equality, and not semantic equality.
This is because semantic equality is undecidable, so no algorithm exists for it, but there are algorithms that can approximate it via syntactic equality.
(It means that the algorithm might miss stuff, since semantic equality is richer than syntactic equality, but if two things are syntactically equal they must also be semantically equal, so nothing ill-behaved will happen.)

## Basic unification ideas
There are a bunch of rules that define how unification operates.
The exact details depend on the many details of how a particular type theory works, but there are some basics that are more or less universal.

The most basic rule is that if two types have the same abstract syntax tree, then they can be unified.
This means that two types with the same names unify, of course.
It means that function types unify when their input types and their output types unify (`i -> o` unifies with `i' -> o'` when `i` unifies with `i'` and `o` with `o'`).
Other type constructors follow a similar pattern to function types.

Things get much more interesting when the algorithms solves for (type) variables by unification.
I wonʼt get into all the details of how variables work, but the primary cases this comes up are:
- In Haskell-like languages, this occurs when applying a polymorphic function, where the polymorphic type variables are solved for implicitly.
- In most dependently-typed languages, this occurs when applying a function with implicit arguments.
  (Note that these need not be types, they could be values.)
- Other cases might be when there are wildcards or holes that should be solved for.

