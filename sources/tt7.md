## The Diagonal of Universe Level and H-Level.
In a type theory without HITs, it seems to me that there is some correspondence between universe level and h-level. In particular (without HITs), types in Type 0 will be at most h-sets, types in Type 1 will have at most h-level 3, etc.

## Parametricity and Predicativity
In Reynoldʼs paper on parametricity, it is stated that set-theoretically, a naïve definition of functions that polymorphic over sets is too large to form a set itself, and instead must be a class, but, crucially, they are equivalent to a set when they are parametric. We see a parallel in (predicative) type theory, where polymorphic functions must lie in a universe higher than what they quantify over, but of course parametric functions biject with corresponding inductive types in a well-known way (e.g. with Boehm-Berarducci encoding), and in fact the inductive type will lie in a smaller universe because it skirts the issue of quantification. Is there a way to recover this kind of shrinking effect in type theories without inductive types?

For example, if I had a dependent type theory with only universes and pi types, I would still be able to form a type of “natural numbers” and many other inductive types, but they would all live in universes that are too large, and in fact it seems like it would lead to an explosion of universes.

In particular, if we define a few types:
```
Nat_n : U_(n+1) :=
  (∀T:U_n, (T → T) → T → T, suitably restricted to be parametric)
List_n,m : U_n -> U_(n+1 ∨ m) :=
  λA:U_m, (∀T:U_n, (T → A → T) → T → T, suitably restricted to be parametric)
```

## Prop elimination

## More generally, “prove” that a thing lives in a universe?

## Internalize lack of injection to higher universe?
Obviously `Type 1` and `Type 2` are not isomorphic. Is there an internal proof of this?