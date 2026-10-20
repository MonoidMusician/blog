---
title: Higher Order Universe Polymorphism
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

Setup:

- Universes are always named by a fresh supply of variables (an incrementing counter (or maybe some state you can split into left and right components))
- Universe sizes are related by [poset-solver](poset-solver.html)

Basic setup would be

```haskell
data FirstOrderConstraints = FOC
  { ordering :: Map (UniVar, UniVar) Ordering
  , apart :: Set (UniVar, UniVar)
  }
```

```haskell
data HigherOrderConstraints = HOC
  { first :: Map UniVar (Map UniVar Comparison)
  , higher :: Map Var [UniVar]
  }
```

Each time you reference a thingy, you instantiate new fresh variables and add the constraints.

---

Whether or not they implement it, everyone seems to agree that top-level constructs can be universe polymorphic.

Strictly.

`zero.{u} : Nat.{u} : Type.{u}`

cumulativitively,

`zero.{u,v} : Nat.{u} : Type.{v} =| u <= v`

`succ.{w,u,v} : Nat.{w} -> Nat.{u} : Type.{v} =| w <= u <= v`

but this is kind of redundant nonsense: `Nat` lives in all universes equally, it contains no useful universe information:

`zero : Nat : Type.{v}`

`succ : Nat -> Nat : Type.{v}`

(where `v` is invented each time when inferring `Nat : Type.{v}`)

only if something actually contains a type, does it need a universe:

`Subtype.{w,u,v} : (T : Type.{w}) -> (T -> Prop.{u}) -> Type.{v} =| w <= v, u <= v` (or `w < v, u < v` if parameters are not available)

`mk : (T : Type.{w}) -> (P : T -> Prop.{u}) -> (t : T) -> (prf : P t) -> Subtype.{w,u,v} T P`

since the whole system is cumulative, we could simplify this: only the output type matters and the inputs can just fit

(the cumulativity can be handled in the typing rules, not the primitives)

`Subtype.{u} : (T : Type.{u}) -> (T -> Prop.{u}) -> Type.{u}`

`mk : (T : Type.{u}) -> (P : T -> Prop.{u}) -> (t : T) -> (prf : P t) -> Subtype.{u} T P`

if you return something like `Tuple Type.{u} Type.{v}`, then you need multiple universes

or if `w` and `u` have different constraints on them: if one is a weak inequality where the other is strict

so, in general, universe sizes originate in leaf nodes like `Type.{u}` and bubble up into other leaf nodes like `Subtype.{u}` as needed, maybe with simplifications along the way

---

now we want to support higher order universe polymorphism

just need abstract constraint templates

each top level can be thought of as coming with a constraint template `C w u v...`

`Subtype.{w,u,v} =| w <= v, u <= v` has `C w u v = w <= v, u <= v`

there is no need for any tricky higher order constraint sets here; just the one level of abstraction will do

note that for a functor `Type.{u} -> Type.{v}`, there are only three meanings that `C u v` can hold:

- phantom: `C u v` is trivial
- covariant: `C u v = u <= v` (including `C u v = u = v`, which gets weakened to `u <= v` by cumulativity)
- strict: `C u v = u < v`
- (the other directions never make sense)

`AdjointHom.{C,w,u,v} (tensor.{x,y,z} : Type.{x} -> Type.{y} -> Type.{z} =| C x y z) : Type.{w} -> Type.{u} -> Type.{v}`

`tensor.{x,y,z}` is quantified over those universe variables because they occur in its type: it brings along an abstract constraint set `C x y z` with fresh variables each time `tensor.{x,y,z}` is referenced; this `C x y z` is what is stored in the constraint context

then when `tensor` is chosen and `AdjointHom tensor` is instantiated, `C` is known

`AdjointHom.{C,w,u,v} tensor i o := exists (b : Type.{m}). (b, tensor.{x0,y0,z0} i b -> o) =| m < v, C x0 y0 z0, w <= x0, m <= y0, z0 <= v, u <= v`

you might think that `exists` might need to expose the universe level `m`, because it cannot be polymorphic over it like `forall` is
but the idea of this type theory is that everything is parametrically polymorphic; whatever `b : Type.{m}` is, it can be resized into another universe level (think of it as shifting levels around until the output level coincides with `m`; in a totally parametrically polymorphic language, you have room somewhere to make the levels fit)
(when a concrete `b` is known, it will bring with it the other concrete constraints that make sense, right?)
(that is, when it gets used, the usage will instantiate the polymorphic expression to be relative to the right things)

that is: assuming pervasive polymorphism eliminates bookkeeping. hopefully, and keeps the typing system agnostic to quantifier variance.

then, from the typing derivation of the definition, `AdjointHom` brings the constraints

`AdjointHom.{C,w,u,v} =| w <= x0, C x0 y0 z0, z0 <= v, u <= v`

(no transitivities are needed in this case, just pruning some variables not mentioned in the universe parameters or in an instantiation of `C`: namely `m`)

realistically, `C x0 y0 z0 = x0 <= z0, y0 <= z0`, thus

`w <= x0 <= z0 <= v, u <= v` aka
`w <= v, u <= v` as expected


