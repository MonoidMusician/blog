---
title: Trying extra reduction rules for induction
subtitle: (Notes)
date: 2021/04/03 – 2021/04/10
---

Here's my definitions:
```agda
inductive N where
  Z : N
  S : N -> N
rec : {R : U} -> R -> (R -> R) -> N -> R
add := \n m, rec n S m
mul := \n m, rec Z (add n) m
```

Now for some laws:
```agda
recZ : rec z s Z = z
recS : rec z s (S n) = s (rec z s n)
triv : rec a (\b, b) n = a
extn : rec Z S n = n
extr : rec (f a) (\b, f (g b)) n = f (rec a (\b, g (f b)) n)

addZ : add n Z = n
  from recZ
addS : add n (S m) = S (add n m)
  from recS
Zadd : add Z m = m
  from extn
Sadd : add (S n) m = m
  from extr
mulZ : mul n Z = Z
  from recZ
mulSZ : mul n (S Z) = n
  from indS, addZ
Zmul : mul Z m = Z
  from Zadd, triv
SZmul : mul (S Z) m = m
  from Sadd, Zadd, extn
```

`recZ` and `recS` are the standard computation rules, of course. `triv` and `extn` are kind of obvious lemmas and it turns out they let us compute `Z` on the “wrong” side of `add` and `mul`. `extr` is a little tricky, but the idea is if every case ends in the same thing, we should be able to pull it out, extract it through the cases, and in particular we pull the successor through addition. (It makes sense to restrict `f` to be a series of constructors, to avoid issues of higher-order unification, of course.)

However we get stuck here: `mul (S (S Z)) m = rec Z (add (S (S Z))) m = rec Z (\r, S (S r)) m`{.agda} but `mul n (S (S Z)) = rec Z (add n) (S (S Z)) = add n (add n Z) = add n n = rec n S n`{.agda}. We don't get a general `mulS` or `Smul` lemma.

I have some vague notion that maybe we can split the recursor `rec a (\b, f (f b)) m = rec (rec a f m) f m`. But this still only handles 2, and we need to generalize to any n. A full theory here would basically capture the relationship of natural-number powers of endofunctions.

A lot of these things can be generalized to arbitrary inductive types.

Not sure of the behavior of these rules as an actual rewrite system&nbsp;…

Anyways, still doesn't get judgmental commutativity, but we can get the next best thing (at least for addition): constructors compute on both sides.

-----

```agda
pow f = rec id (f .)
tension between structure and concrete knowledge

extr
rec (f a) (f . g) n = f (rec a (g . f) n)
Z: refl: f a = f a
S: cong (f.g): f (g (rec (f a) (f . g) n)) = f (g (f (rec a (g . f) n))

pow2
rec (rec a f n) f n = rec a (f . f) n
Z: refl: a = a
S: cong (f.f) . extr: f (rec (f (rec a f n)) f n) = f (f (rec (rec a f n) f n)) = f (f (rec a (f . f) n))

rec (rec a f n) (\b. rec b f m) l = rec a f (add n (mul m l))???

rec (rec a f n) (pow f m) n = rec a (pow f m . f) n
Z: refl: a = a
S:
  Z: refl: rec (rec a f (S n)) id (S n) = rec a f (S n)
  S: (f . rec id (f .) m) (rec (f (rec a f n)) (f . rec id (f .) m) n) = ... = (f . rec id (f .) m . f) (rec a (f . rec id (f .) m . f) n)

rec a (pow f m) n = pow f (mul m n) a

rec (rec a (\b. rec b f x) m) (\b. rec b f y) n) = rec a f (x*m + y*n)
```

-----

```agda
pow f n . pow f m = pow f (n + m)
rec f (g .) n a = rec (f a) g n

S n + m = S (n + m) = n + S m

add := \n m, rec n S m
mul := \n m, rec Z (add n) m

(S n) * m
rec Z (add (S n)) m
rec Z (S . add n) m
rec Z (\r, S (rec n S r)) m
?? add m (rec Z (add n) m)
m + (n * m)

m * (S n)
rec Z (add m) (S n)
add m (rec Z (add m) n)
m + (m * n)

(a + b) * x
rec Z (add (a + b)) x
rec Z (rec (a + b) S) x
rec Z (rec (rec a S b) S) x

a * (x + y)
rec Z (add a) (x + y)
rec A (rec a S) (rec x S y)
```