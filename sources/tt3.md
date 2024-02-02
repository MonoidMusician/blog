# Where is the information?

<!-- ʼ Π ≡ ℕ → λ Σ ≃ -->

I always find it interesting to track which parts of a definition (mathematical, etc.) are uniquely determined by the other parts.
This is a quasi-combinatoric view that data is determined by freedom to make choices, and everything else, while informative in one sense of providing properties/boundaries/restrictions, does not actually provide the information that characterizes an object.
Itʼs applicable in many contexts, and Iʼll just go through some hand-wavey examples to show you how I think about it.

Note that I will be assuming univalence in this post, which means isomorphic types are equal.
This greatly simplifies how types are treated, as you will see.

## Algebraic examples

Letʼs take the algebraic hierarchy of magmas to groups as our first set of examples.
At each step we will discuss what each piece of data brings to the picture.

### Magmas

Magmas have a carrier type `T : Set` (an [h-set](https://ncatlab.org/nlab/show/h-set)) and a binary operation `op : T → T → T`.
In code it would be `magma := Σ(T : Set), T → T → T`, or as a structure in Lean-prover syntax:
```lean
structure Magma :=
(T : Set)
(op : T → T → T)
```

It turns out that the type doesnʼt matter as much as the operation does, especially once it is wrapped in a magma that hides the type (I will try to justify this later in the section on categories).
For any equivalent types `T1` and `T2`, you can take an operation `op1 : T1 → T1 → T1` and define an equivalent operation `op2 : T2 → T2 → T2` by transporting along the equivalence as needed.
This is esssentially the definition of what it means for two operations on possibly different types to be equivalent – that is, for two magmas to be equal:
```lean
Π(T1 T2 : Set), Π(op1 : T1 → T1 → T1), Π(op2 : T2 → T2 → T2),
(Magma.mk T1 op1 = Magma.mk T2 op2) = Σ(t : T1 = T2), Π(a b : T1), apply t (op1 a b) = op2 (apply t a) (apply t b)
```

In words this means for all `T1`, `T2`, `op1`, and `op2` as above, the corresponding magmas are equal precisely when the types are equal and the operations are also equal when transported along that equality.
(Note that due to proof-relevance in HoTT, there may be many ways of establishing an equality between types, so magmas on the same type may be equal in interesting ways.)

Of course, since we are working in type theory, the type matters, but really it is determined by the function (two functions could not be considered equivalent in any reasonable sense if their types were not equivalent), so for the purposes of our argument, we will treat is as uniquely determined by the operation.

Thus a magma is essentially determined by its operation.

<!-- TODO: non-exhaustive magmas -->

### Semigroups
Semigroups are magmas whose operation is associative:
```lean
structure Semigroup :=
(T : Set)
(op : T → T → T)
(notation a `•`:65 b:65 := op a b)
(assoc : Π(a b c : T), a • (b • c) = (a • b) • c)
```

Note that since `T` is an `h-set`, `assoc` will be an `h-prop`, which means it contributes no data, only a unique proof.
So if you accept that magmas are uniquely determined by their operation, semigroups are still uniquely determined by that.

### Monoids
Monoids are semigroups with an identity element `i`:
```lean
structure Monoid :=
(T : Set)
(op : T → T → T)
(notation a `•`:65 b:65 := op a b)
(assoc : Π(a b c : T), a • (b • c) = (a • b) • c)
(i : T)
(identity : Π(a : T), a • i = a ∧ i • a = a)
```

Now we are adding data, `i : T`, which looks like it could be any element of `T`, an arbitrarily large type!
But it turns out that it is still uniquely determined by the operation:
for identities `i1 i2 : T`, we have a proof that `i1 • i2 = i1` (right identity of `i2`) and another that `i1 • i2 = i2` (left identity of `i1`), so it must be that `i1 = i2`.

Another way to formulate it would be with the following sigma type replacing the separate fields `i` and `identity`:
```lean
def IdentityOf {T : Type} (op : T → T → T) :=
  Σ(i : T), Π(a : T), op a i = a ∧ op i a = a
```

And then one could prove that `IdentityOf op` is a subsingleton, i.e. h-prop.

So we see that each operation has at most one identity.
So a monoid describes a magma with some proofs of nice properties, but no additional choices were made after deciding what operation to use (and on what type).

How will it look for groups?

### Groups
Groups are monoids with inverses:
```lean
structure Groups :=
(T : Set)
(op : T → T → T)
(notation a `•`:65 b:65 := op a b)
(assoc : Π(a b c : T), a • (b • c) = (a • b) • c)
(i : T)
(identity : Π(a : T), a • i = a ∧ i • a = a)
(inv : T → T)
(inverse : Π(a : T), a • inv a = i ∧ inv a • a = i)
```

Again, thereʼs a standard abstract algebra proof that inverses are unique, which means the function `inv` witnessing the totality of the inverse relation is also unique, so again, being a group is a property of the magma operation, there are no additional choices made in the definition of a group.

Like before, `inv` and `inverse` could be bundled into a subsingleton sigma type like so:
```lean
def InverseOf {T : Type} (op : T → T → T) (i : T) :=
  Σ(inv : T → T), Π(a : T), op a (inv a) = i ∧ op (inv a) a = i
```

## Takeaways
Here weʼve been studying magmas &c. as mathematical objects, and we saw that they were HoTT-equal when their operations were equivalent under the change of type, and, as a related fact, that the extra fields of monoids/groups added information about properties of the operation but no additional data (i.e. choices).

However, these algebraic structures are most often used via typeclasses, in programming languages like Haskell and theorem provers like Lean, where they are parameterized over their carrier type: `class magma (T : Type) := (op : T → T → T)`, etc.
In this way, an instance of the class provides a canonical choice of operation for a particular (nominal) type, and easy/implicit access to the properties like associativity.
This can be great for brevity, but can be a challenge where there are multiple natural choices for an operation on the same type, such as addition/multiplication, or when you run into other dark corners of typeclass issues (like diamonds, where the same type can have two different instances from generic derivations).

But as it turns out, most of these issues go away when you parameterize the classes by the operation (and leave the type implicit), like `class semigroup {T : Type} (op : T → T → T) := (assoc : _)`.
All the data thatʼs hidden away through the implicit resolution of the typeclass system is no surprise, as it can only follow from the operator, so there wonʼt be any chance of two incompatible instances being inferred (although one could still run into issues with a lack of definition equality in identities and inverses).

Furthermore, I would also argue that this kind of structure philosophically fits better in a Univalent universe, because it acts more like a well-defined-but-partial function from `Magma -> {Group,Monoid,Semigroup,...}`, one that respects type equalities instead of making decisions based on type names.

While the operators would have to be explicit with this setup, it is compatible with the existence of canonical choices (e.g. a `+` operator could still be disambiguated by type), and it still retains the ability for identities and inverses to be implicit in some sense (since they are not parameters to the structure), but syntactically disambiguating the identites would get harder in general.

On a final note, of course these results generalize to other algebraic structures, such as the hierarchy from rigs/semirings and rings to fields, which would need to be parameterized over the additive and multiplicative operators together.

## Other examples
### Categories
Letʼs apply what weʼve learned to (small) categories, which are much richer objects than magmas. Start with the laws:
```lean
structure CategoryLaws (obj : Set) (hom : obj → obj → Set)
  (comp : Π{a b c : obj}, hom a b → hom b c → hom c d)
  (id : Π(o : obj), hom o o) :=
(left_id := Π{a b : obj} (f : hom a b), comp id f = f)
(right_id := Π{a b : obj} (f : hom a b), comp f id = f)
(comp_assoc := Π{a b c d : obj} (f : hom a b) (g : hom b c) (h : hom c d), comp f (comp g h) = comp (comp f g) h)
```

Now a category, as a mathematical object, looks like
```lean
structure Category :=
(obj : Set)
(hom : obj → obj → Set)
(comp : Π{a b c : obj}, hom a b → hom b c → hom c d)
(id : Π(o : obj), hom o o)
(lawful : CategoryLaws obj hom comp id)
```

What information characterizes this? Well, as we were saying earlier, it isnʼt `obj`, since it can be inferred from `hom`. Objects are essential for the form and structure of a category, dictating which morphisms can be composed, but they are not the important part. This is exactly what category theorists have been saying for decades: itʼs the arrows that matter, not the objects!

But wait a second, is that really right? Actually, I would argue that `comp` is what determines the category – since `hom` can be inferred from that! This seems a little weird, since we very rarely talk about what composition looks like in a category. This is because it is usually determined by `hom` and parametricity (e.g. for functions, there really is only one way to parametrically compose them, the obvious combinator `forall a b c. (a -> b) -> (b -> c) -> (a -> c)`), and once you add in the restrictions on associativity and identity, there is usually a unique choice (I think relations fall into this category) or an canonical “interesting” choice.

Of course, `id` does not contribute information (it is uniquely determined by `comp`).

So it looks like `hom` often determines what category we are talking about, but it is really `comp` . All the rest of the fields serve to constrain `comp` in some way, saying which homs it needs to be able to relate, or what properties it should have.

In fact, another way to look at categories would be to take the `comp` operation to be partial not in the sense of requiring its arguments to have type indices that align (`forall a b c. hom a b -> hom b c -> hom a c`), but being able to return nothing for certain combinations of arguments (`hom -> hom -> Maybe hom`). This looks more like a magma operation, but the indices need to be recovered with `dom : hom -> obj` and `cod : hom -> obj` which define where composition is allowed: `comp f g = Nothing` iff `cod f = dom g`. It turns out that this is less convenient to work with, but it is mathematically equivalent with the right rules (e.g. `id : obj -> hom` and `comp f (id (cod f)) = Just f`). Hopefully this helps clarify why `comp` is the locus of information in a category.

### Cardinals and Ordinals

## The rules
The heuristics used informally above gathered in one place:
1. Consider types to be non-informative when they can be inferred from later arguments (so pretty much most of the time). Behavior is determined by functions and terms, not by types!
2. Proofs (of h-props) are non-informative. Usually these are pretty obvious.
3. Sometimes terms, even functions, are non-informative when there are restrictions on them that make them unique (e.g. identities and inverses of operations). This will require more careful examination and arguments than the other cases but is definitely worth figuring out.
