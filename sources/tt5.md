<!-- ʼ ≡ -->

I want to write some brief comments on equality in type theories.

If you only take away one thing, let it be this:

> Definitional equality `t1 ≡ t2` means that `t1` and `t2` are **obviously** equal, by a decidable syntactic algorithm, and is the basis for a compilerʼs typechecking process, as well as internal propositional equalities via `refl`.

One of the basic facts about type theories is that they rely on two primary judgments:

- A term `t` is well-typed with a type `T`: `t : T`
- Two terms `t1` and `t2` with the same type `T` are equal: `t1 ≡ t2`

(Note that each judgment takes place in a context, usually labelled `Γ`, which is just a list pairing variables that have been bound with their types, but we omit it here for brevity.)

A compiler for a type theory will look at a term and follow certain rules to associate it with a type (type inference), or verify that it has a specified type (type checking).
The mathematical formalization of type theories doesnʼt really distinguish these, and the typing judgment acts like a bit of both.

Letʼs consider a fundamental example of a typing judgment: function application.
If I have a function `f`, and I want to apply it to an argument `a`, how does a compiler know that they are compatible?
(Note that these stand for well-typed expressions, so they could be variables in scope, but they donʼt have to be.)

Letʼs say that `f : T1` and `a : T2`.
Well, first things first, we want `f` to actually be a _function_ type: we require `f : T3 -> T4`.
Next we want to apply `a` to it, so its type had better line up with the functionʼs input type.
We might ask that `a : T3` instead, since that is the functionʼs input type, but itʼs far better to make explicit what we mean:
letʼs instead ask for `T2 ≡ T3`, since a priori we donʼt know they are equal, and we instead want to require that they are indeed.
If all these conditions are satisfied, we say that `f a : T4`.

In the syntax of judgments we would write it this way:

```
f : T3 -> T4 -- require `f` has a function type
a : T2       -- require `a` is well-typed
T2 ≡ T3      -- require that `a`ʼs type matches the input type
____________
f a : T4     -- then `f` applied to `a` has the output type
```

This is a simple but fundamental example: in order to make sure everything lines up, compilers are constantly checking whether types are equal.
And in dependently-typed languages, where terms can show up in types, this extends to terms too.

Now it turns out that equality is really hard to prove or check; otherwise mathematicians would be out of their jobs!
But compilers do their best with a limited set of well-behaved rules.

Obviously one start is that if two expressions are literally the same, then they are judgmentally equal.
Then next we ask: what if they bind different variables, but are still really the same?
This is called alpha-equivalence, it means that `λ(a : t) → a` and `λ(b : t) → b` are definitionally equal, which hopefully makes sense (theyʼre both the identity function!).
And so on: there are particular rules that solve some part of definitional equality (like eta-equivalence, beta-equivalence, etc.).

All this means that the compiler can do a fair amount of work in order to check that two things are equal.
But thereʼs no magic to it: it wonʼt solve calculus equations for sure!
It really underestimates when two things are equal, which in turn means that it cannot be used to prove that two things are different.

This is where we need to talk about propositional equality, which can be defined like so (in Agda):

```agda
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
```

Or like this, in Lean:

```lean
inductive eq {T : Type n} : T -> T -> Type n
| refl : Π {x : T}, eq x x -- notated x = x
```

Donʼt let Agdaʼs notation fool you: this isnʼt judgmental equality anymore!
For clarity I will stick to a normal equals `=` for propositional equality.

Propositional equality really starts with definitional equality:
we really need to use `refl` to prove that two things are equal – after all, it _is_ the only constructor – and it turns out that `refl : x = y` is only well-typed when `x ≡ y` definitionally.

But propositional equality has more flexibility than judgmental equality, since it is internal to the type theory and it lives in a type universe, it can be asked for as an argument, it can be negated to ask that two things are not equal, and it can be used for complex proofs (including of calculus!).
But the fundamental start of it all, how you prove anything is equal at all, is definitional equality.

A couple miscellaneous notes for now:

- Judgmental equality should follow the rules of normal equalities, such as symmetry and transitivity, but at least the Lean prover fails to be adequately transitive in some cases (particularly regarding quotients).
- A (good) compiler will always accept replacing one term with another that is definitionally equal to it
- The equality type is semantically the smallest reflexive relation; in HoTT it is more complex than its inductive definition would suggest, but it still works as if it just had that one constructor (I cannot hope to justify that here, but you probably do not need to worry about it unless you are specifically interested in HoTT).
- Some type theories, called extensional type theories, reflect propositional equalities back to judgmental equalities, so there is no distinction, but it makes type checking undecidable