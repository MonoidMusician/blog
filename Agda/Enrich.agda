module Enrich where

open Agda.Primitive renaming (Set to Type; Setω to Typeω)
open import Data.Nat hiding (_⊔_)
open import Relation.Binary.PropositionalEquality.Core
  using (_≡_; _≢_; refl; sym; cong; trans; subst)
open import Data.Unit.Polymorphic.Base
  using (⊤; tt)
open import Function.Base
  using (_$_)

Fun : {ℓ : Level} → Type ℓ → Type ℓ → Type ℓ
Fun x y = x → y

identity : {ℓ : Level} {X : Type ℓ} → Fun X X
identity x = x

-- Focus a subproblem before finishing the proof
focus_⟨_⟩ : {ℓ : Level} {T₁ T₂ : Type ℓ} {x y : T₁} {z : T₂} →
  (f : T₁ → T₂) → (x ≡ y) → (f y ≡ z) → (f x ≡ z)
focus f ⟨ p ⟩ q = trans (cong f p) q

_~_ : {ℓ : Level} {T : Type ℓ} {x y z : T} →
  (x ≡ y) → (y ≡ z) → (x ≡ z)
_~_ = trans

infixr 1 _~_

-- Type ascription
ascribe⟨_⟩_ : {ℓ : Level} (T : Type ℓ) → T → T
ascribe⟨ T ⟩ t = t

_∋_≡_ : {ℓ : Level} (T : Type ℓ) → T → T → Type ℓ
T ∋ x ≡ y = x ≡ y

record TypeCat {ℓ₁ ℓ₂} (Ob : Type ℓ₁) (Hom : Ob → Ob → Type ℓ₂) : Type (ℓ₁ ⊔ ℓ₂) where
  field
    id : {x : Ob} → Hom x x
    _⨟_ : {x y z : Ob} → Hom x y → Hom y z → Hom x z
    lid : {x y : Ob} → (f : Hom x y) → id ⨟ f ≡ f
    rid : {x y : Ob} → (f : Hom x y) → f ⨟ id ≡ f
    assoc : {m n p q : Ob} → (f : Hom m n) → (g : Hom n p) → (h : Hom p q) → (f ⨟ g) ⨟ h ≡ f ⨟ (g ⨟ h)
  infixr 40 _⨟_
  record Iso (x y : Ob) : Type ℓ₂ where
    field
      f : Hom x y
      g : Hom y x
      fwd : f ⨟ g ≡ id
      bwd : g ⨟ f ≡ id

FunTypeCat : TypeCat Type Fun
FunTypeCat = record
  { id = λ x → x
  ; _⨟_ = λ f g x → g (f x)
  ; lid = λ _ → refl
  ; rid = λ _ → refl
  ; assoc = λ _ _ _ → refl
  }

record TypeMonCat {ℓ₁ ℓ₂} (Ob : Type ℓ₁) (Hom : Ob → Ob → Type ℓ₂) (Mon : Ob → Ob → Ob) (I : Ob) : Type (ℓ₁ ⊔ ℓ₂) where
  field
    C : TypeCat Ob Hom
  open TypeCat C
  field
    lI : {x : Ob} → Iso (Mon I x) x
    rI : {x : Ob} → Iso (Mon x I) x
    MM : {x y z : Ob} → Iso (Mon (Mon x y) z) (Mon x (Mon y z))
    mon : {m n p q : Ob} → Hom m n → Hom p q → Hom (Mon m p) (Mon n q)
    idmon : {m p : Ob} → mon (id {m}) (id {p}) ≡ id {Mon m p}
    ⨟mon : {m n o p q r : Ob} → (mn : Hom m n) → (no : Hom n o) → (pq : Hom p q) → (qr : Hom q r) →
      mon mn pq ⨟ mon no qr ≡ mon (mn ⨟ no) (pq ⨟ qr)
  -- open TypeCat.Iso lI renaming (f to lIf; g to lIg)
  -- open TypeCat.Iso rI renaming (f to rIf; g to rIg)
  -- open TypeCat.Iso MM renaming (f to MMf; g to MMg)

record TypeEnrCat
  {ℓ₁ ℓ₂ ℓ₃}
  {OB : Type ℓ₁}
  {HOM : OB → OB → Type ℓ₂}
  {MON : OB → OB → OB}
  {I : OB}
  (C : TypeMonCat OB HOM MON I)
  (Ob : Type ℓ₃) (Hom : Ob → Ob → OB) : Type (ℓ₁ ⊔ ℓ₂ ⊔ ℓ₃) where
  open TypeMonCat C using (lI; rI; MM; mon) renaming (C to CC)
  open TypeCat CC renaming (id to ID; assoc to ASSOC)
  field
    id : {x : Ob} → HOM I (Hom x x)
    comp : {x y z : Ob} → HOM (MON (Hom x y) (Hom y z)) (Hom x z)
    lid : {x y : Ob} → mon id (ID {Hom x y}) ⨟ comp ≡ Iso.f (lI {Hom x y})
    rid : {x y : Ob} → mon (ID {Hom x y}) id ⨟ comp ≡ Iso.f (rI {Hom x y})
    assoc : {m n p q : Ob} → Iso.f MM ⨟ mon (ID {Hom m n}) (comp {n} {p} {q}) ⨟ comp ≡ mon (comp {m} {n} {p}) (ID {Hom p q}) ⨟ comp

-- Type of objects for each level
record Enriched {ℓ₁ ℓ₂} (O : (n : ℕ) → Type ℓ₁) : Type (ℓ₁ ⊔ lsuc ℓ₂) where
  field
    -- Monoidal tensor for each level
    _⊗_ : {n : ℕ} → O n → O n → O n
    -- Identity objects for each level
    I : {n : ℕ} → O n
    -- Hom-objects
    _~>_ : {n : ℕ} → O n → O n → O (suc n)
    -- Global object (suggestively named)
    I=>_ : {n : ℕ} → O n → Type ℓ₂
  -- Hom-sets, via the global object
  _=>_ : {n : ℕ} → O n → O n → Type ℓ₂
  _=>_ = λ x y → I=> (x ~> y)
  infixr 50 _⊗_
  infix 30 _~>_
  infix 20 _=>_
  infix 20 I=>_
  field
    -- Characterize the global objects: (I=> (I ~> x)) ≅ (I=> x)
    ↑I=> : {n : ℕ} {x : O n} → (I => x) -> I=> x
    ↓I=> : {n : ℕ} {x : O n} → I=> x -> (I => x)
    ↑↓I=> : {n : ℕ} {x : O n} (f : I=> x) → ↑I=> (↓I=> f) ≡ f
    ↓↑I=> : {n : ℕ} {x : O n} (f : I => x) → ↓I=> (↑I=> f) ≡ f
    -- Tensor identity
    ↓⊗_ : {n : ℕ} (x : O n) → I ⊗ x => x
    ↑⊗_ : {n : ℕ} (x : O n) → x => I ⊗ x
    _⊗↓ : {n : ℕ} (x : O n) → x ⊗ I => x
    _⊗↑ : {n : ℕ} (x : O n) → x => x ⊗ I
    -- Tensor assoc
    _⊗→_⊗_ : {n : ℕ} → (x y z : O n) → (x ⊗ y) ⊗ z => x ⊗ (y ⊗ z)
    _⊗_←⊗_ : {n : ℕ} → (x y z : O n) → x ⊗ (y ⊗ z) => (x ⊗ y) ⊗ z
  infixr 80 ↓⊗_
  infixr 80 ↑⊗_
  infixl 81 _⊗↓
  infixl 81 _⊗↑
  infix 70 _⊗→_⊗_
  infix 70 _⊗_←⊗_
  field
    -- Identity arrows
    id : {n : ℕ} {x : O n} → x => x
    -- Composition
    [⨟] : {n : ℕ} {x y z : O n} → ((x ~> y) ⊗ (y ~> z)) => (x ~> z)
    -- Tensor functor
    [⊠] : {n : ℕ} {x₁ x₂ y₁ y₂ : O n} → (x₁ ~> y₁) ⊗ (x₂ ~> y₂) => (x₁ ⊗ x₂ ~> y₁ ⊗ y₂)
  [id] : {n : ℕ} {x : O n} → I => (x ~> x)
  [id] = ↓I=> id
  field
    -- Evaluating global morphism at global element
    ev : {n : ℕ} {x y : O (suc n)} → x => y → I=> x → I=> y
    -- Pairing global elements
    pair : {n : ℕ} {x y : O (suc n)} → I=> x → I=> y → I=> (x ⊗ y)
  _⨟_ : {n : ℕ} {x y z : O n} → (x => y) → (y => z) → (x => z)
  _⨟_ = λ f g → ev [⨟] (pair f g)
  infixr 20 _⨟_
  _⊠_ : {n : ℕ} {x₁ x₂ y₁ y₂ : O n} → (x₁ => y₁) → (x₂ => y₂) → (x₁ ⊗ x₂ => y₁ ⊗ y₂)
  _⊠_ = λ f g → ev [⊠] (pair f g)
  infixr 50 _⊠_
  field
    evid : {n : ℕ} {x : O (suc n)} → ev (id {suc n} {x}) ≡ identity
    ev⨟ : {n : ℕ} {x y z : O (suc n)} {f : x => y} {g : y => z} → ev (f ⨟ g) ≡ (λ x → ev g (ev f x))
    id⊠id : {n : ℕ} {x₁ x₂ : O n} → id ⊠ id ≡ id {n} {x₁ ⊗ x₂}
  field
    ↑↓⊗_ : {n : ℕ} (x : O n) → ↑⊗ x ⨟ ↓⊗ x ≡ id
    ↓↑⊗_ : {n : ℕ} (x : O n) → ↓⊗ x ⨟ ↑⊗ x ≡ id
    _⊗↑↓ : {n : ℕ} (x : O n) → x ⊗↑ ⨟ x ⊗↓ ≡ id
    _⊗↓↑ : {n : ℕ} (x : O n) → x ⊗↓ ⨟ x ⊗↑ ≡ id
    _⊗→←_⊗_ : {n : ℕ} (x y z : O n) → x ⊗→ y ⊗ z ⨟ x ⊗ y ←⊗ z ≡ id
    _⊗_→←⊗_ : {n : ℕ} (x y z : O n) → x ⊗ y ←⊗ z ⨟ x ⊗→ y ⊗ z ≡ id
  field
    [id⨟] : {n : ℕ} {x y : O n} → id ⊠ [id] ⨟ [⨟] ≡ (x ~> y)⊗↓
    [⨟id] : {n : ℕ} {x y : O n} → [id] ⊠ id ⨟ [⨟] ≡ ↓⊗(x ~> y)
    [⨟⨟] : {n : ℕ} {w x y z : O n} →
      (w ~> x) ⊗→ (x ~> y) ⊗ (y ~> z) ⨟ id ⊠ [⨟] ⨟ [⨟] ≡ [⨟] ⊠ id ⨟ [⨟]
    -- id⨟_ : {n : ℕ} {x y : O n} → (f : x => y) → id ⨟ f ≡ f
    -- _⨟id : {n : ℕ} {x y : O n} → (f : x => y) → f ⨟ id ≡ f
    -- _⨟→_⨟_ : {n : ℕ} {w x y z : O n}
    --   (f : w => x) (g : x => y) (h : y => z) →
    --   (f ⨟ g) ⨟ h ≡ f ⨟ g ⨟ h

record _×_ {ℓ₁ ℓ₂} (X : Type ℓ₁) (Y : Type ℓ₂) : Type (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    fst : X
    snd : Y



TypeEnriched : {ℓ : Level} → Enriched (λ _ → Type ℓ)
TypeEnriched = record
  { _⊗_ = _×_
  ; I = ⊤
  ; _~>_ = Fun
  ; I=>_ = λ T → T
  ; ↑I=> = λ f → f tt
  ; ↓I=> = λ x tt → x
  ; ↑↓I=> = λ f → refl
  ; ↓↑I=> = λ f → refl
  ; pair = _,_
  ; ev = λ f x → f x
  ; id = identity
  ; [⨟] = λ (g , f) x → f (g x)
  ; [⊠] = λ (f , g) (x₁ , x₂) → f x₁ , g x₂
  ; evid = refl
  ; ev⨟ = refl
  ; id⊠id = refl
  ; ↓⊗_ = λ _ (tt , x) → x
  ; ↑⊗_ = λ _ x → (tt , x)
  ; _⊗↓ = λ _ (x , tt) → x
  ; _⊗↑ = λ _ x → (x , tt)
  ; _⊗→_⊗_ = λ _ _ _ ((x , y) , z) → (x , (y , z))
  ; _⊗_←⊗_ = λ _ _ _ (x , (y , z)) → ((x , y) , z)
  ; ↑↓⊗_ = λ _ → refl
  ; ↓↑⊗_ = λ _ → refl
  ; _⊗↑↓ = λ _ → refl
  ; _⊗↓↑ = λ _ → refl
  ; _⊗→←_⊗_ = λ _ _ _ → refl
  ; _⊗_→←⊗_ = λ _ _ _ → refl
  ; [id⨟] = refl
  ; [⨟id] = refl
  ; [⨟⨟] = refl
  }

-- module Proves {ℓ₁ ℓ₂} (O : (n : ℕ) → Type ℓ₁) (C : Enriched {ℓ₁} {ℓ₂} O) where
--   open Enriched C
--   I⊗I : {n : ℕ} → ↑⊗ I {n} ≡ I {n} ⊗↑
--   I⊗I = _

record Closing {ℓ₁ ℓ₂} (O : Type ℓ₁) : Type (ℓ₁ ⊔ lsuc ℓ₂) where
  field
    -- Hom-objects
    _~>_ : O → O → O
    -- Identity objects for each level
    I : O
    -- Global object (suggestively named)
    I=> : O → Type ℓ₂
  -- Hom-sets, via the global object
  _=>_ : O → O → Type ℓ₂
  _=>_ = λ x y → I=> (x ~> y)
  infix 30 _~>_
  infix 20 _=>_

  field
    -- Characterize the global objects: (I=> (I ~> x)) ≅ (I=> x)
    ↑I=> : {x : O} → (I => x) -> I=> x
    ↓I=> : {x : O} → I=> x -> (I => x)
    ↑↓I=> : {x : O} (f : I=> x) → ↑I=> (↓I=> f) ≡ f
    ↓↑I=> : {x : O} (f : I => x) → ↓I=> (↑I=> f) ≡ f
    -- Symmetric closed
    [↔] : {x y z : O} → (x ~> (y ~> z)) => (y ~> (x ~> z))
  focus↑ : {x : O} {f : I => x} {g : I=> x} → (f ≡ ↓I=> g) → (↑I=> f ≡ g)
  focus↑ p = focus ↑I=> ⟨ p ⟩ (↑↓I=> _)
  focus↓ : {x : O} {f : I=> x} {g : I => x} → (f ≡ ↑I=> g) → (↓I=> f ≡ g)
  focus↓ p = focus ↓I=> ⟨ p ⟩ (↓↑I=> _)

  field
    -- Identity arrows
    id : {x : O} → x => x
    -- Composition for internal and external homs
    [⨟] : {x y z : O} → (x ~> y) => ((y ~> z) ~> (x ~> z))
    _⨟_ : {x y z : O} → (x => y) → (y => z) → (x => z)
  infixr 20 _⨟_
  [id] : {x : O} → I => (x ~> x)
  [id] = ↓I=> id

  -- Apply an external hom to a global element
  _·_ : {x y : O} → x => y -> I=> x -> I=> y
  _·_ = λ f x → ↑I=> (↓I=> x ⨟ f)
  infixl 20 _·_

  ↔ : {x y z : O} → (x => (y ~> z)) -> (y => (x ~> z))
  ↔ f = [↔] · f

  -- Functorial (?)
  _⟨=>⟩_ : {w x y z : O} → (w => x) -> (y => z) -> (x ~> y) => (w ~> z)
  _⟨=>⟩_ f h = (↔ [⨟] · h) ⨟ ([⨟] · f)

  field
    [id]⨟[⨟] : {x y : O} → [id] ⨟ [⨟] ≡ [id] {x ~> y}
    [id]⨟↔[⨟] : {x y : O} → [id] ⨟ ↔ [⨟] ≡ [id] {x ~> y}
    [↔]⨟[↔] : {x y z : O} →
      (x ~> (y ~> z) => (x ~> (y ~> z))) ∋ [↔] ⨟ [↔] ≡ id
    _⨟id : {x y : O} → (f : x => y) → f ⨟ id ≡ f
    id⨟_ : {x y : O} → (f : x => y) → id ⨟ f ≡ f
    _⨟→_⨟_ : {w x y z : O}
      (f : w => x) (g : x => y) (h : y => z) →
      (f ⨟ g) ⨟ h ≡ f ⨟ g ⨟ h
    agrees : {x y z : O}
      (f : x => y) (g : y => z) → [⨟] · f · g ≡ f ⨟ g
    -- Not sure if this can be derived?
    apswap : {x y z : O}
      (f : x => (y ~> z)) (a : I=> x) (b : I=> y) → ↔ f · b · a ≡ f · a · b

  _⨟id≡_ : {x y : O} → (f : x => y) → {g : y => y} → g ≡ id → f ⨟ g ≡ f
  f ⨟id≡ p = focus (f ⨟_) ⟨ p ⟩ (_ ⨟id)
  _≡id⨟_ : {x y : O} → {g : x => x} → g ≡ id → (f : x => y) → g ⨟ f ≡ f
  p ≡id⨟ f = focus (_⨟ f) ⟨ p ⟩ (id⨟ _)

  id·_ : {x : O} -> (v : I=> x) -> id · v ≡ v
  id· v = focus↑ (↓I=> v ⨟id)

  _≡id·_ : {x : O} -> {f : x => x} -> (f ≡ id) -> (v : I=> x) -> f · v ≡ v
  p ≡id· v = focus↑ (↓I=> v ⨟id≡ p)

  [⨟]·id : {x y : O} -> ([⨟] · id) ≡ id {x ~> y}
  [⨟]·id = focus↑ ([id]⨟[⨟])

  ↔[⨟]·id : {x y : O} -> (↔ [⨟] · id) ≡ id {x ~> y}
  ↔[⨟]·id = focus↑ ([id]⨟↔[⨟])

  ↔↔ : {x y z : O} → (f : x => (y ~> z)) -> ↔ (↔ f) ≡ f
  ↔↔ f =
    ascribe⟨ ↑I=> ((↓I=> (↑I=> ((↓I=> f) ⨟ [↔]))) ⨟ [↔]) ≡ f ⟩
    focus ↑I=>
      -- Cancel ↓↑I=> to get at the composition
      -- (The other ones are not cancelable!)
      -- Reassociate to gather the [↔]s
      ⟨ focus (_⨟ [↔]) ⟨ ↓↑I=> _ ⟩ (_ ⨟→ [↔] ⨟ [↔])
      -- Cancel the now-adjacent [↔]s
      -- And remove the resulting id
      ~ ↓I=> f ⨟id≡ [↔]⨟[↔]
      ⟩
      -- To obtain ↑I=> (↓I=> f), which cancels
      (↑↓I=> f)

  id⟨=>⟩id : {x y : O} →
    ((x ~> y) => (x ~> y)) ∋ id ⟨=>⟩ id ≡ id
  id⟨=>⟩id = _ ⨟id≡ [⨟]·id ~ ↔[⨟]·id
