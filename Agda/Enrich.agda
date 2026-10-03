module Enrich where

open Agda.Primitive renaming (Set to Type; Setω to Typeω)
open import Data.Nat hiding (_⊔_)
open import Relation.Binary.PropositionalEquality.Core
  using (_≡_; _≢_; refl; sym; cong; cong₂; trans; subst)
open import Relation.Binary.PropositionalEquality.Properties
  using (trans-reflʳ; trans-assoc)
open import Data.Unit.Polymorphic.Base
  using (⊤; tt)
open import Function.Base
  using (_$_)

record _×_ {ℓ₁ ℓ₂} (X : Type ℓ₁) (Y : Type ℓ₂) : Type (ℓ₁ ⊔ ℓ₂) where
  constructor _,_
  field
    fst : X
    snd : Y

record [,] {ℓ} (T : Type ℓ) : Type ℓ where
  constructor [_,_]
  field
    t0 : T
    t1 : T
record [,,] {ℓ} (T : Type ℓ) : Type ℓ where
  constructor [_,_,_]
  field
    t0 : T
    t1 : T
    t2 : T

record IsoLike {ℓ₁ ℓ₂} {O : Type ℓ₁} (Hom : O → O → Type ℓ₂) (IsInv : {U V : O} → Hom U V → Hom V U → Type ℓ₂) (X Y : O) : Type ℓ₂ where
  field
    f : Hom X Y
    g : Hom Y X
    fwd : IsInv f g
    bwd : IsInv g f


Fun : {ℓ : Level} → Type ℓ → Type ℓ → Type ℓ
Fun x y = x → y

identity : {ℓ : Level} {X : Type ℓ} → Fun X X
identity x = x

-- Focus a subproblem
-- (I will use ⟨_⟩ for tactic-like functions in this module)
focus_⟨_⟩ : {ℓ : Level} {T₁ T₂ : Type ℓ} {x y : T₁} →
  (f : T₁ → T₂) → (x ≡ y) → (f x ≡ f y)
focus f ⟨ p ⟩ = cong f p

-- Transitivity
_~_ : {ℓ : Level} {T : Type ℓ} {x y z : T} →
  (x ≡ y) → (y ≡ z) → (x ≡ z)
_~_ = trans

infixr 3 _~_

-- Use a section–retract pair to cancel the injective function f
around_⟨_⟩ : {ℓ : Level} {T₁ T₂ : Type ℓ} {x y : T₁}
  ((f , g) : (T₁ → T₂) × (T₂ → T₁))
  (prf : (i : T₁) → g (f i) ≡ i) →
  (f x ≡ f y) → (x ≡ y)
around_⟨_⟩ {x = x} {y = y} (f , g) (prf) q = sym (prf x) ~ cong g q ~ prf y

-- Type ascription
ascribe⟨_⟩_ : {ℓ : Level} (T : Type ℓ) → T → T
ascribe⟨ T ⟩ t = t

infixr 1 ascribe⟨_⟩_

-- Type ascription for equalities
_∋_≡_ : {ℓ : Level} (T : Type ℓ) → T → T → Type ℓ
T ∋ x ≡ y = x ≡ y

-- Boring type-enriched category
record TypeCat {ℓ₁ ℓ₂} (Ob : Type ℓ₁) (Hom : Ob → Ob → Type ℓ₂) : Type (ℓ₁ ⊔ ℓ₂) where
  field
    id : {x : Ob} → Hom x x
    _⨾_ : {x y z : Ob} → Hom x y → Hom y z → Hom x z
    lid : {x y : Ob} (f : Hom x y) → id ⨾ f ≡ f
    rid : {x y : Ob} (f : Hom x y) → f ⨾ id ≡ f
    assoc : {m n p q : Ob} (f : Hom m n) (g : Hom n p) (h : Hom p q) → (f ⨾ g) ⨾ h ≡ f ⨾ (g ⨾ h)
  infixr 40 _⨾_
  record Iso (x y : Ob) : Type ℓ₂ where
    field
      f : Hom x y
      g : Hom y x
      fwd : f ⨾ g ≡ id
      bwd : g ⨾ f ≡ id

-- Category of functions
FunTypeCat : {ℓ : Level} → TypeCat (Type ℓ) Fun
FunTypeCat = record
  { id = λ x → x
  ; _⨾_ = λ f g x → g (f x)
  ; lid = λ _ → refl
  ; rid = λ _ → refl
  ; assoc = λ _ _ _ → refl
  }

-- Groupoid of equalities
EqTypeCat : {ℓ : Level} {T : Type ℓ} → TypeCat T (_≡_ {ℓ} {T})
EqTypeCat = record
  { id = refl
  ; _⨾_ = λ f g → f ~ g
  ; lid = λ _ → refl
  ; rid = λ f → trans-reflʳ f
  ; assoc = λ f _ _ → trans-assoc f
  }

-- Boring type-enriched monoidal category
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
    ⨾mon : {m n o p q r : Ob} (mn : Hom m n) (no : Hom n o) (pq : Hom p q) (qr : Hom q r) →
      mon mn pq ⨾ mon no qr ≡ mon (mn ⨾ no) (pq ⨾ qr)
  -- open TypeCat.Iso lI renaming (f to lIf; g to lIg)
  -- open TypeCat.Iso rI renaming (f to rIf; g to rIg)
  -- open TypeCat.Iso MM renaming (f to MMf; g to MMg)

-- Boring enriched category (enriched in a boring type-enriched monoidal category)
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
    lid : {x y : Ob} → mon id (ID {Hom x y}) ⨾ comp ≡ Iso.f (lI {Hom x y})
    rid : {x y : Ob} → mon (ID {Hom x y}) id ⨾ comp ≡ Iso.f (rI {Hom x y})
    assoc : {m n p q : Ob} → Iso.f MM ⨾ mon (ID {Hom m n}) (comp {n} {p} {q}) ⨾ comp ≡ mon (comp {m} {n} {p}) (ID {Hom p q}) ⨾ comp

-- An infinitely enriched category?
-- O is the type of objects for each level, used to index the morphisms
-- Each category is enriched in the next
record Enriched {ℓ₁ ℓ₂} (O : (n : ℕ) → Type ℓ₁) : Type (ℓ₁ ⊔ lsuc ℓ₂) where
  field
    -- Monoidal tensor for each level
    _⊗_ : {n : ℕ} → O n → O n → O n
    -- Identity objects for each level
    I : {n : ℕ} → O n
    -- Hom-objects: the category is enriched in the next
    -- (if this resulted in `O n` it would be a closed category)
    _~>_ : {n : ℕ} → O n → O n → O (suc n)
    -- Global object (suggestively named)
    I=>_ : {n : ℕ} → O n → Type ℓ₂
  -- Hom-sets, via the global object
  -- Notice how it is `I=>_` from the next level
  _=>_ : {n : ℕ} → O n → O n → Type ℓ₂
  _=>_ {n} = λ x y → I=>_ {n = suc n} (x ~> y)
  -- Declare some precedence
  infixr 50 _⊗_
  infix 30 _~>_
  infix 20 _=>_
  infix 20 I=>_

  -- Make it into a monoidal category
  field
    -- Identity arrows
    id : {n : ℕ} {x : O n} → x => x
    -- Composition
    [⨾] : {n : ℕ} {x y z : O n} → ((x ~> y) ⊗ (y ~> z)) => (x ~> z)
    -- Functorial action of tensor
    [⊠] : {n : ℕ} {x₁ x₂ y₁ y₂ : O n} → (x₁ ~> y₁) ⊗ (x₂ ~> y₂) => (x₁ ⊗ x₂ ~> y₁ ⊗ y₂)
    -- Evaluating global morphism at global element
    -- a.k.a. functoriality of I=>
    ev : {n : ℕ} {x y : O (suc n)} → x => y → I=> x → I=> y
    -- Pairing global elements
    pair : {n : ℕ} {x y : O (suc n)} → I=> x → I=> y → I=> (x ⊗ y)
    -- Characterize the global objects: (I=> (I ~> x)) ≅ (I=> x)
    ↑I=> : {n : ℕ} {x : O n} → (I => x) -> I=> x
    ↓I=> : {n : ℕ} {x : O n} → I=> x -> (I => x)
  -- Identity morphism at an explicit object
  at : {n : ℕ} (x : O n) → x => x
  at x = id {x = x}
  -- External composition
  _⨾_ : {n : ℕ} {x y z : O n} → (x => y) → (y => z) → (x => z)
  _⨾_ = λ f g → ev [⨾] (pair f g)
  infixr 20 _⨾_
  -- External tensor
  _⊠_ : {n : ℕ} {x₁ x₂ y₁ y₂ : O n} → (x₁ => y₁) → (x₂ => y₂) → (x₁ ⊗ x₂ => y₁ ⊗ y₂)
  _⊠_ = λ f g → ev [⊠] (pair f g)
  infixr 50 _⊠_
  -- Iso-sets
  _<=>_ : {n : ℕ} (x y : O n) → Type ℓ₂
  _<=>_ {n} = IsoLike (_=>_) λ f g → f ⨾ g ≡ id
  infix 20 _<=>_
  -- Internal identity morphism
  [id] : {n : ℕ} {x : O n} → I => (x ~> x)
  [id] = ↓I=> id

  -- Isomorphisms
  field
    -- Tensor identity
    ↓⊗_ : {n : ℕ} (x : O n) → I ⊗ x => x
    ↑⊗_ : {n : ℕ} (x : O n) → x => I ⊗ x
    _⊗↓ : {n : ℕ} (x : O n) → x ⊗ I => x
    _⊗↑ : {n : ℕ} (x : O n) → x => x ⊗ I
    -- Tensor assoc
    _⊗→_⊗_ : {n : ℕ} (x y z : O n) → (x ⊗ y) ⊗ z => x ⊗ (y ⊗ z)
    _⊗_←⊗_ : {n : ℕ} (x y z : O n) → x ⊗ (y ⊗ z) => (x ⊗ y) ⊗ z
  infixr 80 ↓⊗_
  infixr 80 ↑⊗_
  infixl 81 _⊗↓
  infixl 81 _⊗↑
  infix 70 _⊗→_⊗_
  infix 70 _⊗_←⊗_
  ⊗→⊗ : {n : ℕ} {x y z : O n} → (x ⊗ y) ⊗ z => x ⊗ (y ⊗ z)
  ⊗→⊗ = _ ⊗→ _ ⊗ _
  ⊗←⊗ : {n : ℕ} {x y z : O n} → x ⊗ (y ⊗ z) => (x ⊗ y) ⊗ z
  ⊗←⊗ = _ ⊗ _ ←⊗ _

  -- Properties of internal and external composition
  field
    [id⨾] : {n : ℕ} {x y : O n} → id ⊠ [id] ⨾ [⨾] ≡ (x ~> y)⊗↓
    [⨾id] : {n : ℕ} {x y : O n} → [id] ⊠ id ⨾ [⨾] ≡ ↓⊗(x ~> y)
    [⨾⨾] : {n : ℕ} {w x y z : O n} →
      (w ~> x) ⊗→ (x ~> y) ⊗ (y ~> z) ⨾ id ⊠ [⨾] ⨾ [⨾] ≡ [⨾] ⊠ id ⨾ [⨾]
    id⨾_ : {n : ℕ} {x y : O n} (f : x => y) → id ⨾ f ≡ f
    _⨾id : {n : ℕ} {x y : O n} (f : x => y) → f ⨾ id ≡ f
    _⨾→_⨾_ : {n : ℕ} {w x y z : O n}
      (f : w => x) (g : x => y) (h : y => z) →
      (f ⨾ g) ⨾ h ≡ f ⨾ g ⨾ h
  _⨾_←⨾_ : {n : ℕ} {w x y z : O n}
    (f : w => x) (g : x => y) (h : y => z) →
    f ⨾ g ⨾ h ≡ (f ⨾ g) ⨾ h
  _⨾_←⨾_ f g h = sym (f ⨾→ g ⨾ h)
  ⨾→⨾ : {n : ℕ} {w x y z : O n}
    {f : w => x} {g : x => y} {h : y => z} →
    (f ⨾ g) ⨾ h ≡ f ⨾ g ⨾ h
  ⨾→⨾ = _ ⨾→ _ ⨾ _
  ⨾←⨾ : {n : ℕ} {w x y z : O n}
    {f : w => x} {g : x => y} {h : y => z} →
    f ⨾ g ⨾ h ≡ (f ⨾ g) ⨾ h
  ⨾←⨾ = _ ⨾ _ ←⨾ _

  -- Properties of evaluation and pairing
  field
    evid : {n : ℕ} {x : O (suc n)} {v : I=> x} → ev (id {suc n} {x}) v ≡ v
    ev⨾ : {n : ℕ} {x y z : O (suc n)} {f : x => y} {g : y => z} {v : I=> x} → ev (f ⨾ g) v ≡ ev g (ev f v)
    id⊠id : {n : ℕ} {x₀ x₁ : O n} → id ⊠ id ≡ id {n} {x₀ ⊗ x₁}
    ⨾⊠⨾ : {n : ℕ} {x₀ y₀ z₀ x₁ y₁ z₁ : O n} {f₀ : x₀ => y₀} {f₁ : y₀ => z₀} {g₀ : x₁ => y₁} {g₁ : y₁ => z₁} →
      (f₀ ⊠ g₀) ⨾ (f₁ ⊠ g₁) ≡ (f₀ ⨾ f₁) ⊠ (g₀ ⨾ g₁)

  -- Properties of structure
  field
    ↑↓I=> : {n : ℕ} {x : O n} (f : I=> x) → ↑I=> (↓I=> f) ≡ f
    ↓↑I=> : {n : ℕ} {x : O n} (f : I => x) → ↓I=> (↑I=> f) ≡ f
    -- Isomorphisms
    ↑↓⊗_ : {n : ℕ} (x : O n) → ↑⊗ x ⨾ ↓⊗ x ≡ id
    ↓↑⊗_ : {n : ℕ} (x : O n) → ↓⊗ x ⨾ ↑⊗ x ≡ id
    _⊗↑↓ : {n : ℕ} (x : O n) → x ⊗↑ ⨾ x ⊗↓ ≡ id
    _⊗↓↑ : {n : ℕ} (x : O n) → x ⊗↓ ⨾ x ⊗↑ ≡ id
    _⊗→←_⊗_ : {n : ℕ} (x y z : O n) → x ⊗→ y ⊗ z ⨾ x ⊗ y ←⊗ z ≡ id
    _⊗_→←⊗_ : {n : ℕ} (x y z : O n) → x ⊗ y ←⊗ z ⨾ x ⊗→ y ⊗ z ≡ id
    -- Commutative diagrams
    triangle→ : {n : ℕ} {x y : O n} →
      (((x ⊗ I) ⊗ y) => (x ⊗ y)) ∋ ((x ⊗→ I ⊗ y) ⨾ (id ⊠ ↓⊗ y)) ≡ (x ⊗↓ ⊠ id)
    pentagon→ : {n : ℕ} {w x y z : O n} →
      (w ⊗→ x ⊗ y) ⊠ id  ⨾  w ⊗→ (x ⊗ y) ⊗ z  ⨾  id ⊠ (x ⊗→ y ⊗ z)   ≡   (w ⊗ x) ⊗→ y ⊗ z  ⨾  w ⊗→ x ⊗ (y ⊗ z)
    -- Naturality
    ₙ↓⊠_ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → (at I ⊠ f) ⨾ ↓⊗ x₁ ≡ ↓⊗ x₀ ⨾ f
    ₙ↑⊠_ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → ↑⊗ x₀ ⨾ (at I ⊠ f) ≡ f ⨾ ↑⊗ x₁
    ₙ_⊠↓ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → (f ⊠ at I) ⨾ x₁ ⊗↓ ≡ x₀ ⊗↓ ⨾ f
    ₙ_⊠↑ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → x₀ ⊗↑ ⨾ (f ⊠ at I) ≡ f ⨾ x₁ ⊗↑
    ₙ_⊠→_⊠_ : {n : ℕ} {x₀ y₀ z₀ x₁ y₁ z₁ : O n} (f : x₀ => x₁) (g : y₀ => y₁) (h : z₀ => z₁) →
      (f ⊠ g) ⊠ h ⨾ x₁ ⊗→ y₁ ⊗ z₁  ≡  x₀ ⊗→ y₀ ⊗ z₀ ⨾ f ⊠ (g ⊠ h)
    ₙ_⊠_←⊠_ : {n : ℕ} {x₀ y₀ z₀ x₁ y₁ z₁ : O n} (f : x₀ => x₁) (g : y₀ => y₁) (h : z₀ => z₁) →
      f ⊠ (g ⊠ h) ⨾ x₁ ⊗ y₁ ←⊗ z₁  ≡  x₀ ⊗ y₀ ←⊗ z₀ ⨾ (f ⊠ g) ⊠ h
  -- Symmetry of naturality (u for un-naturality)
  ᵤ↓⊠_ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → ↓⊗ x₀ ⨾ f ≡ (at I ⊠ f) ⨾ ↓⊗ x₁
  ᵤ↓⊠_ f = sym (ₙ↓⊠_ f)
  ᵤ↑⊠_ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → f ⨾ ↑⊗ x₁ ≡ ↑⊗ x₀ ⨾ (at I ⊠ f)
  ᵤ↑⊠_ f = sym (ₙ↑⊠_ f)
  ᵤ_⊠↓ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → x₀ ⊗↓ ⨾ f ≡ (f ⊠ at I) ⨾ x₁ ⊗↓
  ᵤ_⊠↓ f = sym (ₙ_⊠↓ f)
  ᵤ_⊠↑ : {n : ℕ} {x₀ x₁ : O n} (f : x₀ => x₁) → f ⨾ x₁ ⊗↑ ≡ x₀ ⊗↑ ⨾ (f ⊠ at I)
  ᵤ_⊠↑ f = sym (ₙ_⊠↑ f)
  ᵤ_⊠→_⊠_ : {n : ℕ} {x₀ y₀ z₀ x₁ y₁ z₁ : O n} (f : x₀ => x₁) (g : y₀ => y₁) (h : z₀ => z₁) →
    x₀ ⊗→ y₀ ⊗ z₀ ⨾ f ⊠ (g ⊠ h)  ≡  (f ⊠ g) ⊠ h ⨾ x₁ ⊗→ y₁ ⊗ z₁
  ᵤ_⊠→_⊠_ f g h = sym (ₙ_⊠→_⊠_ f g h)
  ᵤ_⊠_←⊠_ : {n : ℕ} {x₀ y₀ z₀ x₁ y₁ z₁ : O n} (f : x₀ => x₁) (g : y₀ => y₁) (h : z₀ => z₁) →
    x₀ ⊗ y₀ ←⊗ z₀ ⨾ (f ⊠ g) ⊠ h  ≡  f ⊠ (g ⊠ h) ⨾ x₁ ⊗ y₁ ←⊗ z₁
  ᵤ_⊠_←⊠_ f g h = sym (ₙ_⊠_←⊠_ f g h)




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
  ; [⨾] = λ (g , f) x → f (g x)
  ; [⊠] = λ (f , g) (x₁ , x₂) → f x₁ , g x₂
  ; evid = refl
  ; ev⨾ = refl
  ; id⊠id = refl
  ; ⨾⊠⨾ = refl
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
  ; [id⨾] = refl
  ; [⨾id] = refl
  ; [⨾⨾] = refl
  ; id⨾_ = λ _ → refl
  ; _⨾id = λ _ → refl
  ; _⨾→_⨾_ = λ _ _ _ → refl
  ; triangle→ = refl -- uses eta
  ; pentagon→ = refl -- uses eta
  ; ₙ↓⊠_ = λ f → refl
  ; ₙ↑⊠_ = λ f → refl
  ; ₙ_⊠↓ = λ f → refl
  ; ₙ_⊠↑ = λ f → refl
  ; ₙ_⊠→_⊠_ = λ f g h → refl
  ; ₙ_⊠_←⊠_ = λ f g h → refl
  }

-- Proofs for the infinitely enriched category
module Proves {ℓ₁ ℓ₂} (O : (n : ℕ) → Type ℓ₁) (C : Enriched {ℓ₁} {ℓ₂} O) where
  open Enriched C

  -- Start with a bunch of tactic-like combinators

  -- Focus on (apply a proof to) the left side of ⨾
  focus⨾⟨_⟩ : {n : ℕ} {x y z : O n} {f₀ f₁ : x => y} {g : y => z} →
    (f₀ ≡ f₁) → (f₀ ⨾ g ≡ f₁ ⨾ g)
  focus⨾⟨ p ⟩ = focus (_⨾ _) ⟨ p ⟩

  -- Focus on (apply a proof to) the right side of ⨾
  ⨾focus⟨_⟩ : {n : ℕ} {x y z : O n} {f : x => y} {g₀ g₁ : y => z} →
    (g₀ ≡ g₁) → (f ⨾ g₀ ≡ f ⨾ g₁)
  ⨾focus⟨ p ⟩ = focus (_ ⨾_) ⟨ p ⟩

  -- Split into separate proofs for left and right sides of ⨾
  split⟨_⟩⨾⟨_⟩ : {n : ℕ} {x y z : O n} {f₀ f₁ : x => y} {g₀ g₁ : y => z} →
    (f₀ ≡ f₁) → (g₀ ≡ g₁) → (f₀ ⨾ g₀ ≡ f₁ ⨾ g₁)
  split⟨ p ⟩⨾⟨ q ⟩ = cong₂ (_⨾_) p q

  -- Remove the left side of ⨾
  del⨾⟨_⟩ : {n : ℕ} {x y : O n} {f : x => x} {g : x => y} →
    (f ≡ id) → (f ⨾ g ≡ g)
  del⨾⟨ p ⟩ = focus⨾⟨ p ⟩ ~ id⨾ _

  -- Remove the right side of ⨾
  ⨾del⟨_⟩ : {n : ℕ} {x y : O n} {f : x => y} {g : y => y} →
    (g ≡ id) → (f ⨾ g ≡ f)
  ⨾del⟨ p ⟩ = ⨾focus⟨ p ⟩ ~ _ ⨾id

  -- Swap left composition for its inverse on the other side of ≡
  inv⨾⟨_⟩ : {n : ℕ} {x y z : O n} {f : x => y} {g : y => x} {h : y => z} {r : x => z} →
    (g ⨾ f ≡ id) →
    (f ⨾ h ≡ r) →
    (h ≡ g ⨾ r)
  inv⨾⟨ p ⟩ q = sym del⨾⟨ p ⟩ ~ ⨾→⨾ ~ ⨾focus⟨ q ⟩

  -- Swap right composition for its inverse on the other side of ≡
  ⨾inv⟨_⟩ : {n : ℕ} {x y z : O n} {f : y => z} {g : z => y} {h : x => y} {r : x => z} →
    (f ⨾ g ≡ id) →
    (h ⨾ f ≡ r) →
    (h ≡ r ⨾ g)
  ⨾inv⟨ p ⟩ q = sym ⨾del⟨ p ⟩ ~ ⨾←⨾ ~ focus⨾⟨ q ⟩

  -- Cancel left composition via a section–retract pair
  cancel⨾_⟨_⟩ : {n : ℕ} {x y : O n} ((f , g) : (x => y) × (y => x)) →
    (g ⨾ f ≡ id) → {z : O n} → {h₀ h₁ : y => z} →
    (f ⨾ h₀ ≡ f ⨾ h₁) →
    (h₀ ≡ h₁)
  cancel⨾ (f , g) ⟨ p ⟩ = around
    ((λ m → f ⨾ m) , (λ m → g ⨾ m))
    ⟨(λ m → ⨾←⨾ ~ del⨾⟨ p ⟩)⟩

  -- Cancel right composition via a section–retract pair
  ⨾cancel_⟨_⟩ : {n : ℕ} {y z : O n} ((f , g) : (y => z) × (z => y)) →
    (f ⨾ g ≡ id) → {x : O n} → {h₀ h₁ : x => y} →
    (h₀ ⨾ f ≡ h₁ ⨾ f) →
    (h₀ ≡ h₁)
  ⨾cancel (f , g) ⟨ p ⟩ = around
    ((λ m → m ⨾ f) , (λ m → m ⨾ g))
    ⟨(λ m → ⨾→⨾ ~ ⨾del⟨ p ⟩)⟩


  focus⊠⟨_⟩ : {n : ℕ} {w x y z : O n} {f₀ f₁ : w => x} {g : y => z} →
    (f₀ ≡ f₁) → (f₀ ⊠ g ≡ f₁ ⊠ g)
  focus⊠⟨ p ⟩ = focus (_⊠ _) ⟨ p ⟩

  ⊠focus⟨_⟩ : {n : ℕ} {w x y z : O n} {f : w => x} {g₀ g₁ : y => z} →
    (g₀ ≡ g₁) → (f ⊠ g₀ ≡ f ⊠ g₁)
  ⊠focus⟨ p ⟩ = focus (_ ⊠_) ⟨ p ⟩

  split⟨_⟩⊠⟨_⟩ : {n : ℕ} {w x y z : O n} {f₀ f₁ : w => x} {g₀ g₁ : y => z} →
    (f₀ ≡ f₁) → (g₀ ≡ g₁) → (f₀ ⊠ g₀ ≡ f₁ ⊠ g₁)
  split⟨ p ⟩⊠⟨ q ⟩ = cong₂ (_⊠_) p q


  atI⊠_ : {n : ℕ} {x y : O n} (f : x => y) →
    ↓⊗ x ⨾ f ⨾ ↑⊗ y  ≡  at I ⊠ f
  atI⊠ f = ⨾focus⟨ ᵤ↑⊠ f ⟩ ~ (_ ⨾ _ ←⨾ _) ~ del⨾⟨ ↓↑⊗ _ ⟩

  _⊠atI : {n : ℕ} {x y : O n} (f : x => y) →
    x ⊗↓ ⨾ f ⨾ y ⊗↑  ≡  f ⊠ at I
  f ⊠atI = ⨾focus⟨ ᵤ f ⊠↑ ⟩ ~ (_ ⨾ _ ←⨾ _) ~ del⨾⟨ _ ⊗↓↑ ⟩

  -- Cancel (at I ⊠_) out of a proof
  atI⊠⟨_⟩ : {n : ℕ} {x y : O n} {f₀ f₁ : x => y} →
    at I ⊠ f₀ ≡ at I ⊠ f₁ → f₀ ≡ f₁
  atI⊠⟨ p ⟩ = around ((λ m → at I ⊠ m) , (λ m → ↑⊗ _ ⨾ m ⨾ ↓⊗ _))
    ⟨ (λ m →
      cong (λ m → ↑⊗ _ ⨾ m ⨾ ↓⊗ _) (sym (atI⊠ _)) ~
      ⨾focus⟨
    -- d ; ((u ; (m ; u)) ; d)
        focus⨾⟨ _ ⨾ m ←⨾ _ ⟩
    -- d ; (((u ; m) ; u) ; d)
        ~ (_ ⨾→ _ ⨾ _)
    -- d ; ((u ; m) ; (u ; d))
        ~ ⨾del⟨ ↑↓⊗ _ ⟩
    -- d ; (u ; m)
      ⟩
      ~ (_ ⨾ _ ←⨾ m)
    -- (d ; u) ; m
      ~ del⨾⟨ ↑↓⊗ _ ⟩
    -- m
    ) ⟩ p

  -- Cancel (_⊠ at I) out of a proof
  ⊠atI⟨_⟩ : {n : ℕ} {x y : O n} {f₀ f₁ : x => y} →
    f₀ ⊠ at I ≡ f₁ ⊠ at I → f₀ ≡ f₁
  ⊠atI⟨ p ⟩ = around ((λ m → m ⊠ at I) , (λ m → _ ⊗↑ ⨾ m ⨾ _ ⊗↓))
    ⟨ (λ m →
      cong (λ m → _ ⊗↑ ⨾ m ⨾ _ ⊗↓) (sym (m ⊠atI)) ~
      ⨾focus⟨
    -- d ; ((u ; (m ; u)) ; d)
        focus⨾⟨ _ ⨾ m ←⨾ _ ⟩
    -- d ; (((u ; m) ; u) ; d)
        ~ (_ ⨾→ _ ⨾ _)
    -- d ; ((u ; m) ; (u ; d))
        ~ ⨾del⟨ _ ⊗↑↓ ⟩
    -- d ; (u ; m)
      ⟩
      ~ (_ ⨾ _ ←⨾ m)
    -- (d ; u) ; m
      ~ del⨾⟨ _ ⊗↑↓ ⟩
    -- m
    ) ⟩ p

  ⨾⊠id : {n : ℕ} {x y z o : O n} {f : x => y} {g : y => z} →
    (f ⊠ at o) ⨾ (g ⊠ at o) ≡ (f ⨾ g) ⊠ at o
  ⨾⊠id = ⨾⊠⨾ ~ ⊠focus⟨ id⨾ _ ⟩

  id⊠⨾ : {n : ℕ} {x y z o : O n} {f : x => y} {g : y => z} →
    (at o ⊠ f) ⨾ (at o ⊠ g) ≡ at o ⊠ (f ⨾ g)
  id⊠⨾ = ⨾⊠⨾ ~ focus⊠⟨ id⨾ _ ⟩

  [id⨾]aux1 : {n : ℕ} {x y : O n} → (id ⊠ [id] ⨾ [⨾]) ⨾ (x ~> y) ⊗↑ ≡ id
  [id⨾]aux1 {n} {x} {y} = cong (λ m → m ⨾ _ ⊗↑) [id⨾] ~ (x ~> y) ⊗↓↑
  [⨾id]aux1 : {n : ℕ} {x y : O n} → ([id] ⊠ id ⨾ [⨾]) ⨾ ↑⊗ (x ~> y) ≡ id
  [⨾id]aux1 {n} {x} {y} = cong (λ m → m ⨾ ↑⊗ _) [⨾id] ~ ↓↑⊗ (x ~> y)

  [id⨾]aux2 : {n : ℕ} {x y : O n} → id ⊠ [id] ⨾ [⨾] ⨾ (x ~> y) ⊗↑ ≡ id
  [id⨾]aux2 {n} {x} {y} = ⨾←⨾ ~ [id⨾]aux1
  [⨾id]aux2 : {n : ℕ} {x y : O n} → [id] ⊠ id ⨾ [⨾] ⨾ ↑⊗ (x ~> y) ≡ id
  [⨾id]aux2 {n} {x} {y} = ⨾←⨾ ~ [⨾id]aux1

  -- Relate left unitors (↓⊗ (x ⊗ y)) and ((↓⊗ x) ⊠ at y) using the associator
  triangle2 : {n : ℕ} {x y : O n} → I ⊗→ x ⊗ y ⨾ ↓⊗ (x ⊗ y) ≡ (↓⊗ x) ⊠ at y
  triangle2 {n} {x} {y} = sym step7
    where

    -- Paste two

    step1-1 : (I ⊗→ I ⊗ x) ⊠ at y ⨾ (at I ⊠ ↓⊗ x) ⊠ at y ≡ (I ⊗↓ ⊠ at x) ⊠ at y
    step1-1 = ⨾⊠id ~ focus⊠⟨ triangle→ ⟩
    step1-2 : (at I ⊠ ↓⊗ x) ⊠ at y ⨾ ⊗→⊗  ≡  ⊗→⊗ ⨾ at I ⊠ (↓⊗ x ⊠ at y)
    step1-2 = ₙ (at I) ⊠→ (↓⊗ x) ⊠ (at y)
    -- Naturality of associativity with a lifted triangle, glued along ((at I ⊠ ↓⊗ x) ⊠ at y)
    -- (The shared path zig-zags, so they both gain a term at opposite ends)
    step1   : ((I ⊗↓) ⊠ at x) ⊠ at y ⨾ ⊗→⊗  ≡  (⊗→⊗ ⊠ at y) ⨾ ⊗→⊗ ⨾ at I ⊠ (↓⊗ x ⊠ at y)
    step1   = focus⨾⟨ sym step1-1 ⟩ ~ ⨾→⨾ ~ ⨾focus⟨ step1-2 ⟩

    step2-1 : ((I ⊗↓) ⊠ _) ⊠ _ ⨾ ⊗→⊗  ≡  ⊗→⊗ ⨾ (I ⊗↓) ⊠ (at x ⊠ at y)
    step2-1 = ₙ (I ⊗↓) ⊠→ (at x) ⊠ (at y)
    step2-2 : (I ⊗↓) ⊠ (at x ⊠ at y)  ≡  (I ⊗→ I ⊗ (x ⊗ y)) ⨾ id ⊠ (↓⊗ (x ⊗ y))
    step2-2 = ⊠focus⟨ id⊠id ⟩ ~ sym triangle→
    -- Naturality of associativity with a triangle, glued along ((I ⊗↓) ⊠ (at x ⊠ at y))
    step2   : ((I ⊗↓) ⊠ _) ⊠ _ ⨾ ⊗→⊗  ≡  ⊗→⊗ ⨾ (I ⊗→ I ⊗ (x ⊗ y)) ⨾ id ⊠ (↓⊗ (x ⊗ y))
    step2   = step2-1 ~ ⨾focus⟨ step2-2 ⟩

    -- Add the pentagon onto step2, with the unitor (id ⊠ (↓⊗ (x ⊗ y))) trailing
    step3   : ((I ⊗↓) ⊠ at x) ⊠ at y ⨾ ⊗→⊗  ≡  (⊗→⊗ ⊠ at y) ⨾ ⊗→⊗ ⨾ (at I ⊠ ⊗→⊗) ⨾ id ⊠ (↓⊗ (x ⊗ y))
    step3   = step2 ~ ⨾←⨾ ~ focus⨾⟨ sym pentagon→ ⟩ ~ ⨾→⨾ ~ ⨾focus⟨ ⨾→⨾ ⟩

    -- They now share (((I ⊗↓) ⊠ at x) ⊠ at y ⨾ ⊗→⊗)
    step4 = sym step1 ~ step3
    -- Cancel the associator isomorphisms off of the front
    step5 = cancel⨾ ((⊗→⊗ ⊠ at y) , (⊗←⊗ ⊠ at y))
      ⟨(⨾⊠⨾ ~ split⟨ _ ⊗ _ →←⊗ _ ⟩⊠⟨ id⨾ _ ⟩ ~ id⊠id)⟩ step4
    step6 : at I ⊠ (↓⊗ x ⊠ at y)  ≡  (at I ⊠ ⊗→⊗) ⨾ id ⊠ (↓⊗ (x ⊗ y))
    step6 = cancel⨾ _ ⟨ _ ⊗ _ →←⊗ _ ⟩ step5

    -- Cancel the identity tensoring (at I ⊠_)
    step7 = atI⊠⟨ step6 ~ id⊠⨾ {o = I} ⟩

  lassoc : {n : ℕ} {x y : O n} → (I ⊗ x ←⊗ y) ⨾ (↓⊗ x) ⊠ at y ≡ ↓⊗ (x ⊗ y)
  lassoc = sym $ inv⨾⟨ _ ⊗ _ →←⊗ _ ⟩ triangle2


  id⊠↓⊗ : {n : ℕ} {x : O n} → id {n} ⊠ (↓⊗ x) ≡ (↓⊗ (I ⊗ x))
  id⊠↓⊗ = ⨾cancel ((↓⊗ _) , (↑⊗ _)) ⟨ ↓↑⊗ _ ⟩ (ₙ↓⊠ _)

  -- The unitors agree at the identity
  ↓⊗↓ : {n : ℕ} → ↓⊗ I {n} ≡ I {n} ⊗↓
  ↓⊗↓ {n} = ⊠atI⟨ sym triangle2 ~ ⨾focus⟨ sym id⊠↓⊗ ⟩ ~ triangle→ ⟩

  -- So their inverses do too
  ↑⊗↑ : {n : ℕ} → ↑⊗ I {n} ≡ I {n} ⊗↑
  ↑⊗↑ {n} = sym ⨾del⟨ I ⊗↓↑ ⟩ ~ cong (λ m → ↑⊗ I ⨾ m ⨾ I ⊗↑) (sym ↓⊗↓) ~ ⨾←⨾ ~ del⨾⟨ ↑↓⊗ I ⟩


-- A closed category is self-enriched. Maybe this simplifies things a bit ...
record Closed {ℓ₁ ℓ₂} (O : Type ℓ₁) : Type (ℓ₁ ⊔ lsuc ℓ₂) where
  field
    -- Hom-objects (inner hom)
    _~>_ : O → O → O
    -- Identity objects for each level
    I : O
    -- Global object (suggestively named)
    I=>_ : O → Type ℓ₂
  -- Hom-sets, via the global object
  _=>_ : O → O → Type ℓ₂
  _=>_ = λ x y → I=> (x ~> y)
  infix 30 _~>_
  infix 20 _=>_
  infix 20 I=>_

  field
    -- Characterize the global objects: (I=> (I ~> x)) ≅ (I=> x)
    ↑I=> : {x : O} → (I => x) -> I=> x
    ↓I=> : {x : O} → I=> x -> (I => x)
    ↑↓I=> : {x : O} (f : I=> x) → ↑I=> (↓I=> f) ≡ f
    ↓↑I=> : {x : O} (f : I => x) → ↓I=> (↑I=> f) ≡ f
    -- Symmetric closed, you can swap arguments in the inner hom
    [↔] : {x y z : O} → (x ~> (y ~> z)) => (y ~> (x ~> z))
  focus↑ : {x : O} {f : I => x} {g : I=> x} → (f ≡ ↓I=> g) → (↑I=> f ≡ g)
  focus↑ p = focus ↑I=> ⟨ p ⟩ ~ (↑↓I=> _)
  focus↓ : {x : O} {f : I=> x} {g : I => x} → (f ≡ ↑I=> g) → (↓I=> f ≡ g)
  focus↓ p = focus ↓I=> ⟨ p ⟩ ~ (↓↑I=> _)

  field
    -- Identity arrows
    id : {x : O} → x => x
    -- Composition for internal and external homs
    [⨾] : {x y z : O} → (x ~> y) => ((y ~> z) ~> (x ~> z))
    _⨾_ : {x y z : O} → (x => y) → (y => z) → (x => z)
  infixr 20 _⨾_
  [id] : {x : O} → I => (x ~> x)
  [id] = ↓I=> id

  -- Apply an external hom to a global element
  _·_ : {x y : O} → x => y -> I=> x -> I=> y
  _·_ = λ f x → ↑I=> (↓I=> x ⨾ f)
  infixl 20 _·_

  ↔ : {x y z : O} → (x => (y ~> z)) -> (y => (x ~> z))
  ↔ f = [↔] · f

  -- Functorial (?)
  _⟨=>⟩_ : {w x y z : O} → (w => x) -> (y => z) -> (x ~> y) => (w ~> z)
  _⟨=>⟩_ f h = (↔ [⨾] · h) ⨾ ([⨾] · f)

  field
    [id]⨾[⨾] : {x y : O} → [id] ⨾ [⨾] ≡ [id] {x ~> y}
    [id]⨾↔[⨾] : {x y : O} → [id] ⨾ ↔ [⨾] ≡ [id] {x ~> y}
    [↔]⨾[↔] : {x y z : O} →
      (x ~> (y ~> z) => (x ~> (y ~> z))) ∋ [↔] ⨾ [↔] ≡ id
    _⨾id : {x y : O} (f : x => y) → f ⨾ id ≡ f
    id⨾_ : {x y : O} (f : x => y) → id ⨾ f ≡ f
    _⨾→_⨾_ : {w x y z : O}
      (f : w => x) (g : x => y) (h : y => z) →
      (f ⨾ g) ⨾ h ≡ f ⨾ g ⨾ h
    agrees : {x y z : O}
      (f : x => y) (g : y => z) → [⨾] · f · g ≡ f ⨾ g
    -- Not sure if this can be derived?
    apswap : {x y z : O}
      (f : x => (y ~> z)) (a : I=> x) (b : I=> y) → ↔ f · b · a ≡ f · a · b

  _⨾id≡_ : {x y : O} → (f : x => y) → {g : y => y} → g ≡ id → f ⨾ g ≡ f
  f ⨾id≡ p = focus (f ⨾_) ⟨ p ⟩ ~ (_ ⨾id)
  _≡id⨾_ : {x y : O} → {g : x => x} → g ≡ id → (f : x => y) → g ⨾ f ≡ f
  p ≡id⨾ f = focus (_⨾ f) ⟨ p ⟩ ~ (id⨾ _)

  id·_ : {x : O} -> (v : I=> x) -> id · v ≡ v
  id· v = focus↑ (↓I=> v ⨾id)

  _≡id·_ : {x : O} -> {f : x => x} -> (f ≡ id) -> (v : I=> x) -> f · v ≡ v
  p ≡id· v = focus↑ (↓I=> v ⨾id≡ p)

  [⨾]·id : {x y : O} -> ([⨾] · id) ≡ id {x ~> y}
  [⨾]·id = focus↑ ([id]⨾[⨾])

  ↔[⨾]·id : {x y : O} -> (↔ [⨾] · id) ≡ id {x ~> y}
  ↔[⨾]·id = focus↑ ([id]⨾↔[⨾])

  ↔↔ : {x y z : O} (f : x => (y ~> z)) → ↔ (↔ f) ≡ f
  ↔↔ f =
    ascribe⟨ ↑I=> ((↓I=> (↑I=> ((↓I=> f) ⨾ [↔]))) ⨾ [↔]) ≡ f ⟩
    focus ↑I=>
      -- Cancel ↓↑I=> to get at the composition
      -- (The other ones are not cancelable!)
      -- Reassociate to gather the [↔]s
      ⟨ focus (_⨾ [↔]) ⟨ ↓↑I=> _ ⟩ ~ (_ ⨾→ [↔] ⨾ [↔])
      -- Cancel the now-adjacent [↔]s
      -- And remove the resulting id
      ~ ↓I=> f ⨾id≡ [↔]⨾[↔]
      ⟩
      -- To obtain ↑I=> (↓I=> f), which cancels
    ~ (↑↓I=> f)

  id⟨=>⟩id : {x y : O} →
    ((x ~> y) => (x ~> y)) ∋ id ⟨=>⟩ id ≡ id
  id⟨=>⟩id = _ ⨾id≡ [⨾]·id ~ ↔[⨾]·id

TypeClosed : {ℓ : Level} → Closed (Type ℓ)
TypeClosed = record
  { I = ⊤
  ; _~>_ = Fun
  ; I=>_ = λ T → T
  ; ↑I=> = λ f → f tt
  ; ↓I=> = λ x tt → x
  ; ↑↓I=> = λ f → refl
  ; ↓↑I=> = λ f → refl
  ; id = identity
  ; [⨾] = λ g f x → f (g x)
  ; [↔] = λ f y x → f x y
  ; _⨾_ = λ f g x → g (f x)
  ; [id]⨾[⨾] = refl
  ; [id]⨾↔[⨾] = refl
  ; [↔]⨾[↔] = refl
  ; _⨾id = λ f → refl
  ; id⨾_ = λ f → refl
  ; _⨾→_⨾_ = λ f g h → refl
  ; agrees = λ f g → refl
  ; apswap = λ f a b → refl
  }
