---
title: Declarative WASM GC Types
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
---

<!-- {.wasm} {.haskell} ʼ -->

[WASM]{t=} modules are meant to be parsed and validated very linearly: they list complex types up front, and then refer to them by index inside functions and other key places in the module.
Throw in nested structured types with the WASM [GC]{t=} extension, and managing them declaratively becomes a little tough.
Recursive type groups are the most difficult to model declaratively!

I wonʼt say that the [WASM spec](https://webassembly.github.io/spec/core/) is bad, but it has a barebones style and lacks a lot of connective tissue that would turn it into really helpful documentation.
Having things organized by validation/execution/[etc.]{t=} makes getting a sense of individual instructions difficult.
And that version of the spec also does weird things with formatting/fonts, making it impossible to <kbd>Ctrl</kbd>+<kbd>F</kbd> for useful things like instruction names in Firefox, although somehow the [W3C spec](https://www.w3.org/TR/wasm-core-2/) is better in that regard.

So part of the goal of a declarative interface to WASM [GC]{t=} types is just to get a better sense of what they are and how they work.

For example, one interesting nuance is that every heap value knows its specific type, which is visible at runtime through casts to subtypes, and this subtype relationship can be refined within existing subtyping rules to simulate nominal types.
But what exactly gets assigned an index in the type table?
It seems like it should be a composite type, but spoiler: it is actually a recursive type (a particular subtype in a recursive group).

Here goes!

(If you want to skip to the good part, look at [Recursive Types: `RTyp`{.haskell}](#recursive-types-rtyp) and its Key Idea, but I can almost guarantee you that some of the details along the way will be surprising unless youʼve written a WASM implementation or a backend that targets WASM GC and faced these same problems before.)

## Preamble: Generating WASM Modules

The WASM file format features several tables of information (“sections”): a table of types, a table of imports, a table of functions, and so on.^[Okay, “table” has a specific meaning for WASM, meaning an indexed place to store references – like linear memory is for bytes and words. But saying table here in its lay sense conveys the structure of the information represented.]

The main idea is to deduplicate the things we are referring to: plop them in a table once, use their index to refer to them, and look them up the next time they come ʼround.

I made a `GenIdx`{.haskell} helper for *gen*erating these *indexes*.

```haskell
-- A helper for generating types, functions, et cetera, into a linear index
-- in a way so they are deduplicated. The idea is that they are deduplicated
-- by value, with its position in the index and metadata from the first
-- usage.
data GenIdx meta val = GenIdx
  !(Map val (Int, meta))
  ![(val, meta)] -- reverse order

-- Put a new item into the index
gen :: forall meta val. Ord val => GenIdx meta val -> val -> meta -> (GenIdx meta val, Int, meta)
gen g@(GenIdx validx stack) val newMeta =
  case Map.lookup val validx of
    Just (idx, oldMeta) -> (g, idx, oldMeta)
    Nothing ->
      let
        idx = Map.size validx
        g' = GenIdx (Map.insert val (idx, newMeta) validx) ((val, newMeta) : stack)
      in (g', idx, newMeta)

-- Reserve multiple items for sure, without checking whether those values
-- exist already in the index. For dealing with mutual recursion, basically.
reserveNForSure ::
  forall t each collected ret meta val.
    Ord val =>
    Traversable t =>
  GenIdx meta val ->
  t each ->
  (t (each, Int) -> collected) ->
  (each -> Int -> collected -> (val, meta, (meta -> GenIdx meta val -> GenIdx meta val) -> ret)) ->
  (GenIdx meta val, t ((val, (Int, meta)), ret), collected)
```

Generating each of those indexes have their own unique challenges, however.

For instance, imports and functions share the same index, but imports must come first:
this means that I actually need to generate the WASM module twice: once to gather all the imports that were used, and a second time with those imports pre-seeded in the front, so all the references line up to their correct locations.
(This was much easier than making sure the output format was structured enough to traverse correctly.)

Generating non-imported functions is also a little tricky: you can use their code to syntactically deduplicate them, but this does not work for recursive functions.
Instead, I use the trick of [Sticking Functions Where They Donʼt Belong](functions-as-data.html) to allow separating a key (what the function means) from how to generate the function.
So a function like `show_t :: t -> IOData` is represented as a unique type `newtype CShow = CShow STyp`{.haskell}, which uses a typeclass in Haskell to generate whatever code it wants and reference itself or other functions, and because the `CShow`{.haskell} key exists before its code, it can be deduplicated safely without infinite recursion.

Finally, there is the problem of generating types declaratively.

:::Key_Idea
What does declarative mean?
Well, it means that we want to be able to write coherent values in the Haskell source that refer to the types we intend, without having to worry about whether they were generated already or what index they landed at.

For instance, we want to be able to write `(struct (field (mut i64) (mut i32) (mut i32)))`{.wasm} as `WS [Mut WI64, Mut WI32, Mut WI32]`{.haskell}, then wrap it in a ref `WR Nul $ HCTyp $ WS [...]`{.haskell} that shows up in the source as something like `(ref null $mystruct)`{.wasm} or `(ref null 6)`{.wasm} once types are allocated to their places.
The key thing is that we are not going to rely on the global uniqueness of an identifier like `$mystruct`{.wasm}.

One of the key features is that comparing types for equality will be as simple as using the default `Eq`{.haskell} instance, nothing special required.
(This does mean that all metadata is omitted, particularly field names for structs.)

We want to be able to incorporate recursive type groups into this same framework.
It is surprisingly subtle!
:::

## Stack Types: `WTyp`{.haskell}

With the GC extension, WASM is divided into two kinds of types: stack types and heap types.
Stack types are of course the default, and everything that is processed has to go through the stack, so itʼs worth talking about first.

The stack is meant to be shuffle through machine registers, so it includes 32 and 64 bit types: integers and floats, `i32`{.wasm}, `i64`{.wasm}, `f32`{.wasm}, and `f64`{.wasm}.
It also includes a 128 bit type `v128`{.wasm} for vector registers (undiscriminated between integers and floats).
Finally, with the GC extension, there is a `ref`{.wasm} type constructor to reference heap types from the stack.

Stack types are always monomorphic, but the `ref`{.wasm} itself can point to a richer type lattice.
For example, the top of this lattice, `anyref`{.wasm} [a.k.a.]{t=} `(ref null any)`{.wasm}, allows passing any reference whatsoever, and then it can be casted to a useful type later (where WASM is responsible for safely checking the valueʼs runtime type, which it has to know for garbage collection anyways).

:::Note
There is a special `i31`{.wasm} (yes, 31 bit) “heap” type that is meant to live on the stack, with a tag bit reserved to distinguish `(ref i31)`{.wasm} from actual references.
This does not need to be treated specially from the surface or semantics level: it is just an available optimization for implementations.
(Though it does show up in a funny place: in `HTyp`{.haskell} directly.)
:::

I named the [ADT]{t=} for stack types `WTyp`{.haskell}, for “WASM type”.

```haskell
-- Basic WASM types that live on the stack, plus packed types that are unpacked
-- into i32 on the stack.
data WTyp
  = WF64 | WF32   -- Floats f64/f32
  | WI64 | WI32   -- Integers i64/i32
  | WI16 | WI8    -- Packed integers for structs/arrays
  | WV128         -- Vector register v128
  | WR !Nul !HTyp -- Reference to a garbage-collected heap type

data Nul = Nul {- nullable -} | Non {- nonnullable -}
```

:::Warning
You read that right, the lying has started already!
The only types that live on the stack are 32, 64, or 128 bits wide.

The **packed types** `i16`{.wasm} and `i8`{.wasm} technically do not live on the stack, they can only appear in other contexts (`struct`{.wasm} fields and `array`{.wasm} data), but it is convenient to include them here.
They are canonically mapped to `i32`{.wasm} on the stack with the appropriate instructions (sign extending or not).
:::

Aside from that, it is straightforward translation: floats, integers, vectors, and references, which may be nullable.

## Function Types

Function types are not too complicated: conceptually they are just a list of stack types for input and for output, `([WTyp], [WTyp])`{.haskell}.
The usual variance rules apply to them: contravariant in the input, covariant in the output.

They are important because functions exist on their own in the module.
In terms of the heap, though, we will see that they slot into “composite types”.
See the `WF`{.haskell} constructor [below](#composite-types-ctyp).

## Heap Types Designations: `HTyp`{.haskell}

Heap type designation means “what a `ref`{.wasm} can refer to”, so it includes both concrete heap types (structs, arrays, functions) as well as heap type classifiers.
The quasi-stack type `i31`{.wasm} also slots in here: it is clearly not a generic type classifier, nor is it a composite type (`struct`{.wasm}/`array`{.wasm}).

```haskell
-- Heap types, the target of references. This includes composite types
-- (structs, arrays, and functions) and their recursive and subtyped forms, plus
-- the means to classify them (any struct, any array, any function, etc.), and
-- the i31 type which is left for implementations to use as an immediate, not
-- actually heap allocated.
data HTyp -- Heap types
  = HCls !HCls -- Abstract classified reference types
  | HRTyp !RTyp -- Recursive and composite types
  | HI31 -- Special i31

-- Constructor for non-recursive composite types
pattern HCTyp :: CTyp -> HTyp
pattern HCTyp t = HRTyp (CTyp t)
```

The heap type directly references recursive types `RTyp`{.haskell}, but through that, non-recursive composite types `CTyp`{.haskell} also bubble up, so we include a special pattern synonym to recognize that.

Note that `HCls`{.haskell} and `HI31`{.haskell} have syntax to reference them, but recursive and composite types need to be generated and assigned an index to be used.

### Heap Type Classifiers: `HCls`{.haskell}

The heap type classifiers round out the type lattice with some interesting points: `any`{.wasm} and `none`{.wasm} for the top and bottom types, respectively.

Specific types of values have their own sub-lattices, with top types named by themselves: any `struct`{.wasm}, any `array`{.wasm}, any `func`{.wasm}, and any `extern`{.wasm}.
Some lattices have bottom types too: `nofunc`{.wasm} and `noextern`{.wasm}.
That is, `nofunc`{.wasm} does not mean “the negation of `func`{.wasm} within the whole type lattice” but rather “the function type that is below every other function types, but with no inhabitants of its own”.

The algebra of types says that one can take joins (least upper bound/supremum) and meets (greatest lower bound/infimum) of types in this lattice, but most of the answers are uninteresting: [e.g.]{t=} the join of `struct`{.wasm} and `array`{.wasm} is `any`{.wasm}, there is no more specific label for their union.

Regardless of the semantic picture, it is easy enough to paste it into an [ADT]{t=}:

```haskell
-- Heap type classifiers
data HCls = HAny | HEq | HStruct | HArray | HNone | HFunc | HNoFunc | HExtern | HNoExtern
```

:::{.Details box-name="Aside"}
WASM has a whole bunch of shorthand reference types for the type classifiers in `ref`{.wasm}s.

::::{.centered style="margin: auto; width: fit-content; text-align: left"}
+---------------------------------+--------------------------------------+
| `anyref`{.wasm .nowrap}         | `(ref null any)`{.wasm .nowrap}      |
+---------------------------------+--------------------------------------+
| `eqref`{.wasm .nowrap}          | `(ref null eq)`{.wasm .nowrap}       |
+---------------------------------+--------------------------------------+
| `i31ref`{.wasm .nowrap}         | `(ref null i31)`{.wasm .nowrap}      |
+---------------------------------+--------------------------------------+
| `structref`{.wasm .nowrap}      | `(ref null struct)`{.wasm .nowrap}   |
+---------------------------------+--------------------------------------+
| `arrayref`{.wasm .nowrap}       | `(ref null array)`{.wasm .nowrap}    |
+---------------------------------+--------------------------------------+
| `nullref`{.wasm .nowrap}        | `(ref null none)`{.wasm .nowrap}     |
+---------------------------------+--------------------------------------+
| `funcref`{.wasm .nowrap}        | `(ref null func)`{.wasm .nowrap}     |
+---------------------------------+--------------------------------------+
| `nullfuncref`{.wasm .nowrap}    | `(ref null nofunc)`{.wasm .nowrap}   |
+---------------------------------+--------------------------------------+
| `exnref`{.wasm .nowrap}         | `(ref null exn)`{.wasm .nowrap}      |
+---------------------------------+--------------------------------------+
| `nullexnref`{.wasm .nowrap}     | `(ref null noexn)`{.wasm .nowrap}    |
+---------------------------------+--------------------------------------+
| `externref`{.wasm .nowrap}      | `(ref null extern)`{.wasm .nowrap}   |
+---------------------------------+--------------------------------------+
| `nullexternref`{.wasm .nowrap}  | `(ref null noextern)`{.wasm .nowrap} |
+---------------------------------+--------------------------------------+
::::

WASM sure loves its nullable references.
I am not exactly sure why they included that, given that every instruction accepts nullable references and panics if they are null.
But I suppose it makes translating imports and exports with other languages, particularly Rust, much more palatable.
It might make optimization easier as well.
:::

### Composite Types: `CTyp`{.haskell}

Beyond the abstract types, we get to the real interesting types: these are specific struct, array, and function types.

:::Warning
This is where the distinction between the **packed types** `i8`{.wasm} and `i16`{.wasm} ought to appear:
structs and arrays take **packed types**, while functions need to stick to **stack types**.

But for my purposes it was just not worth it to distinguish them and require another layer of wrapping in the Haskell representation.
[tmTTmt would fix this,,](tmttmt.html#types)
:::

```haskell
-- Composite types (for type declarations)
data CTyp
  = WS ![Mut WTyp] -- Struct, with individual field types
  | WA !(Mut WTyp) -- Array, with its item type
  | WF ![WTyp] ![WTyp] -- Function, with input and output types

data Mut t = Mut !t {- mutable -} | Imm !t {- immutable -}
```

:::Warning
  Note the key difference: a struct includes a *list* of fields, each with their own type and mutability, while an array only includes *one element type*, and is variable length at runtime, of course.
  (Yeah, no named struct fields, sorry.)
:::

:::Error
  Although arrays can be marked immutable, they fundamentally are _not very useful_ without mutation.

  Thus far, WASM has not included a way to [initialize variable-length arrays without mutation](https://github.com/WebAssembly/design/issues/1561) or to [freeze arrays once they have been created or reference them readonly](https://github.com/WebAssembly/gc/blob/main/proposals/gc/Post-MVP.md#readonly-fields).
  Importantly, [`array.copy`{.wasm}](https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-array-mathsf-array-copy-x-1-x-2) is **not** it: that copies a slice from a source into a mutable destination slice.

  Statically-sized immutable arrays can be initialized from [a default value with `array.new`{.wasm}/`array.new_default`{.wasm}](https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-array-mathsf-array-new-x), from [items on the stack with `array.new_fixed`{.wasm}](https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-array-mathsf-array-new-fixed-x-n), from [data segments with `array.new_data`{.wasm}](https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-array-mathsf-array-new-data-x-y) (for byte types), or from [element segments with `array.new_elem`{.wasm}](https://webassembly.github.io/spec/core/valid/instructions.html#xref-syntax-instructions-syntax-instr-array-mathsf-array-new-elem-x-y) (for reference types).

  Tangentially, arrays also have awkward shortcomings for web/host interop ([“Wasm GC isn’t ready for realtime graphics”](https://dthompson.us/posts/wasm-gc-isnt-ready-for-realtime-graphics.html)).
  It looks like [multibyte array accesses](https://github.com/WebAssembly/multibyte-array-access/blob/main/proposals/multibyte-array-access/Overview.md) may be trundling forward, but not fast/convenient array access from the host.^[In theory I think web engines can JIT both JavaScript and WASM together? But I am not sure.]
:::

:::{.Example box-name="Examples"}
  Now you can make rich types.

  You can make your own kind-of linear memory with `WA (Mut WI64)`{.haskell} for words or `WA (Mut WI8)`{.haskell} for bytes.

  Maybe you decide that these need a bitstring counterpart, so you slap a length on it to make `WS [Mut WI32, Mut (WR Nul (HCTyp (WA (Mut WI64))))]`{.haskell}, which shows nicely how they nest declaratively.

  That declarativeness is not allowed in WASM: you cannot write `(struct (field (mut i32)) (field (mut (ref null (array (mut i64))))))`{.wasm} in WASM, you have to generate the inner type first, `(type $wordstring (array (mut i64)))`{.wasm}, before you can use it in `(ref null $wordstring)`.

  As a more complex example, here is how I am modeling closures in my toy [FP]{t=} language:

  ```haskell
  baseClosureC :: CTyp
  baseClosureC = WS
    -- boxed partial application
    [ Imm $ WR Non $ HCTyp $ WF
      -- closure data and next argument, boxed
      [WR Nul (HCls HAny), WR Nul (HCls HAny)]
      -- boxed result
      [WR Nul (HCls HAny)]
    -- unboxed partial application, if available
    , Imm $ WR Nul (HCls HFunc)
    -- closure data
    , Imm $ WR Nul (HCls HAny)
    ]

  baseClosureR :: RTyp
  baseClosureR = RTyp "closure" $ Map.singleton "closure" $
    SubTyp False Nothing baseClosureC
  ```

  At the end it contains closure data, which is any arbitrary type (or even just `null`{.wasm}).
  Before that it contains two functions: one has the generic boxed shape, `(func (param anyref) (param anyref) (result anyref))`{.wasm}, and another is `anyfunc`{.wasm} standing for an unboxed function, if it can be used directly.

  This is then wrapped in a `RTyp`{.haskell}, not because it is recursive, but so it can be marked as a non-`final`{.wasm} subtype, open to extension.
  Specific closure types can use `SubTyp False (Just baseClosureR)`{.haskell} to extend it, though this is only useful for avoiding some casts, I guess.
:::

When you think of a generated type referenced through a `ref`{.wasm}, a composite type is probably what you are thinking of.
Strictly speaking, though, composite types are *not* what are generated in the source file: _subtypes in recursive groups_ are.
And those are the tricky bit.

### Recursive Types: `RTyp`{.haskell}

Everything has been straightforward so far.
Tree-shaped, nicely declarative.
You just need to deduplicate the same type appearing in different branches of the tree.

How do you model recursive types though? *owo*

I opted for the minor evil of requiring *local* names for the types.
It is still quite declarative!

In WASM, a recursive type group consists of a bunch of subtypes.
Subtypes are composite types with a little extra fanfare, more on that later.

A type group acts flat in the file: each subtype just receives the next type index.
The types can reference any type in the group, preceding or following, or itself.
Their subtype relations can only point to preceding types, though (this ensures that the subtype relationships are acyclic).

A recursive type group is compared [in order, as a unit](https://stackoverflow.com/questions/77472803/how-to-understand-the-recursive-types-in-wasm#77476157).

:::Key_Idea
How can we use local names to model recursive types?
How do you do it in the middle of all the other stuff happening?

Well, a recursive type **group** can be modeled by `Map Name SubTyp`{.haskell}: a group of subtypes with names for each.^[See note on their (lack of) ordering below: it requires a toposort.]
And a recursive **type** is modeled by `(Name, Map Name SubTyp)`{.haskell}: it keeps the group together, just as it is, and picks out a valid name from it saying, “this one here, refer to *that* type from the lot”.

So the first constructor of `data RTyp`{.haskell} is `RTyp !Name !(Map Name SubTyp)`{.haskell}.

But below this `Map Name SubTyp`{.haskell}, that `RTyp`{.haskell} constructor is outlawed: you cannot refer to another group, and you cannot directly refer to your own group.
Instead you *must* use another constructor `RRef !Name`{.haskell}.

When you pull a subtype out of the `Map`{.haskell}, you promote all of the `RRef`{.haskell}s to `RTyp`{.haskell}s by adding the group.
This keeps everything declarative!

Finally, we include a constructor `CTyp !CTyp`{.haskell} for plain composite groups: there is no sense of going through the ceremony of a `RTyp`{.haskell} with an empty/default name and final subtype for them.
:::

Putting this into a datatype we get:

```haskell
-- A potentially-recursive type can be an ordinary composite type (technically
-- redundant but really nice to have), or a particular case chosen out of a
-- recursive group `Map Name SubTyp`, always kept together. Within the subtypes
-- comprising this recursive group, `RRef` is valid to refer to that particular
-- group: no nesting is possible, and it should not be necessary. If mutual
-- recursion is needed, it should be aggregated into a larger group.
--
-- This enables a simple representation of recursive type groups without
-- imposing any kind of global coherence requirements, just local coherence
-- of names within a single group.
data RTyp
  = RTyp !Name !(Map Name SubTyp) -- Recursive types and subtypes
  | RRef !Name -- Recursive references, only valid within a recursive group
  | CTyp !CTyp -- Plain composite groups
```

Technically speaking, this `RTyp`{.haskell} is what gets assigned an index in the table: it can be a plain `CTyp`{.haskell} or it can be a subtype from a recursive group, where `RRef`{.haskell} has to refer to the right thing as items are still being added.^[This is what `reserveNForSure`{.haskell} is for.]

:::Warning
What about mutual recursion?

There are no mutually recursive type groups in WASM or in this model: you are responsible for aggregating them into a single type group yourself, assigning names to make it work.

<details class="Details">
<summary>Details</summary>

```haskell
-- ADTs as a synthetic data type: many constructors with many fields each.
-- Each field can either be directly recursive (not mutually recursive), or an
-- external type. Substitute the recursion with `unrollTDat`.
newtype TDat = TDat (Map Name [(Name, Recursive () STyp)])
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SDat :: Map Name [(Name, Recursive () STyp)] -> STyp
pattern SDat constructors = S (TDat constructors)
-- Mutually recursive types
data TMutDat = TMutDat Name (Map Name (Map Name [(Name, Recursive Name STyp)]))
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SMutDat :: Name -> Map Name (Map Name [(Name, Recursive Name STyp)]) -> STyp
pattern SMutDat which group = S (TMutDat which group)

-- Unroll TDat, giving `STyp` instead of `Recursive () STyp`
unrollTDat :: TDat -> Map Name [(Name, STyp)]
unrollTDat t@(TDat constructors) =
  constructors <&> fmap (fmap (recurses (const (S t))))

-- Unroll TMutDat, giving `STyp` instead of `Recursive Name STyp`
unrollTMutDat :: TMutDat -> Map Name [(Name, STyp)]
unrollTMutDat (TMutDat which group) =
  group ! which <&> fmap (fmap (recurses (\name -> SMutDat name group)))

instance SynthType TDat where
  -- Codegenned as a recursive group with a _supertype struct which the
  -- individual constructors subtype
  wtyp (unrollTDat -> constructors) = WR Non $ HRTyp $ RTyp "_supertype" $
    Map.insert "_supertype" (plainTyp $ WS []) $
      constructors <&> plainTyp . WS . fmap (Mut . wtyp . snd)
  isDataType = Just . unrollTDat
instance SynthType TMutDat where
  -- Codegenned as a recursive group where each type gets its own section for
  -- its supertype and constructors, ty._self and ty.con1, ty.con2, etc.
  wtyp (TMutDat which group) = WR Non $ HRTyp $ RTyp (suffix which "_supertype") $
    group & Map.foldMapWithKey \ty constructors ->
      Map.insert (suffix ty "_supertype") (plainTyp $ WS []) $
        Map.fromList $ Map.toList constructors <&>
          \(k, fields) -> (suffix ty k,) $ plainTyp $ WS $
            fields <&> Mut . wtyp . recurses (\other -> SMutDat other group) . snd
    where
    suffix (Name name) (Name suffix) = Name (name <> "." <> suffix)
  isDataType = Just . unrollTMutDa
```
</details>
:::

### Subtypes: `SubTyp`{.haskell}

A subtype is a composite type (struct, array, or function) with some extra information:

- Is this a subtype of another type? (which must have lower index: must occur earlier in the file, same recursive group or not)
- Is this subtype `final`{.wasm}, or are other types allowed to extend it?

:::Warning
Note that the syntax for this is a bit weird:

The declaration is `(rec (sub final? (...)))`{.wasm}, where `rec`{.wasm} and `sub`{.wasm} are both optional.
Leaving `rec`{.wasm} out doesnʼt do much, it just means that the subtype belongs to its own group.
Leaving `sub`{.wasm} out is weird because without `sub`{.wasm}, `final`{.wasm} is implied.

Finally, the less relevant oddity is that the spec is open to allowing multiple supertypes for future compatibility, but it is not allowed past the syntax, to keep implementations simple.
Frankly, I donʼt see this changing ever: it seems like the tree-shaped custom subtyping is so much simpler, considering that it has to be viable to check at runtime, especially between modules.^[I am not actually familiar with how the runtime type checking is implemented. Obviously they are not walking type trees.]
And not enough people care or even know how it works to justify changing it; I have to infer that the usage of complex GC types is pretty low, from the lack of information about them online.
While it is *nice* that limited custom subtyping behavior is available, projects should not make it load-bearing, not to the extent where you would need a full lattice instead of a tree.
Just add a tag field yourself!!
:::

```haskell
-- Subtypes can be declared final and declared to extend another nominal type
-- (that must be structurally compatible with the type definition).
data SubTyp = SubTyp
  { stFinal :: !Bool
  , stExtends :: !(Maybe RTyp)
  , stIs :: !CTyp
  }
```

This allows rather interesting flexibility over the subtyping lattice, where each subtype can slot itself in beneath another type, if allowed and acyclic.

Of course this subtyping is not arbitrary, it must follow the primary subtyping rules.
You cannot just fiat declare an array type a subtype of a function type, for example.
But those rules should be intuitive.

This matters for runtime type casts, visible through `ref.cast`{.wasm} (which aborts/traps on mismatch) and the branching instructions `br_on_cast_fail`{.wasm}, `br_on_cast`{.wasm}, `br_on_null`{.wasm}, and `br_on_nonnull`{.wasm}.

:::Bonus
Thatʼs right! Thatʼs the sound of *generating declarative WASM types* requiring a [toposort](https://en.wikipedia.org/wiki/Topological_sorting)!

Hereʼs my cutest toposort implementation in 5 lines of code:

```haskell
-- Toposort in 5 lines of code
toposort :: forall k. Ord k => NFData k => Map k (Set k) -> [k]
toposort = join . reverse . go [] where
  go :: [[k]] -> Map k (Set k) -> [[k]]
  go acc entries =
    let (sel, remaining) = Map.partition Set.null entries
    in if Map.null sel then acc else
      go (Map.keys sel : acc) (remaining <&> (Set.\\ Map.keysSet sel))
```
:::


## Generating Types

So the good thing about this declarative model of WASM types is that the data says what it means and means what it says.
When maintaining the `GenIdx`{.haskell} of types generated so far, we can rely on plain equality to see if the type already exists.

The bad news is that it is not trivial to generate, especially if you are sticking to good functional programming practice.

What gets generated?
Strictly speaking, an `RTyp`{.haskell} is what gets deduplicated, turned into syntax, and assigned an index in the types section.
The recursive type itself drags along the recursive type group it belongs to: this needs to be handled altogether, then the recursive type (the particular subtype) plucked back out of the result.
Subtypes are extra metadata on top of composite types, so composite types are special cases of recursive types.

It actually requires two indexes: `typeGroups`, simply for deduplicating the group, and `types`, for generating individual types (bypassing the normal duplication check for recursive subtypes, in fact, by using `reserveNForSure`{.haskell}).

```haskell
-- WASM state context
data WASMS = WASMS
  { ...
  , types :: GenIdx (Maybe Name, Map Name SubTyp) WASMC
  -- ^ composite types generated so far (struct, array, func)
  , typeGroups :: GenIdx (Map Name WASMC) (Map Name SubTyp)
  -- ^ recursive type groups generated so far, with their internal naming
  ... }
```

That is the state needed, and here is how the syntax shakes down:

+----------------------------+---+
| `RTyp`{.haskell .nowrap}   | Generates as a whole recursive type group, gets assigned an index and is solely referenced by index or name.
+----------------------------+---+
| `SubTyp`{.haskell .nowrap} | Individual member of a recursive type group, like `RTyp`{.haskell} but viewed from the inside.
+----------------------------+---+
| `CTyp`{.haskell .nowrap}   | Composite type, the main part of a `SubTyp`{.haskell}: a particular `struct`{.wasm}, `array`{.wasm}, or `func`{.wasm} type.
+----------------------------+---+
| `HTyp`{.haskell .nowrap}   | Heap type designation, which is either a `RTyp`{.haskell} (including `CTyp`{.haskell}) which is generated as above, or a classifier or `i31`{.wasm} which are keywords already and not generated in the index.
+----------------------------+---+
| `WTyp`{.haskell .nowrap}   | Stack type, either concrete (`f32`{.wasm}, `i8`{.wasm}, which is used directly), or a reference to a heap type designation `HTyp`{.haskell}, where composite types are always referenced by index or name in the `ref`{.wasm} syntax.
+----------------------------+---+

Turning a recursive type group `Map Name SubTyp`{.haskell} into syntax is not as easy as it sounds, though.

First you need to generate every child type referenced, both the composite types for each case *and* the supertypes they might declare.
Next you need to toposort the group by supertype relationship.
This allows you to reserve those indices from the table, and then you can use this to generate the syntax without touching the table in the meantime.
Then the table indices are filled in and returned, as a group or the individual type of interest.
Following functional design principles means that the generation functions need to operate both on the `WASMS`{.haskell} state live, generating new types as they go, or on the frozen state, with everything pre-generated or reserved.
