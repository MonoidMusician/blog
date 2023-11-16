---
title: Proposal for Unicode in PureScript
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Revisiting [purescript/purescript#3362](https://github.com/purescript/purescript/issues/3662).

My main goals are:

- Specify strings more thoroughly
  - Let backends choose their preferred encoding, instead of trying to mandate UTF-16 (which is not what alternative backends like purerl have done in practice)
  - Provide an abstract API for code units that is **encoding-agnostic**
- Have the compiler produce `CodePoint`{.haskell} literals instead of `Char`{.haskell}
  - This is one of the biggest annoyances of dealing with Unicode data in PureScript today: `CodePoint`{.haskell} literals do not exist.
- Keep changes to user code minimal

The good news is that the existing string libraries are not too far from a sensible interface.
The UTF-16ness only leaks through in a few places.
And a few places were implemented incorrectly anyways.

So changing from strings being specified as UTF-16 to backend-dependent should not be too complicated, and will provide the opportunity to clean several things up along the way.

It is important, however, to keep code units accessible through the `Data.String`{.haskell} APIs:

- indexing strings by native code units is the only performant option (code points incur linear cost lookups)
- native APIs will assume that indices are code units, and – as always – providing easy FFI is a main priority




## Out of scope / non-goals

It is out of scope to make the string APIs 100% safe.
We cannot validate all strings passed through the FFI to ensure they are valid Unicode.
It is reasonable that lone surrogates will appear transiently while parsing!

It is also out of scope to fix the ordering of UTF-16 strings.
(See below, `Ord String`{.haskell}.)

It is also not a goal to provide separate UTF-8/UTF-16/UTF-32 types:

- My main objection to this idea is that it ties PureScript code to particular backends, when there is literally no reason to do so, unless you enjoy doing Unicode arithmetic yourself.
- For UTF-32, there is already a sensible type that will exist across backends: `Array CodePoint`.

Nor to provide separate `String`{.haskell} types for CodeUnits/CodePoints, or separate integer types for their indices/lengths.
The current API that has those two views on unified types works pretty well.

## Background on encodings

There are three encodings used for Unicode data:

- UTF-32, which encodes code points directly in four-byte units
- UTF-16, which is used by JavaScript but is not a good choice
- UTF-8, which is used by the vast majority of code and formats

Code points are numeric codes from `U+000000` to `U+10FFFF`^[This is 21 bits, not 32, limited by how UTF-16 works] used to number “characters”^[The concept of character is ill-defined.] assigned in the Unicode standard.

Each encoding has its concept of code unit: for UTF-32 this corresponds to code points directly, for UTF-16 a code unit is two bytes, and for UTF-8 a code unit is a single byte.

The main features that code units share across encodings:

- You can look at a code unit and tell whether it is a single code unit, a start code unit, or a continuation code unit.
  This means that you can recover to the nearest code point boundary, if you are scanning by code unit.
- The start code unit determines how many continuation code units follow.
- Continuation code units all have the same shape (there is no distinct first/second/third continuation code units).

### Flaws of UTF-16

The main flaw of UTF-16 is that nobody uses it! Except for Java and JavaScript!

It is space-inefficient, and it encourages developers to not think about the distinction between code units and code points until someone finally uses characters from supplementary planes.

The other major flaw is that it allows lone surrogates.
These cannot be represented in valid UTF-8 or UTF-32: most decoders will replace them with the replacement character `U+FFFD`.
UTF-8 and UTF-32 have their own invalid sequences, but lone surrogates are particularly pernicious for some reason.

The PureScript compiler already deals with lone surrogates in weird ways:

- It allows lone surrogates in strings.
  - This assumption only works if strings are UTF-16
  - CoreFn encodes strings in as JSON strings if they are valid, but as arrays of UTF-16 code unit integers if there are lone surrogates
- `Char`{.haskell} literals, since they are UTF-16 code units, can be lone surrogates (and cannot be code points from supplementary planes)

Another flaw is that `Ord String`{.haskell} has different behavior in edge cases in UTF-16 than that of UTF-8/UTF-32.
([Source](unicode.html#ordering).)
Uhh I think it is beyond scope to fix that in `Data.String`{.haskell} (comparisons should reflect native comparisons, otherwise stuff will get weird).
But technically this means that there could be some subtle nasty bugs with `Data.Map`{.haskell} and other ordered structures across backends if you are not careful.

### Uhh

However, the silver lining of UTF-16 was that invalid UTF-16 (i.e. lone surrogates) *could* be preserved in code points.
This is not the case with UTF-8: if you uncons a codepoint from an invalid UTF-8 string, you cannot preserve what you unconsed in any way.
(Itʼs not even clear how much the uncons should advance the string.)
So we need to make a decision here – and if it will be backwards compatible for the JS backend, or if it will be stricter validation.

idk, I guess it is weird.

corresponds to `decode("utf-16", errors="surrogatepass")`{.python} in Python: https://docs.python.org/3/library/codecs.html#error-handlers

:::Note
Interestingly, this invalidates `CP.length (fromCodePointArray cps) == Array.length cps`{.haskell}.
:::

This is particularly important for `NonEmptyString`{.haskell}.
Certainly a `NonEmptyString`{.haskell} is guaranteed to contain a `CodeUnit`{.haskell}, but is it guaranteed to contain a `CodePoint`{.haskell}??

I think itʼs just Undefined Behavior.
It should advance the state at least a little bit, though.
Probably return `U+FFFD`.
Or crash.

### Abstract interface

My goal is to specify a nice abstract interface between code points and code units, so that the exact encoding is irrelevant.

There will still be conversions `Array CodePoint <-> String <-> Array CodeUnit`{.haskell} and `BoundedEnum CodePoint`{.haskell}/&#8203;`BoundedEnum CodeUnit`{.haskell} instances.

Now we need to be able to deal with codeunits on their own.

The straightforward direction of the API is `CodePoint -> Array CodeUnit`{.haskell}.
This is like the composition `Data.String.CodePoints.singleton >>> Data.String.CodeUnits.toCharArray`{.haskell}.
Also need the optimized composition with `Data.Array.length`{.haskell}.

Then we need to design the way to parse code units into code points.

<details class="Details">

<summary>Abstract decoding API</summary>

It is possible to give a nice abstract API to unicode encodings that does not depend on knowledge of the particular encoding.
This is what I claim would be an ideal interface.
Although it has too much overhead to implement this way:

```{.haskell data-lang="PureScript"}
parseCodeUnit :: CodeUnit -> ParseCodeUnit

data ParseCodeUnit
  -- The CodeUnit represents a whole CodePoint on its own
  = Singleton CodePoint
  -- The CodeUnit is a continuation unit
  | Continuation
  -- The CodeUnit requires one continuation character
  -- For UTF-16, this is high surrogates, 0xD800 to 0xDBFF
  -- For UTF-8, this is bytes of the form 0b110xxxxx
  | NeedOne
    ( Fn1
      CodeUnit
      (Maybe CodePoint)
    )
  -- The CodeUnit requires two continuation characters
  -- For UTF-8, this is bytes of the form 0b1110xxxx
  | NeedTwo
    ( Fn2
      CodeUnit
      CodeUnit
      (Maybe CodePoint)
    )
  -- The CodeUnit requires three continuation characters
  -- For UTF-8, this is bytes of the form 0b11110xxx
  | NeedThree
    ( Fn3
      CodeUnit
      CodeUnit
      CodeUnit
      (Maybe CodePoint)
    )

-- This would be implemented in FFI for efficiency,
-- but the above type means we can give it a default
-- implementation in PS, and any user code that deals
-- with encodings in other contexts can be based on
-- this pattern
parseCodeUnits :: Int -> String -> Maybe CodePoint
parseCodeUnits i s = do
  cu0 <- charAt i s
  case parseCodeUnit cu0 of
    Singleton cp -> Just cp
    NeedOne more -> do
      cu1 <- charAt (i+1) s
      runFn1 more cu1
    NeedTwo more -> do
      cu1 <- charAt (i+1) s
      cu2 <- charAt (i+2) s
      runFn2 more cu1 cu2
    NeedThree more -> do
      cu1 <- charAt (i+1) s
      cu2 <- charAt (i+2) s
      cu3 <- charAt (i+3) s
      runFn3 more cu1 cu2 cu3
    Continuation -> Nothing
```

For UTF-32, it will always return `Singleton`{.haskell}.
For UTF-16, it will return `Singleton`{.haskell} or `Two`{.haskell}.
Only UTF-8 requires all four possibilities.

:::Note
There is no real benefit to introducing a `ContinuationUnit`{.haskell} type:

```haskell
-- For UTF-16, continuation units are low surrogates, 0xDC00 to 0xDFFF
-- For UTF-8, continuation bytes look like 0b10xxxxxx, 0x80 to 0xBF
foreign import data ContinuationUnit :: Type
```

There are two reasons:

1. Extra allocation overhead
2. Continuation units still need to be validated, at least in the case of `Four`{.haskell}, since `U+10FFFF` is only `0b11110100_10001111_10111111_10111111`{.js}.
  That is, UTF-8, even limited to 4 bytes, could represent higher code points than UTF-16 can.
:::

</details>

:::Note
Instead of closures, we probably just want something like:

```haskell
codeSeqLength :: CodeUnit -> (0 | 1 | 2 | 3 | 4 :: Int)

codeUnitsToPoint :: Array CodeUnit -> Maybe CodePoint

(length cus /= maybe 1 codeSeqLength (head cus))
  `implies` (codeUnitsToPoint cus == Nothing)
```
:::

#### Use cases

Parsers.

NodeJS buffer stuff: need a slosh buffer to reach code point alignment.

## Decisions to make

- Should we rename `Char`{.haskell} to `CodeUnit`{.haskell}??
  I would like to, but I can see why it looks like pointless code churn.

- Should it be a string of length 1 (for compatibility) or an integer?
  I think integer would be better.

- Should `CodePoint`{.haskell} be a newtype over `Int`{.haskell}?
  - Yes?
    - It would make sense
    - Not sure if it would stay in `Data.String`{.haskell} or move to `Prim`{.haskell} since it involves literals
  - No?
    - Backends might want to use their own type though (see discussion on the issue)
      - Iʼm not sure if it would maintain the invariants for e.g. Rust `char`
      - Yeah, we wonʼt maintain the invariants
    - Need to see how it would affect pattern matching in CoreFn, CoreImp, or wherever, to have it not be a newtype

## Proposed changes

### Compiler/spec

- Remove `Prim.Char`{.haskell} and char literals
- Add `Prim.CodePoint`{.haskell} and code point literals
  - This means that code point literals are not restricted to BMP
  - However, lone surrogates will not be allowed … but who would be using that in the first place??
    (I think that required escape codes to even write?)
- Define strings to be any encoding (UTF-8, UTF-16, UTF-32)
- Require string literals to be valid Unicode
  - In particular, validate escape sequences
  - This is because this is the only portable way to do codegen across backends and encodings;
    it is not because strings at runtime will be required to be valid Unicode

### Libraries

- Not directly related but `Show String`{.haskell} needs to be fixed whoops
- `codePointFromChar :: Char -> CodePoint`{.haskell} needs to be partial: it only makes sense for `Singleton`{.haskell}
- Add `Data.String.CodeUnits.CodeUnit`{.haskell}
- `Data.String.CodePoints.uncons`{.haskell} currently assumes UTF-16 for some reason, instead of being FFI:
  https://github.com/purescript/purescript-strings/blob/v6.0.0/src/Data/String/CodePoints.purs#L191-L202
- Revamp the Unicode library a bit while I am at it

Functions to add/replace:

```haskell

CU = Data.String.CodeUnits
Enc = Data.String.Encoding
CP = Data.String.CodePoints

Prim.String :: Type
Prim.CodePoint :: Type

foreign import data CU.CodeUnit :: Type

-- FFI means that it is implemented in FFI
-- ENC means it is implemented in FFI, but just depends on the
-- encoding, it could have otherwise been implemented in PS
fromEnum :: CodeUnit -> Int -- FFI
fromEnum :: CodePoint -> Int -- FFI
toEnum :: Int -> Maybe CodeUnit -- FFI
toEnum :: Int -> Maybe CodePoint -- FFI
top :: CodeUnit -- FFI/ENC

Enc.maxCodeSeqLength :: (0 | 1 | 2 | 3 | 4 :: Int) -- ENC
Enc.codeSeqLength :: CodeUnit -> (0 | 1 | 2 | 3 | 4 :: Int) -- ENC
  isSingleton = eq 1 <<< codeSeqLength
  isContinuation = eq 0 <<< codeSeqLength
Enc.codePointLength :: CodePoint -> (0 | 1 | 2 | 3 | 4 :: Int) -- ENC

Enc.codeUnitsToPoint :: Array CodeUnit -> Maybe CodePoint -- ENC
Enc.codePointToUnits :: CodePoint -> Array CodeUnit -- ENC

-- technically possible to do without FFI, but too complex
-- and too many allocations
-- OR: just use drop??
Enc.parseCodePoint :: Int -> String -> Tuple Int (Maybe CodePoint) -- FFI


-- Align to the previous/next sequence boundary
Enc.alignPrev :: Int -> String -> Int -- ENC
-- alignNext will return `length s` if `i` is a continuation
-- unit of a valid sequence
Enc.alignNext :: Int -> String -> Int -- ENC

-- The length of a partial code point at the end of the string
Enc.partialSuffixLength :: String -> (0 | 1 | 2 | 3 | 4 :: Int)
Enc.appendPartial ::
  { buffer :: String, value :: String } ->
  { value :: String, buffer :: String }

Enc.codeUnitSequences :: String -> Array { start :: Int, length :: Int } -- ENC

-- O(1)
CU.length :: String -> Int -- FFI
-- O(1)?
CU.slice :: Int -> Int -> String -> String -- FFI
  CU.substring :: Int -> Int -> String -> String
  CU.take :: Int -> String -> String
  CU.drop :: Int -> String -> String
  CU.takeEnd :: Int -> String -> String
  CU.dropEnd :: Int -> String -> String
-- O(1)
CU.codeUnitAt :: Int -> String -> Maybe CodeUnit -- FFI
  CU.head :: String -> Maybe CodeUnit
  CU.uncons

-- O(n)
CP.length :: String -> Int -- FFI
-- O(1)
CP.head :: String -> Maybe CodePoint
  CP.uncons
-- O(n)
CP.codePointAt :: Int -> String -> Maybe CodePoint -- FFI

```

<details class="Details">

<summary>Implementations / Specs</summary>

```{.haskell data-lang="PureScript"}
alignPrev i s
  | i < length s && not isContinuation (unsafeCodeUnitAt i s) = i
alignPrev i s = scanback i
  where
  scanback (-1) = i
  scanback j =
    case codeSeqLength (unsafeCodeUnitAt j s) of
      0 | j - 1 < i - maxCodeSeqLength -> i
      0 -> scanback (j-1)
      l | j+l > i -> j
      _ -> i

alignNext i s
  | not isContinuation (unsafeCodeUnitAt i s) = i
alignNext i s =
  -- Yes you have to scan back to decide what to do here
  let j = alignPrev i s in
  case codeSeqLength (unsafeCodeUnitAt j s) of
    -- We have a valid sequence that continues at `i`
    -- We can skip forward one iteration of `loop` since
    -- we already checked that `i` is a continuation unit
    l | i < j+l -> scanforward (i+1) ((j+l) - (i+1))
    -- Too far back; `i` is a lone continuation unit
    _ -> i
  where
  scanforward k 0 = k
  scanforward k r =
    case isContinuation (unsafeCodeUnitAt k s) of
      true -> scanforward (k+1) (r-1)
      false -> k


partialSuffixLength s =
  let i = alignPrev (length s - 1) in
  case codeUnitAt i of
    Nothing -> 0
    Just cu -> max 0 $ (i + codeSeqLength cu) - length s

appendPartial { buffer, value } =
  let total = buffer <> value in
  let r = splitAt (length total - 1 - partialSuffixLength total) total
  in { value: r.before, buffer: r.after }

-- Each code unit sequence is either a single continuation
-- unit or starts with a non-continuation unit and includes
-- up to the requisite number of continuation units (possibly
-- fewer, if it is invalid Unicode)
codeUnitSequences s = unfoldr start 0
  where
  l = length s
  start i | i >= l = Nothing
  start i =
    Just $ continue i (i+1) $ codeSeqLength (unsafeCodeUnitAt i s)
  continue j i 0 = restart j i
  continue j i | i >= l = restart j i
  continue j i n =
    case codeSeqLength (unsafeCodeUnitAt i s) of
      0 -> continue j (i+1) (n-1)
      _ -> restart j i
  restart j i = Tuple { start: j, length: i - j } i



parseCodePoint i s = Tuple (length cus) (codeUnitsToPoint cus)
  where
  l = length s
  cus
    | i < l =
      let cu0 = unsafeCodeUnitAt i s in
      case codeSeqLength cu0 of
        0 -> [cu0]
        1 -> [cu0]
        n -> toCodeUnitArray (slice i (min l (i+n)))
    | otherwise = []

```

</details>
