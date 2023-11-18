---
title: Proposal for Unicode in PureScript
subtitle: "Give us `CodePoint`{.haskell} literals!"
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

Revisiting [purescript/purescript#3362](https://github.com/purescript/purescript/issues/3662).

My main goals are:

- Specify strings more thoroughly
  - Let backends choose their preferred encoding, instead of trying to mandate UTF-16 (alternative backends like purerl have already gone for UTF-8 in practice)
  - Require literals in source code to be valid Unicode.
  - At runtime, strings may still be invalid Unicode in the backendʼs preferred representation.
  - Provide an abstract API for code units that is **encoding-agnostic**
    - The goal is to provide whatever operations you would need, so you donʼt ever feel tempted to do the arithmetic yourself
    - Side goal: reduce the FFI burden of implementations a bit
    - Provide tests for this API
- Have the compiler produce `CodePoint`{.haskell} literals instead of `Char`{.haskell}
  - This is one of the biggest annoyances of dealing with Unicode data in PureScript today: `CodePoint`{.haskell} literals do not exist.
  - Again: literals need to be valid (i.e. non-surrogate) code points, but at runtime there is no such guarantee.
- Keep changes to user code minimal
  - Besides renaming types and a few methods (sorry).

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
  - This assumption only works if strings are UTF-16.
    Lone surrogates cannot be represented in UTF-8 source code.^[Technically it is feasible to encode them, like any other code point, but no decoder accepts this.]
  - CoreFn encodes strings in as JSON strings if they are valid, but as arrays of UTF-16 code unit integers if there are lone surrogates ([source](https://github.com/purescript/purescript/blob/2412101a8301f2d63e8fb8b316a23ac8ff6463e6/src/Language/PureScript/PSString.hs#L132-L134)).
  - In fact, the whole [PSString module](https://github.com/purescript/purescript/blob/2412101a8301f2d63e8fb8b316a23ac8ff6463e6/src/Language/PureScript/PSString.hs) only exists to deal with lone surrogates.
- `Char`{.haskell} literals, since they are UTF-16 code units, can be lone surrogates (and cannot be code points from supplementary planes).

Another flaw is that `Ord String`{.haskell} has different behavior in edge cases in UTF-16 than that of UTF-8/UTF-32.
([Source](unicode.html#ordering).)
Uhh I think it is beyond scope to fix that in `Data.String`{.haskell} (comparisons should reflect native comparisons, otherwise stuff will get weird).
But technically this means that there could be some subtle nasty bugs with `Data.Map`{.haskell} and other ordered structures across backends if you are not careful.

### Dealing with invalid Unicode

However, the silver lining of UTF-16 was that invalid UTF-16 (i.e. lone surrogates) *could* be preserved in code points.
This is not the case with UTF-8: if you `uncons`{.haskell} a codepoint from an invalid UTF-8 string, you cannot preserve what you unconsed in any way.

I think each backend will have to choose the desired behavior:

- It can keep the current JS backend semantics, where UTF-16 lone surrogates become “code points”.
<!-- - Or it has to return `U+FFFD` or crash (depending on which API is used). -->
- Or it has to return `U+FFFD` when it sees invalid encodings.

:::Details
The existing behavior of JS corresponds to `decode('utf-16', errors='surrogatepass')`{.python} in [Python](https://docs.python.org/3/library/codecs.html#error-handlers), for example.
Throwing an error is the default Python behavior, and replacement characters can inserted instead by using `errors='replace'`.
:::

### Abstract interface

My goal is to specify a nice abstract interface between code points and code units, so that the exact encoding is irrelevant.

There will still be conversions `Array CodePoint <-> String <-> Array CodeUnit`{.haskell} and `BoundedEnum CodePoint`{.haskell}/&#8203;`BoundedEnum CodeUnit`{.haskell} instances.

Now we need to be able to deal with codeunits on their own.

The straightforward direction of the API is `CodePoint -> Array CodeUnit`{.haskell}.
This is like the composition `Data.String.CodePoints.singleton >>> Data.String.CodeUnits.toCharArray`{.haskell}.
Also need the optimized composition with `Data.Array.length`{.haskell}.

Then we need to design the way to parse code units into code points.

<details class="Details">

<summary>Abstract decoding API (inefficient version)</summary>

It is possible to give a nice abstract API to unicode encodings that does not depend on knowledge of the particular encoding.
This is what I claim would be an ideal interface.
Although it has too much overhead to implement this way, so I have used its core idea as inspiration for my proposal below.

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

#### Proposed

After some workshopping, this seems like a good API that supports multiple encoding formats abstractly, supports parsers and other use cases, and should be relatively efficient (depending on how many corners you want to cut):

```{.haskell data-lang="PureScript"}
Enc.maxCodeSeqLength :: (1 | 2 | 4 :: Int) -- ENC
-- The number of code units expected in a sequence that
-- starts with the given code unit (including itself).
Enc.codeSeqLength :: CodeUnit -> (0 | 1 | 2 | 3 | 4 :: Int) -- ENC
  isSingleton = eq 1 <<< codeSeqLength
  isContinuation = eq 0 <<< codeSeqLength
-- The number of code units to encode this code point.
Enc.codePointLength :: CodePoint -> (1 | 2 | 3 | 4 :: Int) -- ENC

-- Decode some code units into their corresponding codepoint.
-- This will not handle over-long arrays!
Enc.codeUnitsToPoint :: Array CodeUnit -> Maybe CodePoint -- ENC
-- The reverse direction, for efficiency.
Enc.codePointToUnits :: CodePoint -> Array CodeUnit -- ENC

-- Parse a code point out of a string, where the
-- index is given in code units
Enc.parseCodePoint ::
  forall r.
    -- 0 is passed to this continuation
    -- if the index is out of bounds;
    -- a positive number if an encoding
    -- error occurred
    (Int -> r) ->
    (Int -> CodePoint -> r) ->
    Int -> String -> r -- ENC

data ParsedCodePoint
  = OutOfBounds
  -- number of code units involved in the error
  = UnicodeError Int
  -- number of code units encoding the code point
  | UnicodeSuccess Int CodePoint

-- Crash on out of bounds, and return U+FFFD replacement character
-- for encoding errors (with the possible exception of surrogates).
-- Meant to be used with `Enc.advance`.
Enc.parseCodePointReplacement :: Int -> String -> CodePoint -- ENC

-- Advance from a sequence boundary to the next sequence boundary.
-- Similar to `syncFwd <<< add 1`, but assumes that it is at
-- a sequence boundary already, and is thus more efficient.
Enc.advance :: Int -> String -> Int

-- Sync backwards to the nearest code unit sequence boundary.
Enc.syncBwd :: Int -> String -> Int -- ENC
-- works only if valid Unicode
Enc.unsafeSyncBwd :: Int -> String -> Int -- ENC

-- Sync forwards to the nearest code unit sequence boundary.
-- syncFwd will return `length s` if `i` is a continuation
-- unit of a valid sequence.
Enc.syncFwd :: Int -> String -> Int -- ENC
-- works only if valid Unicode
Enc.unsafeSyncFwd :: Int -> String -> Int -- ENC


-- Chop a string into code unit sequences. Not very useful on
-- its own, but it explains the semantics of `syncBwd`/`syncFwd`.
Enc.codeUnitSequences
  :: String
  -> Array
    { start :: Int
    , length :: Int
    } -- ENC
-- Faster version that does not check for malformed Unicode.
Enc.unsafeCodeUnitSequences
  :: String
  -> Array
    { start :: Int
    , length :: Int
    } -- ENC

-- The length of a fragment of a code point at the end of the string
Enc.suffixFragmentLength :: String -> (0 | 1 | 2 | 3 | 4 :: Int)
Enc.appendWithFragment ::
  { buffer :: String, value :: String } ->
  { value :: String, buffer :: String }
```

#### Use cases

Parsers.

NodeJS buffer stuff: need a slosh buffer to reach code point alignment.
See proposed `Enc.appendWithFragment`{.haskell}.

## Proposed changes

### Compiler/spec

I have already implemented this.

- Remove `Prim.Char`{.haskell} and char literals
- Add `Prim.CodePoint`{.haskell} and code point literals
  - This means that code point literals are not restricted to BMP
  - However, lone surrogates will not be allowed in literals … but who would be using that in the first place??
    (It required escape codes to even write!)
  - It will be represented with integers
- Define strings to be any encoding, based on the backend (UTF-8, UTF-16, UTF-32)
- Require string literals to be valid Unicode
  - In particular, validate escape sequences
  - This is because this is the only portable way to do codegen across backends and encodings;
    it is not because strings at runtime will be required to be valid Unicode

### Libraries

- Not directly related but `Show String`{.haskell} needs to be fixed whoops
- `codePointFromChar :: Char -> CodePoint`{.haskell} needs to be partial: it only makes sense for `Singleton`{.haskell}
- Add `Data.String.CodeUnits.CodeUnit`{.haskell} as a newtype over `Int`{.haskell}
- `Data.String.CodePoints.uncons`{.haskell} currently assumes UTF-16 for some reason, instead of being FFI:
  https://github.com/purescript/purescript-strings/blob/v6.0.0/src/Data/String/CodePoints.purs#L191-L202
- Update the parser libraries!
  - Including the CST parser
- Revamp the Unicode library a bit while I am at it

Rough list of functions to add/replace in the `purescript-strings` library:

```haskell
CU = Data.String.CodeUnits
Enc = Data.String.Encoding
CP = Data.String.CodePoints

Prim.String :: Type
newtype Prim.CodePoint = CodePoint Int

newtype CU.CodeUnit = CodeUnit Int

-- FFI means that it is implemented in FFI
-- ENC means it is implemented in FFI, but just depends on the
-- encoding, it could have otherwise been implemented in PS
fromEnum :: CodeUnit -> Int -- FFI
fromEnum :: CodePoint -> Int -- FFI
toEnum :: Int -> Maybe CodeUnit -- FFI
toEnum :: Int -> Maybe CodePoint -- FFI
top :: CodeUnit -- ENC
cardinality :: CodeUnit -- ENC
  -- haha, this was actually off by 1

-- See above for Enc = Data.String.Encoding module.
-- Those are the main additions, and the rest of the
-- API is pretty much as-is!

-- O(1)
CU.length :: String -> Int -- FFI
-- O(1)?
CU.slice :: Int -> Int -> String -> String -- FFI
  +CU.substring :: Int -> Int -> String -> String
  CU.take :: Int -> String -> String
  CU.drop :: Int -> String -> String
  CU.takeEnd :: Int -> String -> String
  CU.dropEnd :: Int -> String -> String
-- O(n)
CU.countPrefix :: (CodeUnit -> Boolean) -> String -> Int -- FFI
-- O(1)
CU.codeUnitAt :: Int -> String -> Maybe CodeUnit -- FFI
  CU.head :: String -> Maybe CodeUnit
  CU.uncons
  +CU.last
  +CU.unsnoc

-- O(n)
CP.length :: String -> Int -- FFI
-- O(1)
CP.head :: String -> Maybe CodePoint
  CP.uncons
  +CP.last
  +CP.unsnoc
-- O(n)
CP.countPrefix'
  :: (CodePoint -> Boolean)
  -> String
  ->
    { codePoints :: Int
    , codeUnits :: Int
    }
-- O(n)
CP.codePointAt :: Int -> String -> Maybe CodePoint -- FFI

```

<details class="Details">

<summary>Implementations / Specs</summary>

```{.haskell data-lang="PureScript"}
syncBwd i s
  | i < length s && not isContinuation (unsafeCodeUnitAt i s) = i
syncBwd i s | i > length s = i
syncBwd i s = scanback (i-1)
  where
  scanback (-1) = i
  scanback j =
    case codeSeqLength (unsafeCodeUnitAt j s) of
      0 | j - 1 < i - maxCodeSeqLength -> i
      0 -> scanback (j-1)
      l | j+l > i -> j
      _ -> i

syncFwd i s | i >= length s = i
syncFwd i s
  | not isContinuation (unsafeCodeUnitAt i s) = i
syncFwd i s =
  -- Yes you have to scan back to decide what to do here
  let j = syncBwd i s in
  case codeSeqLength (unsafeCodeUnitAt j s) of
    -- We have a valid sequence that continues at `i`
    l | j+l > i ->
      -- We can skip forward one iteration of `loop` since
      -- we already checked that `i` is a continuation unit
      scanforward (i+1) ((j+l) - (i+1))
    -- Too far back; `i` is a lone continuation unit
    _ -> i
  where
  scanforward k 0 = k
  scanforward k r =
    case isContinuation (unsafeCodeUnitAt k s) of
      true -> scanforward (k+1) (r-1)
      false -> k


suffixFragmentLength s =
  let i = syncBwd (length s - 1) in
  case codeUnitAt i of
    Nothing -> 0
    Just cu -> max 0 $ (i + codeSeqLength cu) - length s

appendFragment { buffer, value } =
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
