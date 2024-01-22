---
title: Unicode Explorer
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
---

<style>
  .widget {
    display: contents;
  }
  .table-wrapper {
    overflow-x: auto;
  }
  @media (min-width: 761px) {
    .table-wrapper {
      padding: 0.5em;
      margin: -0.5em;
    }
  }
  table.properties-table {
    border-collapse: collapse;
  }
  table.properties-table > tbody > tr {
    height: 1.7em;
  }
  table.properties-table > tbody > tr > td:first-child {
    padding-right: 0.5em;
    border-right: 1px solid #8f8f9f69;
  }
  table.properties-table > tbody:not(:last-child) {
    border-bottom: 1px solid #8f8f9f69;
  }
  table.properties-table > tbody > tr > td:last-child {
    padding-left: 0.5em;
  }

  /* Lines go past */
  table.properties-table {
    margin-left: -0.5em;
    margin-right: -0.5em;
  }
  table.properties-table > tbody > tr > td:first-child {
    padding-left: 0.5em;
  }
  table.properties-table > tbody > tr > td:last-child {
    padding-right: 0.5em;
  }

  .full-width.h-scroll {
    display: flex;
    justify-content: center;
    -webkit-mask-image: linear-gradient(90deg, transparent 0%, black 5%, black 95%, transparent 100%);
    mask-image: linear-gradient(90deg, transparent 0%, black 5%, black 95%, transparent 100%);
  }

  .code-points {
    margin: 10px 0;
    white-space: nowrap;
    max-width: 100%;
    overflow-x: scroll;
    overflow-y: visible;
    /* for mask-image */

    /* does not work in Firefox */
    /* https://aileenrae.co.uk/blog/firefox-scroll-padding-bug/ */
    /* padding: 0 5%; */

    /* This does nothing lol */
    scrollbar-gutter: stable;
    /* So we do this */
    padding-bottom: 15px;
  }
  .code-points > .code-point:first-child {
    margin-left: 5vw;
  }
  .code-points > .code-point:last-child {
    margin-right: 5vw;
  }
  .code-points > .code-point {
    display: inline-block;
    white-space: pre;
    border: 1px solid gray;
    width: 1.2em;
    height: 1.2em;
    vertical-align: bottom;
    text-align: center;
    line-height: 1.3;
  }
  .code-points > .code-point:not(:last-child) {
    border-right: none;
  }


  #TOC {
    display: none;
  }
</style>

Enter some text. Select some text. Have some fun.

Note: grapheme analysis is out of scope.

::: {.widget widget="Widget.Query" widget-empty="true" widget-datakey="unicode" widget-data-keys="unicode"}
:::

::: {.widget widget="Widget.Unicode" widget-datakey="unicode" widget-loading="true" widget-data-unicode="Hello World 1234! 😇 é é"}
:::

## Encodings

### Ordering

UTF-8 and UTF-32 have consistent ordering.^[Citations needed.]
But UTF-16 is kind of fucked up.

The reason that UTF-16 is fucked up is that surrogates are taken from [U+D800]{t=unicode  c=no-preview} through [U+DFFF]{t=unicode  c=no-preview} (and are used to encode [U+010000]{t=unicode  c=no-preview} through [U+10FFFF]{t=unicode  c=no-preview} – the astral characters), but [U+E000]{t=unicode  c=no-preview} through [U+FFFF]{t=unicode  c=no-preview} are still valid code points.
Is there a standard name for these?
I will call them “High BMP”.

In UTF-16, this High BMP region will compare as _greater than_ the astral characters.

Things get worse if you allow unpaired surrogates …

<!-- “^ Unicode range E000–F8FF is used as a private use area, which is reserved for private use.” (https://en.wikibooks.org/wiki/Unicode/Character_reference/E000-EFFF#ref_PUA , https://en.wikibooks.org/wiki/Unicode/Character_reference/F000-FFFF) -->

#### Example code

<details class="Details">
<summary>Code</summary>

```purescript
import Prelude
import Control.Alternative (guard)
import Data.Array as Array
import Data.Enum (toEnum)
import Partial.Unsafe (unsafeCrashWith)

-- LowBMP < Astral < HighBMP
data Region = LowBMP | Astral | HighBMP
derive instance Eq Region
derive instance Ord Region

-- Will crash on surrogates (U+D800 to U+DFFF)
compareUTF16 :: Array CodePoint -> Array CodePoint -> Ordering
compareUTF16 l r = Array.fold
  -- character-by-character comparison
  [ Array.fold (Array.zipWith cmp16 l r)
  -- and if they compared to be equal, then look at lengths
  , compare (Array.length l) (Array.length r)
  ]
  where
  cmp16 :: CodePoint -> CodePoint -> Ordering
  cmp16 cp1 cp2 =
    -- first look at the region
    (compare (regionOf cp1) (regionOf cp2)) <>
    -- then at the specific value
    (compare cp1 cp2)

regionOf :: CodePoint -> Region
regionOf cp = exactlyOneOf $ Array.catMaybes
  [ LowBMP <$ guard (isLowBMP cp)
  , Astral <$ guard (isAstral cp)
  , HighBMP <$ guard (isHighBMP cp)
  ]

isLowBMP :: CodePoint -> Boolean
isLowBMP = region 0x0000 0xD7FF

isHighBMP :: CodePoint -> Boolean
isHighBMP = region 0xE000 0xFFFF

isAstral :: CodePoint -> Boolean
isAstral = region 0x010000 0x10FFFF

region :: Int -> Int -> CodePoint -> Boolean
region cpLow cpHigh cp = cpLit cpLow <= cp && cp <= cpLit cpHigh

cpLit :: Int -> CodePoint
cpLit i = case toEnum i of
  Nothing -> unsafeCrashWith ""
  Just cp -> cp

exactlyOneOf :: forall a. Array a -> a
exactlyOneOf [a] = a
exactlyOneOf [] = unsafeCrashWith "No options in exactlyOneOf"
exactlyOneOf _ = unsafeCrashWith "Too many options in exactlyOneOf"
```

</details>

### Endianness

I am hoping I do not have to cover endianness here …

### Unpaired Surrogates

## Layout of Unicode

https://stackoverflow.com/questions/52203351/why-is-unicode-restricted-to-0x10ffff

https://en.wikipedia.org/wiki/Specials_(Unicode_block)

https://en.wikipedia.org/wiki/Private_Use_Areas

### Never Assigned

### Categories

https://en.wikipedia.org/wiki/Unicode_character_property#General_Category

Combining characters are assigned the Unicode major category "M" ("Mark").
https://en.wikipedia.org/wiki/Combining_character

- Mn = Mark, nonspacing
- Mc = Mark, spacing combining
- Me = Mark, enclosing

## Terminology

## Misc

- https://github.com/kennytm/omgwtf8
- https://www.channable.com/tech/so-long-surrogatesa
- https://wingolog.org/archives/2023/10/19/requiem-for-a-stringref
- https://news.ycombinator.com/item?id=31889544
- http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
  - https://hackage.haskell.org/package/text-2.1/docs/src/Data.Text.Internal.Encoding.Utf8.html#byteToClass
