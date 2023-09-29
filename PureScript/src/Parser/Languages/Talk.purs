module Parser.Languages.Talk where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Apply (lift2)
import Data.Array as Array
import Data.Compactable (class Compactable, compact)
import Data.Either (Either, blush, hush)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

-- https://dl.acm.org/doi/10.1145/3341694
-- https://dl.acm.org/doi/10.1145/3409002

-- https://cofree.coffee/~verity/parser.html
-- https://cofree.coffee/~verity/css_selectors.html

-- https://github.com/MonoidMusician/blog/blob/274c1d64c12248773501e2df276cd305ecd94a14/PureScript/src/Parser/Comb/Types.purs#L38-L85
-- (to be pulled out into a proper library later)

{-
0. Motivation?
-}

{-
1. Types of parsers
  - Codecs
    - JSON codecs (Aeson (Haskell), Argonaut (PureScript/Scala))
      - parse from structured type into application types
    - I mean just one way (deserialization)
      - could look bidirectional CST codecs to complete the loop
    - here: CST codecs
      - Concrete Syntax Trees
        - contain all the tokens from the input string
        - but have different ways to represent the same application data
  - Binary formats
    - MP4
    - encode chunks as [length, data of right length ...]
      - typically a limited number of ways to actually parse the data
      - thus selective applicatives should capture it, no need for monads
  - Grammars
    - parsing out of strings/binary, which are very unstructured
    - From least to most powerful:
      - Regular expressions
        - regex libraries are not regular expressions anymore!
      - Context-free (applicative)
      - Context-sensitive (monadic)
-}

{-
2. LR parsers for context-free grammars
  - Deterministic push down automata
    - State transition table
    - Stack of states
  - CST
    - Concrete Syntax Tree

My blog explaining LR(1) parsing, interactively!!
https://cofree.coffee/~verity/parser.html
-}

type Name = String
type RuleName = Tuple Name Int
type Tok = String

type Grammar = Array
  { name :: Name
  , ruleName :: RuleName
  , rule :: Fragment
  }

type Fragment = Array (Either Name Tok)

data CST
  = Leaf Tok
  | Branch RuleName (Array CST)

-- The Concrete Syntax Tree contains all of the tokens from the input
tokensOf :: CST -> Array Tok
tokensOf (Leaf t) = [t]
tokensOf (Branch _ children) = foldMap tokensOf children

{-
3. Selective applicative
  - https://dl.acm.org/doi/10.1145/3341694
  - Every applicative is a selective applicative, since you can implement
    `select` in terms of `apply`, but this is usually not what you want,
    since it means you don't get to skip effects at all.
    (There are lots of different ways for functors to be selective applicatives)
-}

-- flip (<*>) :: f a -> f (a -> b) -> f b
-- select :: f (Either b a) -> f (a -> b) -> f b
-- branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
-- and so on ... (they aren't equivalent in general, but for parsers we don't care)

-- compact :: forall a. f (Maybe a) -> f a

-- Implement `select` in terms of `compact` (failure) and `<|>`
select ::
  forall f a b.
    Alt f =>
    Compactable f =>
    Apply f =>
  f (Either b a) -> f (a -> b) -> f b
select l r =
  compact (blush <$> l)
    <|> ((#) <$> compact (hush <$> l) <*> r)

-- ^ I forgot to say: how I came up with this is I was thinking of the binding
-- structure, like if you made a DSL for it: `<|>` and `branch` both have the
-- same sort of binding structure, since they're both versions of "cases"
-- in different ways (`branch` tells you which case ahead of time,
-- `<|>` relies on parsing to handle it)

-- Punchline: for things like parsers
-- you only need selective failure (`compact`) to implement `select`



{-
4. Split grammar/codec parser combinators
  - Some (non-recursive) data to build a grammar from the parser combinators
  - A (recursive) codec to take the CST into application types
  - Not a free construction!
    - See https://dl.acm.org/doi/10.1145/3409002 for a free construction
-}

newtype Parser a = Parser
  { rules :: Array
    { rule :: Fragment -- Array (Either Name Tok)
    , result :: Codec a
    }
  -- more stuff ;)
  , grammar :: Array
    { name :: Name
    , rules :: Array Fragment
    -- no codec here! the codec for recursive calls get embedded recursively
    -- in the `result` codec above, and in fact you can have multiple codecs
    -- for the same grammar productions, it does not matter at all
    --
    -- (until you get to selective context sensitivity, then you do need to care
    -- about different ways in which the same production can fail)
    }
  }
  -- see the full type at
  -- https://github.com/MonoidMusician/blog/blob/274c1d64c12248773501e2df276cd305ecd94a14/PureScript/src/Parser/Comb/Types.purs#L38-L85
derive instance Functor Parser

data Codec a = Codec Int (Array CST -> PartialResult a)
derive instance Functor Codec

data PartialResult a
  = Failed
  | Partial
  | Result a
derive instance Functor PartialResult

-- to add context sensitivity, come up with an algebra of rejection functions:
--   Array CST -> { didItFailAlready :: Boolean }
-- and use referential identity to reduce the amount of work you have to do

-- To come up with the rules for the combined parser from <*>, you take the
-- Cartesian product of all the rules of the first one with all the rules of
-- the second one, and tell the codecs to look at the first/second part of the
-- `Array CST`.
-- lift2 append / lift2 apply
--
-- The LR parser stuff does not mind this combinatorial explosion.

instance Apply Parser where
  apply (Parser l) (Parser r) = Parser
    { rules: lift2 applyRule l.rules r.rules
    , grammar: l.grammar <> r.grammar
    -- ^ grammar and other structure just gets appended with monoids
    -- we deduplicate the grammar at the end,
    -- or you could use `Map` and require that names are orderable
    }
    where
    applyRule ll rr = { rule: ll.rule <> rr.rule, result: ll.result <*> rr.result }
instance Apply Codec where
  apply (Codec sl cl) (Codec sr cr) = Codec (sl + sr) splitCodec
    where
    splitCodec input = cl (Array.take sl input) <*> cr (Array.drop sl input)
instance Apply PartialResult where
  apply (Result f) (Result a) = Result (f a)
  apply Failed _ = Failed
  apply _ Failed = Failed
  apply _ _ = Partial


-- We want to produce a pretty grammar, so we ask the user to name rules before
-- they can recurse on them. (PureScript is strict, so there is no other way
-- to just embed parsers.)

-- namedRec :: Name -> (Parser a -> Parser a) -> Parser a


-- example JSON parser
-- https://github.com/MonoidMusician/blog/blob/main/PureScript/src/Parser/Languages.purs#L248-L257


{-
5. WIP context sensitivity
  - codec but algebra
  - e.g. Polish notation
    - benefits (compilation)
-}

-- selective applicative


{-
6. Lexing
see top of Parser.Lexing

https://github.com/MonoidMusician/blog/blob/274c1d64c12248773501e2df276cd305ecd94a14/PureScript/src/Parser/Lexing.purs#L84-L92
-}

-- | Typeclass for recognizing tokens based on categories.
-- |
-- | Law: if you recognized it, you must unrecognize it, and vice versa.
class (Token cat o) <= Tokenize cat i o | cat -> o where
  recognize :: cat -> i -> Maybe (Tuple o i)
  unrecognize :: cat -> o -> Maybe (i -> i)

class Token cat o | cat -> o where
  rerecognize :: cat -> o -> Boolean

{-
^ this gets interleaved with parsing, so you can have string tokens take
precedence over regexes that also matched for example, but only in situations
where that string token is actually a valid parse
-}



{-
7. Recursive?

:not(div.someClass, span[lang="en"])

CSS tells you to parse psuedo-functions as "anything"
and then recursively invoke the parser on that
(discarding failed parts, to be lenient)

so I added that kind of recursive invocation to my parser combinators
(this relies on the fact that you can get the input tokens out of the CST)

https://www.w3.org/TR/css-syntax-3/#any-value

-}


-- question about bidirectional codecs and
-- https://github.com/NorfairKing/autodocodec

-- answer: if you can implement bidirectional codecs for concrete syntax trees,
-- then printing it is very easy ... although nice formatting is a bit trickier


-- Other future work:
-- - Implement some sort of fusing to avoid the intermediate CST
--   (probably use Zippers to memoize the work)
-- - Figure out a better story for lexing whitespace, it is always so tricky
--   since if you encode it as a token it shows up in parser ambiguity, when
--   it has absolutely no bearing on the application types that result.
--
--   I am thinking of a type like `{ wsBefore :: Allowed, wsAfter :: Allowed }`
--   where `data Allowed = Required | Allowed | Disallowed`,
--   and then you have to combine them where they adjoin, and
--   `Required <> Disallowed` means that it will never parse.


-- bonus:
-- posts about my efforts to implement selective applicative parsing, and
-- other thoughts about selective applicative functors:
-- https://cohost.org/monoidmusician/tagged/selective%20applicative%20functor
