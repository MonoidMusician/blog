module Parser.Languages.Show where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (fold, intercalate)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (ltraverse)
import Data.Either (either, isLeft)
import Data.Foldable (foldMap, oneOf)
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Dodo (Doc)
import Dodo as D
import Parser.Comb (parseWith)
import Parser.Languages (Comber, key, many1SepBy, rawr, (#->))
import Parser.Lexing (Best, Rawr, Similar(..), len, prioritize)
import Parser.Types (OrEOF(..), decide)

string :: Comber String
string = rawr $ "\"" <> """([^"\\]+|\\.)*?""" <> "\""

char :: Comber String
char = rawr """'([^'\\]+|\\.)*?'"""

boring :: Comber String
boring = rawr """[^\][(){}"'\s,]+"""

sep :: Comber String
sep = rawr """[,]\s*"""

parseShown :: Comber (Doc Void)
parseShown = "stuff" #-> \more -> do
  oneOf
    [ D.text <$> (string <|> char <|> boring)
    , matched "(" more ")"
    , matched "[" more "]"
    , matched "{" more "}"
    ]
  where
  matched :: String -> Comber (Doc Void) -> String -> Comber (Doc Void)
  matched o more c =
    D.flexGroup <<< D.alignCurrentColumn <$> fold
      [ D.text <$> key o
      , oneOf
          [ fold
            [ pure D.space
            , layers more
            , pure D.spaceBreak
            ]
          , pure mempty
          ]
      , D.text <$> key c
      ]

layers :: Comber (Doc Void) -> Comber (Doc Void)
layers more =
  separated "commas" (D.text ",") (rawr """[,]\s*""") $
    separated' "spaces" D.spaceBreak (rawr "\\s+") $
      more

indentAfter :: Array (Doc Void) -> Doc Void
indentAfter = A.uncons >>> foldMap
  \{ head, tail } -> head <> D.indent (A.fold tail)

separated :: forall x. String -> Doc Void -> Comber x -> Comber (Doc Void) -> Comber (Doc Void)
separated name docSep parseSep item = many1SepBy name parseSep item <#>
  (\items -> D.flexGroup <$> items)
  >>> NEA.toArray >>> intercalate (D.flexAlt docSep (D.break <> docSep) <> D.space)

separated' :: forall x. String -> Doc Void -> Comber x -> Comber (Doc Void) -> Comber (Doc Void)
separated' name docSep parseSep item = manySurBy name parseSep item <#>
  A.uncons >>> case _ of
    Nothing -> mempty
    Just { head, tail: [] } -> head
    Just { head, tail } ->
      head <> docSep <> D.indent (intercalate docSep tail)

manySurBy :: forall x a. String -> Comber x -> Comber a -> Comber (Array a)
manySurBy n s p = ([] <$ s) <|> map NEA.toArray (many1SurBy n (map Just s <|> pure Nothing) p)

many1SurBy :: forall x a. String -> Comber x -> Comber a -> Comber (NEA.NonEmptyArray a)
many1SurBy n s p = (\ps -> s *> ps) $
  NEA.fromFoldable1 <$> n #-> \more ->
    lift2 NEL.cons' p (s *> (pure Nil <|> NEL.toList <$> more))

reShow :: String -> String
reShow = parseWith { best: lazyBest } "main" (layers parseShown <|> pure mempty)
  >>> either identity (D.print D.plainText D.twoSpaces)

lazyBest :: forall s r. Best s r (OrEOF (Similar String Rawr)) (OrEOF String) (OrEOF String)
lazyBest = ltraverse decide <<< NEA.head <<< prioritize case _ of
  (_ /\ (Continue (Similar cat)) /\ _ /\ Continue i) -> Just $ Tuple (isLeft cat) (negate (len i))
  _ -> Nothing
