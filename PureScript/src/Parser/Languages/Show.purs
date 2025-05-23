module Parser.Languages.Show where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Argonaut as Json
import Data.Array (fold, intercalate)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Dodo (Doc)
import Dodo as T
import Parser.Comb.Comber (Comber, choices, many1SepBy, rawr, thawWith, token, (#->))
import Parser.Lexing as L
import Whitespace (defaultWS)

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
  choices
    [ T.text <$> (string <|> char <|> boring)
    , matched "(" more ")"
    , matched "[" more "]"
    , matched "{" more "}"
    ]
  where
  matched :: String -> Comber (Doc Void) -> String -> Comber (Doc Void)
  matched o more c =
    T.flexGroup <<< T.alignCurrentColumn <$> fold
      [ T.text <$> token o
      , choices
          [ fold
            [ pure T.space
            , layers more
            , pure T.spaceBreak
            ]
          , pure mempty
          ]
      , T.text <$> token c
      ]

layers :: Comber (Doc Void) -> Comber (Doc Void)
layers more =
  separated "commas" (T.text ",") (rawr """[,]\s*""") $
    separated' "spaces" T.spaceBreak (rawr "\\s+") $
      more

indentAfter :: Array (Doc Void) -> Doc Void
indentAfter = A.uncons >>> foldMap
  \{ head, tail } -> head <> T.indent (A.fold tail)

separated :: forall x. String -> Doc Void -> Comber x -> Comber (Doc Void) -> Comber (Doc Void)
separated name docSep parseSep item = many1SepBy name parseSep item <#>
  (\items -> T.flexGroup <$> items)
  >>> NEA.toArray >>> intercalate (T.flexAlt docSep (T.break <> docSep) <> T.space)

separated' :: forall x. String -> Doc Void -> Comber x -> Comber (Doc Void) -> Comber (Doc Void)
separated' name docSep parseSep item = manySurBy name parseSep item <#>
  A.uncons >>> case _ of
    Nothing -> mempty
    Just { head, tail: [] } -> head
    Just { head, tail } ->
      head <> docSep <> T.indent (intercalate docSep tail)

manySurBy :: forall x a. String -> Comber x -> Comber a -> Comber (Array a)
manySurBy n s p = ([] <$ s) <|> map NEA.toArray (many1SurBy n (map Just s <|> pure Nothing) p)

many1SurBy :: forall x a. String -> Comber x -> Comber a -> Comber (NEA.NonEmptyArray a)
many1SurBy n s p = (\ps -> s *> ps) $
  NEA.fromFoldable1 <$> n #-> \more ->
    lift2 NEL.cons' p (s *> (pure Nil <|> NEL.toList <$> more))

lazyTop :: Comber (Doc Void)
lazyTop = layers parseShown <|> pure mempty

mkReShow :: Maybe String -> String -> T.PrintOptions -> String
mkReShow json =
  thawWith { best: L.lazyBest, defaultSpace: defaultWS } lazyTop (Json.parseJson (fromMaybe "" json))
    >>> either const (flip (T.print T.plainText))
