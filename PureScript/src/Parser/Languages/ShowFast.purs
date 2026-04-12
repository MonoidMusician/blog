module Parser.Languages.ShowFast where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Lazy (fix)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (fold, intercalate)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, oneOf)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.NonEmpty ((:|))
import Data.String.Regex.Flags (dotAll, unicode)
import Dodo (Doc)
import Dodo as T
import Parsing as Parsing
import Parsing.Combinators (try, (<?>))
import Parsing.Combinators.Array as Parsing.Array
import Parsing.String as Parsing.String
import Partial.Unsafe (unsafeCrashWith)

type Parser = Parsing.Parser String

string :: Parser String
string = rawr $ "\"" <> """([^"\\]+|\\.)*?""" <> "\""

char :: Parser String
char = rawr """'([^'\\]+|\\.)*?'"""

boring :: Parser String
boring = rawr """[^\][(){}"'\s,]+"""

sep :: Parser String
sep = rawr """[,]\s*"""

rawr :: String -> Parser String
rawr r = case Parsing.String.regex r (unicode <> dotAll) of
  Left err -> unsafeCrashWith err
  Right p -> p <?> "Expected to match regex " <> show r

token :: String -> Parser String
token = Parsing.String.string

parseShown :: Parser (Doc Void)
parseShown = fix \more -> do
  oneOf
    [ matched "(" more ")"
    , matched "[" more "]"
    , matched "{" more "}"
    , T.text <$> (string <|> char <|> boring)
    ] <* rawr "\\s*"
  where
  matched :: String -> Parser (Doc Void) -> String -> Parser (Doc Void)
  matched o more c =
    T.flexGroup <<< T.alignCurrentColumn <$> fold
      [ T.text <$> token o <* rawr "\\s*"
      , oneOf
          [ fold
            [ pure T.space
            , layers more
            , pure T.spaceBreak
            ]
          , pure mempty
          ]
      , T.text <$> token c
      ]

layers :: Parser (Doc Void) -> Parser (Doc Void)
layers more =
  separated "commas" (T.text ",") (rawr """[,]\s*""") $
    separated' "spaces" T.spaceBreak (rawr "\\s+") $
      more

indentAfter :: Array (Doc Void) -> Doc Void
indentAfter = A.uncons >>> foldMap
  \{ head, tail } -> head <> T.indent (A.fold tail)

separated :: forall x. String -> Doc Void -> Parser x -> Parser (Doc Void) -> Parser (Doc Void)
separated name docSep parseSep item = many1SepEndBy name parseSep item <#>
  (\items -> T.flexGroup <$> items)
  >>> NEA.toArray >>> intercalate (T.flexAlt docSep (T.break <> docSep) <> T.space)

separated' :: forall x. String -> Doc Void -> Parser x -> Parser (Doc Void) -> Parser (Doc Void)
separated' name docSep parseSep item = manySurBy name parseSep item <#>
  A.uncons >>> case _ of
    Nothing -> mempty
    Just { head, tail: [] } -> head
    Just { head, tail } ->
      head <> docSep <> T.indent (intercalate docSep tail)

manySurBy :: forall x a. String -> Parser x -> Parser a -> Parser (Array a)
manySurBy n s p = map NEA.toArray (many1SurBy n (map Just s <|> pure Nothing) p) <|> ([] <$ s)

many1SurBy :: forall x a. String -> Parser x -> Parser a -> Parser (NEA.NonEmptyArray a)
many1SurBy n s p = s *> many1SepBy n s p <* s

many1SepBy :: forall x a. String -> Parser x -> Parser a -> Parser (NEA.NonEmptyArray a)
many1SepBy n s p = lift2 NEA.cons' p (Parsing.Array.many (s *> p))

many1SepEndBy :: forall x a. String -> Parser x -> Parser a -> Parser (NEA.NonEmptyArray a)
many1SepEndBy n s p = do
  x0 <- p
  rlist <- flip tailRecM (x0 :| Nil) $ \(x1 :| xs) ->
    fromMaybe (Done (x1 :| xs)) <<< join <$> optional do
      s *> optional do
        x <- p
        pure (Loop (x :| x1 : xs))
  pure $ NEA.reverse $ NEA.fromFoldable1 rlist



lazyTop :: Parser (Doc Void)
lazyTop = layers parseShown <|> pure mempty

mkReShow :: Maybe String -> String -> T.PrintOptions -> String
mkReShow json =
  flip Parsing.runParser lazyTop
    >>> either (Parsing.parseErrorMessage >>> const) (flip (T.print T.plainText))
