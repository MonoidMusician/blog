module Parser.Languages.TMTTMT where

import Prelude

import Data.Argonaut as Json
import Data.Codec as CA
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Parser.Comb (execute, parseRegex)
import Parser.Comb.Combinators (buildTree)
import Parser.Comb.Run (resultantsOf)
import Parser.Languages (mainName)
import Parser.Languages.CSS (codec)
import Parser.Languages.TMTTMT.Parser (declarationsP)
import Parser.Languages.TMTTMT.Types (Declaration)
import Parser.Lexing (bestRegexOrString)


mkTMTTMTParser :: Maybe String -> String -> String \/ Array Declaration
mkTMTTMTParser (Just json)
  | Right (Right states) <- CA.decode codec <$> Json.parseJson json =
    execute { best: bestRegexOrString } { states, resultants: mainName /\ resultantsOf declarationsP, options: buildTree mainName declarationsP }
mkTMTTMTParser _ = parseRegex mainName declarationsP
