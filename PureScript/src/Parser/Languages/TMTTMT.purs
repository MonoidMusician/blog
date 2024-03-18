module Parser.Languages.TMTTMT where

import Data.Argonaut as Json
import Data.Either (Either)
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Parser.Comb.Comber (thaw)
import Parser.Languages.TMTTMT.Parser (declarationsP, typeP)
import Parser.Languages.TMTTMT.TypeCheck.Structural (Functional)
import Parser.Languages.TMTTMT.Types (Declaration)


mkTMTTMTParser :: Maybe String -> String -> String \/ Array (Either (Tuple String Functional) Declaration)
mkTMTTMTParser json = thaw declarationsP (Json.parseJson (fold json))

mkTMTTMTTypeParser :: Maybe String -> String -> String \/ Functional
mkTMTTMTTypeParser json = thaw typeP (Json.parseJson (fold json))
