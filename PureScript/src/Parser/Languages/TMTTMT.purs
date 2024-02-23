module Parser.Languages.TMTTMT where

import Data.Argonaut as Json
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Maybe (Maybe)
import Parser.Comb.Comber (thaw)
import Parser.Languages.TMTTMT.Parser (declarationsP, typeP)
import Parser.Languages.TMTTMT.TypeCheck.Structural (Functional)
import Parser.Languages.TMTTMT.Types (Declaration)


mkTMTTMTParser :: Maybe String -> String -> String \/ Array Declaration
mkTMTTMTParser json = thaw declarationsP (Json.parseJson (fold json))

mkTMTTMTTypeParser :: Maybe String -> String -> String \/ Functional
mkTMTTMTTypeParser json = thaw typeP (Json.parseJson (fold json))
