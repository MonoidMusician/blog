module Parser.Languages.HTML where

import Prelude

import Data.String as String
import Dodo as Dodo

escape :: String -> String
escape = identity
  >>> String.replaceAll (String.Pattern "&") (String.Replacement "&amp;")
  >>> String.replaceAll (String.Pattern "<") (String.Replacement "&lt;")
  >>> String.replaceAll (String.Pattern ">") (String.Replacement "&gt;")
  >>> String.replaceAll (String.Pattern "\"") (String.Replacement "&quot;")
  >>> String.replaceAll (String.Pattern "'") (String.Replacement "&#039;")

toHTML :: Dodo.Printer String String String
toHTML = Dodo.Printer
  { emptyBuffer: ""
  , writeText: \_ str buff -> buff <> escape str
  , writeIndent: \_ str buff -> buff <> str
  , writeBreak: \buff -> buff <> "\n"
  , enterAnnotation: \ann _ buff -> buff <> "<span class=\"" <> escape ann <> "\">"
  , leaveAnnotation: \_ _ buff -> buff <> "</span>"
  , flushBuffer: \buff -> buff
  }
