module Parser.Languages.HTML where

import Prelude

import Data.List (List)
import Data.List as List
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Dodo as Dodo
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones as D

escape :: String -> String
escape = identity
  >>> String.replaceAll (String.Pattern "&") (String.Replacement "&amp;")
  >>> String.replaceAll (String.Pattern "<") (String.Replacement "&lt;")
  >>> String.replaceAll (String.Pattern ">") (String.Replacement "&gt;")
  >>> String.replaceAll (String.Pattern "\"") (String.Replacement "&quot;")
  >>> String.replaceAll (String.Pattern "'") (String.Replacement "&#039;")

toHTML :: forall ann. (ann -> String) -> Dodo.Printer String ann String
toHTML f = Dodo.Printer
  { emptyBuffer: ""
  , writeText: \_ str buff -> buff <> escape str
  , writeIndent: \_ str buff -> buff <> str
  , writeBreak: \buff -> buff <> "\n"
  , enterAnnotation: \ann _ buff -> buff <> "<span class=\"" <> escape (f ann) <> "\">"
  , leaveAnnotation: \_ _ buff -> buff <> "</span>"
  , flushBuffer: \buff -> buff
  }

toDragon :: forall ann. (ann -> String) -> Dodo.Printer (List (Dragon /\ (Dragon -> Dragon)) /\ Dragon) ann Dragon
toDragon f = Dodo.Printer
  { emptyBuffer: mempty
  , writeText: \_ str (stack /\ buff) -> stack /\ (buff <> D.text str)
  , writeIndent: \_ str (stack /\ buff) -> stack /\ (buff <> D.text str)
  , writeBreak: \(stack /\ buff) -> stack /\ (buff <> D.text "\n")
  , enterAnnotation: \ann _ -> stackUp (D.span [ pure (D.className (f ann)) ])
  , leaveAnnotation: \_ _ -> case _ of
      List.Nil /\ buff -> List.Nil /\ buff
      List.Cons (suspended /\ frame) stack' /\ buff -> stack' /\ (suspended <> frame buff)
  , flushBuffer: \(_stack /\ buff) -> buff
  }
  where
  stackUp fn (stack /\ buff) = List.Cons (buff /\ fn) stack /\ mempty
