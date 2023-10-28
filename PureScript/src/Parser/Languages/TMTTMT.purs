module Parser.Languages.TMTTMT where

import Prelude

import Control.Alt ((<|>))
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.String as String
import Parser.Languages (Comber, delim, key, many, manySepBy, rawr, ws, wsws, (#->), (#:), (<#?>), (>==))
import Parser.Languages.CSS (newline)

string :: Comber String
string = "string"#: oneOf
  [ join delim "\"" $ fold <$> many "double-string-contents" do
      rawr "[^\\\\\"\\n\\r\\f]+" <|> escape <|> key "\\" *> newline
  , join delim "\'" $ fold <$> many "single-string-contents" do
      rawr "[^\\\\'\\n\\r\\f]+" <|> escape <|> key "\\" *> newline
  ]

escape :: Comber String
escape = "escape"#: key "\\" *> oneOf
  [ rawr "[^0-9a-fA-F\\r\\n\\f]"
  , rawr "[0-9a-fA-F]{1,6}\\s?" <#?> do
      String.trim >>> Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
  ]

data Pattern
  = Var String
  | Scalar String
  | Vector (Array Pattern)
  | Macro (Array Expr)

data Expr
  = Pattern Pattern
  -- | Lambda (Array Case)

pattern :: Comber Pattern
pattern = "pattern"#-> \pat -> oneOf
  [ Var <$> rawr "[-a-zA-Z0-9_]+"
  , Scalar <$> string
  , Vector <$> delim "[" "]" do
      wsws $ manySepBy "patterns" ws pat
  , Macro <$> delim "(" ")" do
      wsws $ key "$" *> ws *> manySepBy "exprs" ws (Pattern <$> pat)
  ]
