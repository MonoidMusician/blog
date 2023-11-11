module Parser.Languages.TMTTMT.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Monoid (power)
import Data.String as String
import Data.String.CodeUnits as CU
import Parser.Languages (Comber, delim, key, many, many1, many1SepBy, manySepBy, opt, rawr, ws, wsws, wsws', (#->), (#:), (<#?>), (>==))
import Parser.Languages.CSS (newline)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))

stringP :: Comber String
stringP = "string"#: oneOf
  [ join delim "\"" $ fold <$> many "double-string-contents" do
      rawr "[^\\\\\"\\n\\r\\f]+" <|> escapeP <|> key "\\" *> newline
  , join delim "\'" $ fold <$> many "single-string-contents" do
      rawr "[^\\\\'\\n\\r\\f]+" <|> escapeP <|> key "\\" *> newline
  ]

nameP :: Comber String
nameP = "name"#: rawr "[-a-zA-Z0-9_]+"

pathP :: Comber String
pathP = "path"#: rawr "[-a-zA-Z0-9_]+/" <#> CU.dropRight 1

data Token
  = TName String
  | TPath String
  | TString String
  | TLBracket
  | TRBracket
  | TLParen
  | TRParen
  | TDollar
  | TArrow
  | TBar
  | TColon
  | TExcl
  | TQuest

-- type Toker = Comb (Rec String (OrEOF String) String) String (String ~ Rawr) String

escapeP :: Comber String
escapeP = "escape"#: key "\\" *> oneOf
  [ rawr "[^0-9a-fA-F\\r\\n\\f]"
  , rawr "[0-9a-fA-F]{1,6}\\s?" <#?> do
      String.trim >>> Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
  ]

arrowedP :: forall a b c. (Array a -> b -> c) -> String -> Comber a -> Comber b -> Comber c
arrowedP c name aP bP = ado
  a <- many name aP
  key "=>" *> ws
  b <- bP
  in c a b

patternP :: Comber Pattern
patternP = "pattern"#-> \pat -> oneOf
  [ Var <$> nameP
  , Scalar <$> stringP
  , "vector"#: Vector <$> delim "[" "]" do
      ws *> many "patterns" pat
  , "macro"#: delim "(" ")" do
      ws *> key "$" *> ws *> ado
        { head, tail } <- NEA.uncons <$> many1 "patterns1" (Pattern <$> pat)
        in Macro head tail
  ] <* ws

wswsMany :: forall a. String -> Comber a -> Comber (Array a)
wswsMany name parser =
  [] <$ ws <|> NEA.toArray <$> do
    ws *> many1 name (parser <* ws)

exprP :: Comber Expr
exprP = "expr"#-> \exprL ->
  let
    lambdaCaseL :: Comber Case
    lambdaCaseL = "lambdaCase"#:
      Case <$> matchingL <*> thenConditionsL

    matchingL :: Comber Matching
    matchingL = "matching"#: arrowedP Matching "patterns" patternP exprL

    conditionL :: Comber Condition
    conditionL = "condition"#: Condition <$> exprL <*> callingL

    thenConditionsL :: Comber (Array Condition)
    thenConditionsL = oneOf
      [ NEA.toArray <$> do key ":" *> ws *> many1 "conditions1" conditionL
      , [] <$ do key ";" *> ws
      ]

    callingL :: Comber Calling
    callingL = "calling"#: arrowedP Calling "exprs" exprL patternP
  in oneOf
  [ Pattern <$> patternP
  , Lambda <$> ado
      key "?" *> ws
      cases <- many "lambdaCases" (key "|" *> ws *> lambdaCaseL)
      key "!" *> ws
      in cases
  ]

matchingP :: Comber Matching
matchingP = "matching"#: arrowedP Matching "patterns" patternP exprP

conditionP :: Comber Condition
conditionP = "condition"#: Condition <$> exprP <*> callingP

thenConditionsP :: Comber (Array Condition)
thenConditionsP = oneOf
  [ NEA.toArray <$> do key ":" *> ws *> many1 "conditions1" conditionP
  , [] <$ do key ";" *> ws
  ]

callingP :: Comber Calling
callingP = "calling"#: arrowedP Calling "exprs" exprP patternP

topCaseP :: Comber Case
topCaseP = "topCase"#: Case <$> matchingP <*> thenConditionsP

declarationsP :: Comber (Array Declaration)
declarationsP = many "declarations" $ "declaration"#: oneOf
  [ ado
    fnName <- pathP
    caseName <- many "paths" pathP
    ws
    desc <- topCaseP
    in DefineCase fnName caseName desc
  , Comment <$> do (key "--" <|> key "//") *> rawr "[^\\r\\n]*"
  , Query <$> ado
      key ">" *> ws
      { head, tail } <- NEA.uncons <$> many1 "exprs1" exprP
      in Pattern (Macro head tail)
  ]

printExpr :: Expr -> String
printExpr = printExpr' 0

printExpr' :: Int -> Expr -> String
printExpr' _ (Lambda []) = "?!"
printExpr' depth (Lambda cases) = A.fold
  [ cases # A.foldMap \(Case (Matching pats result) conds) -> do
      A.fold
        [ "| "
        , pats # A.foldMap \p -> printPattern' (depth+1) p <> " "
        , "=> "
        , printExpr' (depth+1) result
        , conds # A.foldMap \(Condition fn (Calling args pat)) ->
            A.fold
              [ "\n" <> power "  " (depth+1)
              , printExpr' (depth+1) fn
              , " "
              , args # A.foldMap \arg -> printExpr' (depth+1) arg <> " "
              , "=> "
              , printPattern' (depth+1) pat
              ]
        , "\n" <> power "  " depth
        ]
  , "!"
  ]
printExpr' depth (Pattern p) = printPattern' depth p

printPattern' :: Int -> Pattern -> String
printPattern' _ (Var v) = v
printPattern' _ (Scalar s) = show s
printPattern' depth (Vector vs) = "[" <> A.intercalate " " (map (printPattern' (depth+1)) vs) <> "]"
printPattern' depth (Macro fn args) = A.fold
  [ "($"
  , printExpr' (depth+1) fn
  , args # A.foldMap \arg -> " " <> printExpr' (depth+1) arg
  , ")"
  ]
