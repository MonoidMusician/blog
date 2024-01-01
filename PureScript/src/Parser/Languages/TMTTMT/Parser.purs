module Parser.Languages.TMTTMT.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Monoid (power)
import Data.String as String
import Data.String.CodeUnits as CU
import Idiolect ((<#?>), (>==))
import Parser.Comb.Comber (Comber, delim, many, many1, mutual, rawr, token, ws, (#->), (#:))
import Parser.Languages.CSS (newline)
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))

stringP :: Comber String
stringP = "string"#: oneOf
  [ join delim "\"" $ fold <$> many "double-string-contents" do
      rawr "[^\\\\\"\\n\\r\\f]+" <|> escapeP <|> token "\\" *> newline
  , join delim "\'" $ fold <$> many "single-string-contents" do
      rawr "[^\\\\'\\n\\r\\f]+" <|> escapeP <|> token "\\" *> newline
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
escapeP = "escape"#: token "\\" *> oneOf
  [ rawr "[^0-9a-fA-F\\r\\n\\f]"
  , rawr "[0-9a-fA-F]{1,6}\\s?" <#?> do
      String.trim >>> Int.fromStringAs hexadecimal >=> toEnum >== String.singleton
  ]

arrowedP :: forall a b c. (Array a -> b -> c) -> String -> Comber a -> Comber b -> Comber c
arrowedP c name aP bP = ado
  a <- many name aP
  token "=>" *> ws
  b <- bP
  in c a b

patternP :: Comber Pattern
patternP = "pattern"#-> \pat -> oneOf
  [ Var <$> nameP
  , Scalar <$> stringP
  , "vector"#: Vector <$> delim "[" "]" do
      ws *> many "patterns" pat
  , "macro"#: delim "(" ")" do
      ws *> token "$" *> ws *> ado
        { head, tail } <- NEA.uncons <$> many1 "patterns1" (Pattern <$> pat)
        in Macro head tail
  ] <* ws

wswsMany :: forall a. String -> Comber a -> Comber (Array a)
wswsMany name parser =
  [] <$ ws <|> NEA.toArray <$> do
    ws *> many1 name (parser <* ws)

exprPG ::
  { callingP :: Comber Calling
  , conditionP :: Comber Condition
  , exprP :: Comber Expr
  , lambdaCaseP :: Comber Case
  , matchingP :: Comber Matching
  , thenConditionsP :: Comber (Array Condition)
  }
exprPG =
  mutual \{ exprP, lambdaCaseP, matchingP, conditionP, thenConditionsP, callingP } ->
    { exprP: oneOf
        [ Pattern <$> patternP
        , Lambda <$> ado
            token "?" *> ws
            cases <- many "lambdaCases" (token "|" *> ws *> lambdaCaseP)
            token "!" *> ws
            in cases
        ]
    , lambdaCaseP: "lambdaCase"#:
        Case <$> matchingP <*> thenConditionsP
    , matchingP: "matching"#: arrowedP Matching "patterns" patternP exprP
    , conditionP: "condition"#: Condition <$> exprP <*> callingP
    , thenConditionsP: oneOf
        [ NEA.toArray <$> do token ":" *> ws *> many1 "conditions1" conditionP
        , [] <$ do token ";" *> ws
        ]
    , callingP: "calling"#: arrowedP Calling "exprs" exprP patternP
    }

exprP :: Comber Expr
exprP = exprPG.exprP
lambdaCaseP :: Comber Case
lambdaCaseP = exprPG.lambdaCaseP
matchingP :: Comber Matching
matchingP = exprPG.matchingP
conditionP :: Comber Condition
conditionP = exprPG.conditionP
thenConditionsP :: Comber (Array Condition)
thenConditionsP = exprPG.thenConditionsP
callingP :: Comber Calling
callingP = exprPG.callingP

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
  , Comment <$> do (token "--" <|> token "//") *> rawr "[^\\r\\n]*"
  , Query <$> ado
      token ">" *> ws
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
