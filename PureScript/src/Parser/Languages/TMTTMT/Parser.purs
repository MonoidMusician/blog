module Parser.Languages.TMTTMT.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Foldable (fold, oneOf)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Monoid (power)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Idiolect ((<#?>), (>==))
import Parser.Comb.Comber (Comber, delim, many, many1, many1SepBy, mutual, opt, rawr, token, ws, wsws, (#->), (#:))
import Parser.Languages.CSS (newline)
import Parser.Languages.TMTTMT.TypeCheck.Structural (BasicSubsetF(..), Functional(..))
import Parser.Languages.TMTTMT.Types (Calling(..), Case(..), Condition(..), Declaration(..), Expr(..), Matching(..), Pattern(..))

tyList :: Functional -> Functional
tyList t = Union [ Concrete (Tupled []), tyNEList t ]
tyNEList :: Functional -> Functional
tyNEList = Concrete <<< ListOf
tyString :: Functional
tyString = Union [ Concrete (Singleton ""), Concrete AnyScalar ]

typePG ::
  { typeFunctionP :: Comber Functional
  , typeAtomP :: Comber Functional
  , typePrefixP :: Comber Functional
  , typeUnionP :: Comber Functional
  }
typePG = mutual \{ typeAtomP, typePrefixP, typeUnionP, typeFunctionP } ->
  { typeAtomP: oneOf
      [ TyVar <$> nameP
      , tyString <$ token "$"
      , Concrete AnyScalar <$ token "$$"
      , Union [] <$ delim "(" ")" (wsws $ token "|")
      , delim "(" ")" typeFunctionP
      , Concrete <<< Tupled <$> delim "[" "]" (many "typeAtoms" typeAtomP)
      , Concrete <<< Singleton <$> stringP
      ] <* ws
  , typePrefixP: oneOf
      [ (tyList <$ token "*" <* ws) <*> typePrefixP
      , (tyNEList <$ token "+" <* ws) <*> typePrefixP
      , typeAtomP
      ]
  , typeUnionP:
      Union <<< NEA.toArray <$> do
        opt (token "|" *> ws) *> many1SepBy "typeUnion" (token "|" *> ws) do
          {- many1 "typeApps" -} typePrefixP
  , typeFunctionP: oneOf
      [ typeUnionP
      , Function <$> typeUnionP <* token "->" <* ws <*> typeFunctionP
      ]
  }

typeP :: Comber Functional
typeP = typePG.typeFunctionP
-- typeP = typePG.typeAtomP *> typePG.typePrefixP *> delim "{" "}" typePG.typeUnionP *> typePG.typeFunctionP

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

-- FIXME
nameWithTypeP :: Comber String
nameWithTypeP = "nameWithType"#: rawr "[-a-zA-Z0-9_]+\\s*:" <#>
  CU.dropRight 1 >>> String.trim

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

declarationsP :: Comber (Array (Either (Tuple String Functional) Declaration))
declarationsP = many "declarations" $ "declaration"#: oneOf
  [ Left <$> ado
      fnName <- nameWithTypeP
      ws
      ty <- typeP
      in Tuple fnName ty
  , Right <$> oneOf
    [ ado
      fnName <- pathP
      caseName <- many "paths" pathP
      ws
      desc <- topCaseP
      in DefineCase fnName caseName desc
    , Comment <$> do (token "--" <|> token "//") *> rawr "[^\\r\\n]*" <* ws
    , Query <$> ado
        token ">" *> ws
        { head, tail } <- NEA.uncons <$> many1 "exprs1" exprP
        in Pattern (Macro head tail)
    ]
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

printPattern :: Pattern -> String
printPattern = printPattern' 0

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
