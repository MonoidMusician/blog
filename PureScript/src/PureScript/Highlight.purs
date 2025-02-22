module PureScript.Highlight where

import Prelude

import Data.Array (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Dodo (Doc)
import Dodo as Dodo
import Parser.Languages.HTML (toHTML)
import PureScript.CST.Lexer (lex)
import PureScript.CST.Print as CST
import PureScript.CST.TokenStream (TokenStep(..), TokenStream, step)
import PureScript.CST.Types (Comment(..), Token(..), SourceToken)

highlight' :: String -> Doc String
highlight' = lex
  >>> printTokenStream
  >>> map toClass

highlight :: String -> String
highlight = highlight' >>> Dodo.print toHTML Dodo.twoSpaces


highlightPandoc :: String -> String
highlightPandoc = fold
  [ const """<div class="sourceCode"><pre class="sourceCode purescript"><code class="sourceCode purescript">"""
  , highlight
  , const """</code></pre></div>"""
  ]

highlightPandocInline :: String -> String
highlightPandocInline = fold
  [ const """<code class="sourceCode purescript">"""
  , highlight
  , const """</code>"""
  ]


data Category
  = CKeyword
  | CString
  | CSpecialChar
  | CSpecialString
  | CDataType
  | COther
  | COperator
  | CChar
  | CFunction
  | CAlert
  | CAnnotation
  | CAttribute
  | CBaseN
  | CBuiltIn
  | CControlFlow
  | CConstant
  | CComment
  | CDocumentation
  | CDecVal
  | CError
  | CExtension
  | CFloat
  | CImport
  | CInformation
  | CPreprocessor
  | CVariable
  | CVerbatimString
  | CWarning
  | CNone

derive instance genericCategory :: Generic Category _

instance showCategory :: Show Category where
  show = genericShow

toClass :: Category -> String
toClass = case _ of
  CKeyword -> "kw"
  CString -> "st"
  CSpecialChar -> "sc"
  CSpecialString -> "ss"
  CDataType -> "dt"
  COther -> "ot"
  COperator -> "op"
  CChar -> "ch"
  CFunction -> "fu"
  CAlert -> "al"
  CAnnotation -> "an"
  CAttribute -> "at"
  CBaseN -> "bn"
  CBuiltIn -> "bu"
  CControlFlow -> "cf"
  CConstant -> "cn"
  CComment -> "co"
  CDocumentation -> "do"
  CDecVal -> "dv"
  CError -> "er"
  CExtension -> "ex"
  CFloat -> "fl"
  CImport -> "im"
  CInformation -> "in"
  CPreprocessor -> "pp"
  CVariable -> "va"
  CVerbatimString -> "vs"
  CWarning -> "wa"
  CNone -> ""

classify :: Token -> Category
classify = case _ of
  TokLeftParen ->
    CNone
  TokRightParen ->
    CNone
  TokLeftBrace ->
    CNone
  TokRightBrace ->
    CNone
  TokLeftSquare ->
    CNone
  TokRightSquare ->
    CNone
  TokLeftArrow _ ->
    COther
  TokRightArrow _ ->
    COther
  TokRightFatArrow _ ->
    COther
  TokDoubleColon _ ->
    COther
  TokForall _ ->
    CKeyword
  TokEquals ->
    COther
  TokPipe ->
    COther
  TokTick ->
    COther
  TokDot ->
    COperator
  TokComma ->
    CNone
  TokUnderscore ->
    CNone
  TokBackslash ->
    CNone
  TokAt ->
    COther
  TokLowerName _ ident | Set.member ident reservedKeywords ->
    CKeyword
  TokLowerName _ _ ->
    CNone
  TokUpperName _ _ ->
    CDataType
  TokOperator _ _ ->
    COperator
  TokSymbolName _ _ ->
    COperator
  TokSymbolArrow _ ->
    COther
  TokHole _ ->
    CInformation
  TokChar _ _ ->
    CString
  TokString _ _ ->
    CString
  TokRawString _ ->
    CString
  TokInt _ _ ->
    CDecVal
  TokNumber _ _ ->
    CFloat
  TokLayoutStart _ ->
    CNone
  TokLayoutSep _ ->
    CNone
  TokLayoutEnd _ ->
    CNone



printTokenStream :: TokenStream -> Doc Category
printTokenStream = go mempty
  where
  go acc stream = case step stream of
    TokenEOF _ comments ->
      acc <> printComments comments
    TokenError _ err rest _ ->
      case rest of
        Just v -> go acc v
        _ -> acc
    TokenCons tok _ rest _ ->
      go (acc <> printSourceToken tok) rest

printSourceToken :: SourceToken -> Doc Category
printSourceToken tok =
  printComments tok.leadingComments
    <> printToken tok.value
    <> printComments tok.trailingComments

printComments :: forall a. Array (Comment a) -> Doc Category
printComments = foldMap printComment

printComment :: forall a. Comment a -> Doc Category
printComment = case _ of
  Comment str -> Dodo.annotate CComment $ Dodo.text str
  Space n -> power Dodo.space n
  Line _ n -> power Dodo.break n

printToken :: Token -> Doc Category
printToken tok =
  Dodo.annotate (classify tok) $
    Dodo.text (CST.printToken tok)




reservedKeywords :: Set.Set String
reservedKeywords = Set.fromFoldable
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "true"
  , "type"
  , "where"
  ]
