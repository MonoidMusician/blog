module Parser.Main.TMTTMT where

import Prelude

import Control.Plus ((<|>))
import Data.Array (fold, intercalate)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Idiolect ((/|\))
import Parser.Comb.Comber (many, parse, ws)
import Parser.Languages.TMTTMT (mkTMTTMTParser, mkTMTTMTTypeParser)
import Parser.Languages.TMTTMT.Eval (evalExpr, fromDeclarations, limit, printEvalError)
import Parser.Languages.TMTTMT.Parser (patternP, printExpr)
import Parser.Languages.TMTTMT.TypeCheck.Structural (Functional, isSubtype, printFunctional, run, synExpr, testExprs, testPatterns, testPatternsResult)
import Parser.Languages.TMTTMT.Types (Declaration(..))
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (Lake, createStream, createStreamStore, foldStream)
import Riverdragon.River.Beyond (affToLake)
import Widget (Widget, adaptInterface)

type Parser = String -> Either String (Array (Either (Tuple String Functional) Declaration))

widgets :: Object Widget
widgets = Object.fromFoldable $
  [ "Parser.Main.TMTTMT" /\ widgetTMTTMT
  , "Parser.Main.TMTTMT.TypeSplit" /\ widgetTypeSplit
  , "Parser.Main.TMTTMT.SubType" /\ widgetSubType
  ]

examples :: Map String String
examples = Map.fromFoldable
  [ "eval-examples" /\ """// Some basic tests of functions

// Typed with overly specific types
// for the sake of example, since
// there is no polymorphism yet
uncurryS: ($ -> $ -> [$ $]) -> [$ $] -> [$ $]
uncurryS/ f1 [a b] => r1:
  f1 a b => r1

// Higher order functions are a hack
// They need distinct variables
// (Sorry sorry sorry)
curryS: ([$ $] -> [$ $]) -> $ -> $ -> [$ $]
curryS/ f2 c d => r2:
  f2 [c d] => r2

identity: [$ $] -> [$ $]
identity/ x => x;

tuple: $ -> $ -> [$ $]
tuple/ y z => [y z];

test: [$] -> [$] -> [$ $]
test/ [g] [h] => [g h];

tset: [[$] [$]] -> [$ $]
tset/ [[i] [j]] => [i j];

// Examples
> uncurryS tuple ["0" "1"]
> uncurryT test [["2"] ["3"]]
> curryS identity "4" "5"
> curryT tset ["6"] ["7"]
> uncurryS ($curryS identity) ["8" "9"]



// For testing untupling
uncurryT: ([$] -> [$] -> [$ $]) -> [[$] [$]] -> [$ $]
uncurryT/ f1 [a b] => r1:
  f1 a b => r1

curryT: ([[$] [$]] -> [$ $]) -> [$] -> [$] -> [$ $]
curryT/ f2 c d => r2:
  f2 [c d] => r2
"""
  -----------------------------
  , "typecheck-examples" /\ """// A simple value: a tuple of 3 strings
// (the first must be empty,
// the third cannot be)
example0: ["" $ $$]
example0/ => ["" "v" "w"];

example1:
  "" | "x" | "y" ->
  [("x" | "y") ("x" | "y")]
// Individual cases can be labelled
example1/empty-case-xy/ "" => ["x" "y"];
example1/x-case/ "x" => ["x" "x"];
example1/y-case/ "y" => ["y" "y"];
// Note that a more specific
// return type would be
// `["x" "y"] | ["x" "x"] | ["y" "y"]`.
// Try it out!

// This one is recursive and
// demonstrates subtyping!
example2:
  | [("x" | "y")]
  | "x"
  | "y"
    ->
  | "x"
  | "y"
example2/nested/ [xy] => r:
  example2 xy => r
example2/x/ "x" => "x";
example2/y/ "y" => "y";
"""
  ]

widgetTMTTMT :: Widget
widgetTMTTMT { interface, attrs } = do
  example <- attrs "example"
  let
    initial = case CA.decode CA.string example of
      Left _ -> ""
      Right k | Just v <- Map.lookup k examples -> v
      Right v -> v
    stringInterface = adaptInterface CA.string (interface "tmttmt-example")
    resetting = pure initial <|> stringInterface.receive
  pure $ component stringInterface.send  resetting

sendExample :: Widget
sendExample { interface, attrs } = do
  let push = (interface "tmttmt-example").send
  example <- attrs "example"
  pure $ D.buttonN "" (push example) "Try this example"

result :: Parser -> String -> String
result = (<<<) case _ of
  Right decls ->
    let
      evalCtx = fromDeclarations (filterMap hush decls)
      tcExprs = testExprs decls
      tcEnv = case tcExprs of
        Left _ -> Map.empty
        Right defs -> defs <#> _.ty
      queries = decls # filterMap case _ of
        Right (Query e) -> Just e
        _ -> Nothing
      evalResults = queries <#> \e0 ->
        fold
          [ case limit 2000 (evalExpr evalCtx e0) of
              Right e -> printExpr e
              Left e -> printEvalError e
          , fold case run tcEnv $ synExpr e0 of
              Left r ->
                [ " // ", testPatternsResult (Left r) ]
              Right r ->
                [ " (: ", printFunctional r, " :)"]
          ]
    in fold
      [ case tcExprs of
          Left r -> testPatternsResult (Left r)
          Right defs | Map.isEmpty defs -> "No definitions"
          Right defs -> fold
            [ "Definitions typecheck:\n"
            , defs # foldMapWithIndex \name { ty } -> do
                "\n  - " <> name <> ": " <> printFunctional ty
            ]
      , "\n\n"
      , case queries of
          [] -> "Use `>` to evaluate expressions"
          _ -> fold
            [ "Evaluation results:\n"
            , intercalate "\n" evalResults
            ]
      ]
  Left failure -> "Parse error: " <> failure

data Update
  = Update String
  | Reset String

fetchParser :: Aff String
fetchParser = _.text =<< fetch "assets/json/tmttmt-parser-states.json" {}

fetchTypeParser :: Aff String
fetchTypeParser = _.text =<< fetch "assets/json/tmttmt-types-parser-states.json" {}

component :: (String -> Effect Unit) -> Lake String -> Dragon
component setGlobal resetting = eggy \shell -> do
  { stream: pushedRaw, send: pushUpdate } <- shell.track createStream
  getParser <- shell.inst $ mkTMTTMTParser <$> affToLake fetchParser
  currentRaw <- shell.inst $
    foldStream (true /\ "") (Reset <$> resetting <|> pushedRaw)
      \_ -> case _ of
        Update v -> false /\ v
        Reset v -> true /\ v
  pure $ Fragment
    [ D.div' [ D.className "sourceCode unicode", D.data_"lang" "Text" ]
        $ D.pre $ D.code $ D.textarea'
            [ D.onInputValue (pushUpdate <<< Update)
            , D.onChangeValue setGlobal
            , D.value' resetting
            , D.style "height: 50vh"
            ]
    , D.pre' [ D.className "css", D.style "white-space: break-spaces;" ]
        $ Text $ result <$> getParser <*> map snd currentRaw
    ]

widgetTypeSplit :: Widget
widgetTypeSplit _ = pure $ eggy \shell -> do
  { stream: pushedRaw, send: pushUpdate } <- shell.track $ createStreamStore $ Just dfTy
  { stream: pushedPat, send: pushPat } <- shell.track $ createStreamStore $ Just dfPat
  getParser <- shell.inst $ mkTMTTMTTypeParser <$> affToLake fetchTypeParser
  pure $ Fragment
    [ D.div' [ D.className "sourceCode unicode", D.data_"lang" "tmTTmt" ] $
        D.pre $ D.code $ D.textarea'
          [ D.onInputValue pushUpdate
          , D.value dfTy
          , D.style "height: 100px"
          ]
    , D.div' [ D.className "sourceCode unicode", D.data_"lang" "tmTTmt" ] $
        D.pre $ D.code $ D.textarea'
          [ D.onInputValue pushPat
          , D.value dfPat
          , D.style "height: 100px"
          ]
    , D.pre' [ D.className "css", D.style "white-space: break-spaces; min-height: 150px" ] $
        Text $ typeResult <$> getParser <*> pushedRaw <*> pushedPat
    ]
  where
  dfTy = """"A" | ["" $ $$] | "B" | *[]"""
  dfPat = """[]
[h i j k l]
[[] b c]
"B"
[a "" c]
[x [] x]
"A"
[a b c]
[[] [] [] []]"""
  parsePats = parse $ ws *> many "patterns" patternP
  typeResult parser tyS patsS = case parser tyS /|\ parsePats patsS of
    Left r -> "Parse error: " <> r
    Right (Tuple ty pats) -> do
      testPatternsResult $ testPatterns pats ty

widgetSubType :: Widget
widgetSubType _ = pure $ eggy \shell -> do
  { stream: pushedRaw, send: pushUpdate } <- shell.track $ createStreamStore $ Just dfTy
  { stream: pushedPat, send: pushPat } <- shell.track $ createStreamStore $ Just dfPat
  getParser <- shell.inst $ mkTMTTMTTypeParser <$> affToLake fetchTypeParser
  pure $ Fragment
    [ D.div' [ D.className "sourceCode unicode", D.data_"lang" "tmTTmt" ] $
        D.pre $ D.code $ D.textarea'
          [ D.onInputValue pushUpdate
          , D.value dfTy
          , D.style "height: 100px"
          ]
    , D.div' [ D.className "sourceCode unicode", D.data_"lang" "tmTTmt" ] $
        D.pre $ D.code $ D.textarea'
          [ D.onInputValue pushPat
          , D.value dfPat
          , D.style "height: 100px"
          ]
    , D.pre' [ D.className "css", D.style "white-space: break-spaces; min-height: 150px" ] $
        Text $ typeResult <$> getParser <*> pushedRaw <*> pushedPat
    ]
  where
  dfTy = """["x" "y"] | ["x" "x"] | ["y" "y"]"""
  dfPat = """[("x" | "y") ("x" | "y")]"""
  typeResult parser tyS patsS = case parser tyS /|\ parser patsS of
    Left r -> "Parse error: " <> r
    Right (Tuple ty1 ty2) -> do
      case isSubtype ty1 ty2, isSubtype ty2 ty1 of
        true, false -> "<:"
        false, true -> ":>"
        true, true -> "~"
        false, false -> "/~"
