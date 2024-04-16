module Parser.Main.TMTTMT where

import Prelude

import Control.Plus ((<|>))
import Data.Array (fold, intercalate)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Filterable (filterMap)
import Data.Foldable (oneOf)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, xdata, (!:=), (<:=>))
import Deku.Control (text_)
import Deku.Control as DC
import Deku.Core (Domable, bussed, envy)
import Deku.DOM as D
import Effect (Effect)
import Effect.Aff (Aff)
import FRP.Aff (affToEvent)
import FRP.Event (Event)
import FRP.Memoize (memoBeh, memoBehFold, memoLast)
import Fetch (fetch)
import Foreign.Object (Object)
import Foreign.Object as Object
import Idiolect ((>==>))
import Parser.Comb.Comber (lift2, many, parse, ws)
import Parser.Languages.TMTTMT (mkTMTTMTParser, mkTMTTMTTypeParser)
import Parser.Languages.TMTTMT.Eval (evalExpr, fromDeclarations, limit, printEvalError)
import Parser.Languages.TMTTMT.Parser (patternP, printExpr)
import Parser.Languages.TMTTMT.TypeCheck.Structural (Functional, isSubtype, printFunctional, run, synExpr, testExprs, testPatterns, testPatternsResult)
import Parser.Languages.TMTTMT.Types (Declaration(..))
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

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
  pure $ SafeNut do
    component stringInterface.send resetting

sendExample :: Widget
sendExample { interface, attrs } = do
  let push = (interface "tmttmt-example").send
  example <- attrs "example"
  pure $ SafeNut do
    D.button
      ( oneOf
          [ D.Class !:= ""
          , D.OnClick !:= push example
          ]
      )
      [ text_ "Try this example" ]

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

component :: forall lock payload. (String -> Effect Unit) -> Event String -> Domable lock payload
component setGlobal resetting =
  bussed \pushUpdate pushedRaw -> do
    envy $ memoLast (mkTMTTMTParser <$> affToEvent fetchParser) \getParser -> do
      let
        upd _ = case _ of
          Update v -> false /\ v
          Reset v -> true /\ v
      envy $ memoBehFold upd (true /\ "") (Reset <$> resetting <|> pushedRaw) \currentRaw -> do
        fold
          [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "Text")) $
            pure $ D.pre_ $ pure $ D.code_ $ pure $
              flip D.textarea [] $ oneOf
                [ D.OnInput !:= updateTA (pushUpdate <<< Update)
                , D.OnChange !:= updateTA setGlobal
                , D.Value <:=> resetting
                , D.Style !:= "height: 50vh"
                ]
          , D.pre (D.Class !:= "css" <|> D.Style !:= "white-space: break-spaces;")
              [ DC.text $ result <$> getParser <*> map snd currentRaw ]
          ]
  where
  updateTA upd = cb $ compose void $
    (Event.target >=> HTMLTextArea.fromEventTarget) >==>
      (HTMLTextArea.value >=> upd)

widgetTypeSplit :: Widget
widgetTypeSplit _ = do
  pure $ SafeNut do
    bussed \pushUpdate pushedRaw -> do
      bussed \pushPat pushedPat -> do
        envy $ memoLast (mkTMTTMTTypeParser <$> affToEvent fetchTypeParser) \getParser -> do
          envy $ memoBeh pushedRaw dfTy \currentRaw -> do
            envy $ memoBeh pushedPat dfPat \currentPat -> do
              fold
                [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "tmTTmt")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushUpdate)
                      , D.Value !:= dfTy
                      , D.Style !:= "height: 100px"
                      ]
                , D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "tmTTmt")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushPat)
                      , D.Value !:= dfPat
                      , D.Style !:= "height: 100px"
                      ]
                , D.pre (D.Class !:= "css" <|> D.Style !:= "white-space: break-spaces;; min-height: 150px")
                    [ DC.text $ result <$> getParser <*> currentRaw <*> currentPat ]
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
  result parser tyS patsS = case parser tyS `lift2 Tuple` parsePats patsS of
    Left r -> "Parse error: " <> r
    Right (Tuple ty pats) -> do
      testPatternsResult $ testPatterns pats ty

widgetSubType :: Widget
widgetSubType _ = do
  pure $ SafeNut do
    bussed \pushUpdate pushedRaw -> do
      bussed \pushPat pushedPat -> do
        envy $ memoLast (mkTMTTMTTypeParser <$> affToEvent fetchTypeParser) \getParser -> do
          envy $ memoBeh pushedRaw dfTy \currentRaw -> do
            envy $ memoBeh pushedPat dfPat \currentPat -> do
              fold
                [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "tmTTmt")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushUpdate)
                      , D.Value !:= dfTy
                      , D.Style !:= "height: 100px"
                      ]
                , D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "tmTTmt")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushPat)
                      , D.Value !:= dfPat
                      , D.Style !:= "height: 100px"
                      ]
                , D.pre (D.Class !:= "css" <|> D.Style !:= "white-space: break-spaces;; min-height: 150px")
                    [ DC.text $ result <$> getParser <*> currentRaw <*> currentPat ]
                ]
  where
  dfTy = """["x" "y"] | ["x" "x"] | ["y" "y"]"""
  dfPat = """[("x" | "y") ("x" | "y")]"""
  result parser tyS patsS = case parser tyS `lift2 Tuple` parser patsS of
    Left r -> "Parse error: " <> r
    Right (Tuple ty1 ty2) -> do
      case isSubtype ty1 ty2, isSubtype ty2 ty1 of
        true, false -> "<:"
        false, true -> ":>"
        true, true -> "~"
        false, false -> "/~"
