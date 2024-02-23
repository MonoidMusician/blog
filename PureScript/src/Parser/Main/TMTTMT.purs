module Parser.Main.TMTTMT where

import Prelude

import Control.Plus ((<|>))
import Data.Array (fold, intercalate)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either, hush)
import Data.Filterable (filterMap)
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Debug (spy)
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
import Idiolect (intercalateMap, (>==>))
import Parser.Comb.Comber (lift2, parse)
import Parser.Languages.TMTTMT (mkTMTTMTParser, mkTMTTMTTypeParser)
import Parser.Languages.TMTTMT.Eval (evalExpr, fromDeclarations)
import Parser.Languages.TMTTMT.Parser (patternP, printExpr)
import Parser.Languages.TMTTMT.TypeCheck.Structural (BasicSubsetF(..), Functional(..), testPattern, testPatternResult)
import Parser.Languages.TMTTMT.Types (Declaration(..))
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

type Parser = String -> Either String (Array Declaration)

widgets :: Object Widget
widgets = Object.fromFoldable $
  [ "Parser.Main.TMTTMT" /\ widgetTMTTMT
  , "Parser.Main.TMTTMT.TypeSplit" /\ widgetTypeSplit
  ]

widgetTMTTMT :: Widget
widgetTMTTMT { interface, attrs } = do
  example <- attrs "example"
  let
    initial = hush $ CA.decode CA.string example
    stringInterface = adaptInterface CA.string (interface "tmttmt-example")
    resetting = oneOfMap pure initial <|> stringInterface.receive
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
      ctx = fromDeclarations decls
      queries = decls # filterMap case _ of
        Query e -> Just e
        _ -> Nothing
      results = queries <#> evalExpr ctx >>> maybe "Failed." printExpr
    in intercalate "\n" results
  Left failure -> failure

data Update
  = Update String
  | Reset String

fetchParser :: Aff String
fetchParser = _.text =<< fetch "assets/json/tmttmt-parser-states.json" {}

fetchTypeParser :: Aff String
fetchTypeParser = _.text =<< fetch "assets/json/tmttmt-type-parser-states.json" {}

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
          , D.pre (D.Class !:= "css" <|> D.Style !:= "text-wrap: wrap")
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
                [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "TMTTMT")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushUpdate)
                      , D.Value !:= dfTy
                      , D.Style !:= "height: 100px"
                      ]
                , D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "TMTTMT")) $
                  pure $ D.pre_ $ pure $ D.code_ $ pure $
                    flip D.textarea [] $ oneOf
                      [ D.OnInput !:= do
                          cb $ compose void $
                            (Event.target >=> HTMLTextArea.fromEventTarget) >==>
                              (HTMLTextArea.value >=> pushPat)
                      , D.Value !:= dfPat
                      , D.Style !:= "height: 100px"
                      ]
                , D.pre (D.Class !:= "css" <|> D.Style !:= "text-wrap: wrap; min-height: 150px")
                    [ DC.text $ result <$> getParser <*> currentRaw <*> currentPat ]
                ]
  where
  dfTy = "\"A\" | [\"\" $ $$] | \"B\""
  dfPat = "[\"\" \"\" c]"
  parsePat = parse patternP
  result parser tyS patS = case parser tyS `lift2 Tuple` parsePat patS of
    Left r -> r
    Right (Tuple ty pat) -> do
      testPatternResult $ testPattern pat ty
