module Parser.Test where

import Prelude

import Ansi.Codes as Ansi
import Ansi.Output (withGraphics)
import Data.Argonaut (Json, encodeJson, stringify)
import Data.Argonaut as J
import Data.Either (Either(..), either)
import Data.Enum (toEnum)
import Data.Foldable (fold, for_, oneOf, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Number as Num
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import Idiolect ((<#?>), (<$?>), (>==))
import Parser.Comb as Comb
import Parser.Comb.Comber (Comber(..), arrayOf, delim, many, named, namedRec, objectOf, parse, parse', piggyback, printGrammarWsn, printStateTable, rawr, squish, toAnsi, token, tokenPrecL, tokenPrecR, topName, ws, wsOf, wsws, (#:), (<|>))
import Parser.Comb.Comber as Comber
import Parser.Debug (thingy)
import Parser.Languages as IL
import Parser.Printer.Languages as Languages
import Parser.Printer.Run as IO
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Whitespace (ParseWS(..), mkParseWS)

failed :: Ref Boolean
failed = unsafePerformEffect do Ref.new false

testNo :: Ref Int
testNo = unsafePerformEffect do Ref.new 0

newtype WRAPPER = WRAPPER (Comber ~> Comber)

main :: Effect Unit
main = do
  let space = mkParseWS { allowed_newline: false, allowed_space: true, required: false }
  let ws = wsOf space
  let spacee = mkParseWS { allowed_newline: false, allowed_space: true, required: true }
  let wss = wsOf spacee
  let nows = wsOf $ mkParseWS { allowed_newline: false, allowed_space: false, required: false }
  let
    sepace :: forall l r. Comber l -> Comber r -> Comber String
    sepace l r = squish (\_ h _ -> fold h) l (Just space) r
  let a_b = sepace (token "a") (token "b")
  for_ [WRAPPER identity, WRAPPER ("wrapped"#:_)] \(WRAPPER wrapper) -> do
    testParse "a b" (wrapper a_b)
      do Right { before: "", result: " ", after: "" }
    testParse "ab" (wrapper a_b)
      do Right { before: "", result: "", after: "" }
    testParse " a b" (wrapper a_b) (Left unit)
    testParse "a b " (wrapper a_b) (Left unit)
    testParse " a b " (wrapper a_b) (Left unit)
    testParse "   a  b " (wrapper (ws *> a_b <* ws))
      do Right { before: "   ", result: "  ", after: " " }
    testParse " " (wrapper ws)
      do Right { before: "", result: unit, after: " " }
    for_ [WRAPPER identity, WRAPPER ("wrapped1"#:_)] \(WRAPPER w1) -> do
      for_ [WRAPPER identity, WRAPPER ("wrapped2"#:_)] \(WRAPPER w2) -> do
        -- ("a" "b" )
        testParse "a b" (wrapper (sepace (w1 $ token "a" *> ws) (w2 $ token "b" *> ws)))
          do Right { before: "", result: " ", after: "" }
        testParse "ab" (wrapper (sepace (w1 $ token "a" *> ws) (w2 $ token "b" *> ws)))
          do Right { before: "", result: "", after: "" }
        testParse "a  b " (wrapper (sepace (w1 $ token "a" *> ws) (w2 $ token "b" *> ws)))
          do Right { before: "", result: "  ", after: " " }
        -- ("a"  "b" )
        testParse "a b" (wrapper (sepace (w1 $ token "a" *> wss) (w2 $ token "b" *> ws)))
          do Right { before: "", result: " ", after: "" }
        testParse "ab" (wrapper (sepace (w1 $ token "a" *> wss) (w2 $ token "b" *> ws)))
          do Left unit
        -- ("a"  "b"  )
        testParse "a b" (wrapper (sepace (w1 $ token "a" *> wss) (w2 $ token "b" *> wss)))
          do Left unit
        -- ("a""b"|"a"  "b")
        let detect = wrapper $ (w1 $ token "a") *> (w2 $ false <$ (nows *> token "b") <|> true <$ (wss *> token "b"))
        testParse "ab" detect do Right { before: "", result: false, after: "" }
        testParse "a b" detect do Right { before: "", result: true, after: "" }
  whenM (not <$> Ref.read failed) do
    log ""
    num <- Ref.read testNo
    log $ show num <> " succeeded!"
  let json = IO.runPrinterParser Languages.json
  for_ [encodeJson "test", encodeJson { "test": ["a","b","c"] }] \jt -> do
    let js = (json.print jt).text Nothing mempty
    log js
    case json.fullParse js of
      Left { string: err } -> log $ force err
      Right j | force j.ast /= jt -> do
        log "Nope:"
        log $ stringify $ force j.ast
      Right j -> do
        log "Yes:"
        log $ j.cst.text Nothing mempty
        traceM $ j.tree
        pure unit

testParse :: forall r. Eq r => String -> Comber r -> Either Unit { before :: String, result :: r, after :: String } -> Effect Unit
testParse input parser expected = do
  num <- Ref.modify (_ + 1) testNo
  case Comb.parseRegex topName (un Comber parser) input of
    Left _err | Left _ <- expected -> pure unit
    Left err -> do
      Ref.write true failed
      log $ show num <> " unexpected failure:"
      log $ Comber.convertParseError err
    Right r | Right exp <- expected, exp == r -> pure unit
    Right r -> do
      Ref.write true failed
      log $ show num <> " unexpected success:"
      log $ unsafeCoerce r

{-
space = wsOf $ WS.mkParseWS tt
nospace = wsOf $ WS.mkParseWS ff
spaceOpt = wsOf one

map (D.code[] <<< D.show) $ sourceOf $ (token "x") *> space *> do sourceOf do "R"#: token "y"
-}
{-
map (D.code[] <<< D.show) $ sourceOf $ (token "x") *> do
  sourceOf do space *> token "z" <* nospace <|> nospace *> token "z" <* space
-}
{-
map (D.code[] <<< D.show) $ sourceOf $ (\x -> spaceOpt *> x <* spaceOpt) $
  "expr" #-> \rec -> rawr "\\d+" <|>
    rec <> (spaceOpt *> (tokenPrecL "+" zero <|> tokenPrecR "*" one) <* spaceOpt) <> rec
-}
{-
import Parser.Languages as L
widget = livePrinterParser

newtype J = J (Json.Json)
instance Show J where
  show (J j) = Json.stringify j
dimap (\(J j) -> j) J $ IOL.json
-- IOL.jsonString

-- map (D.text <<< show) $ I.many "test" $ I.token "x"
-}


{-
T.text <$> "P2"#-> \p2 -> token "1" <|> (token "0" <> token "0" <> p2 <> p2) <|> (token "0" <> token "1" <> token "0" <> p2)

0000
-}
