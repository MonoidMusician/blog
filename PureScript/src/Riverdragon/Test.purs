module Riverdragon.Test where

import Prelude

import Data.Either (either)
import Data.Foldable (oneOfMap)
import Data.Time.Duration (Milliseconds(..))
import Deku.Attribute ((!:=))
import Deku.DOM as Deku
import Effect (Effect)
import Effect.Console as Console
import FRP.Event (Event)
import FRP.Event as FRP
import Idiolect ((/|\), (\|/))
import Riverdragon.Dragon (Dragon, renderEl, renderId)
import Riverdragon.Dragon as D
import Riverdragon.Dragon.Bones as B
import Riverdragon.Dragon.Wings (instantiateListenInput, listenInput, vanishing)
import Riverdragon.River (Lake, Stream, chill, makeStream, subscribe)
import Web.DOM.ElementId (ElementId(..))
import Widget (Widget)
import Widget.Types (SafeNut(..))

logging :: forall flow6 t8. Show t8 => Stream flow6 t8 -> Effect Unit
logging event = void $ subscribe event Console.logShow

rendering :: Dragon -> Effect (Effect Unit)
rendering = renderId (ElementId "render-target")

main :: Effect Unit
main = do
  single <- chill <$> instantiateListenInput true (ElementId "test-input")
  let doubled = single /|\ single
  logging doubled
  let tupled = listenInput false (ElementId "test-input1") /|\ listenInput false (ElementId "test-input2")
  logging tupled
  logging (doubled \|/ tupled)
  let shown = show <$> (doubled \|/ tupled)
  _ <- rendering $ D.Text shown
  _ <- rendering $ B.div $ D.Text shown
  _ <- rendering $ D.Appending $ vanishing (1000.0 # Milliseconds) $ B.li <<< B.text <$> shown
  _ <- rendering $ D.Replacing $ either (B.i <<< B.text <<< show) (B.b <<< B.text <<< show) <$> (doubled \|/ tupled)
  _ <- rendering $ D.Appending $ B.div<<<B.text <$> oneOfMap pure ["l", "r"]
  pure unit

widget :: Widget
widget _ = main $> mempty

host :: Effect Dragon -> Effect SafeNut
host = map \dragon -> SafeNut do
  flip Deku.span [] $ Deku.Self !:= \el ->
    void do renderEl el dragon

event2Lake :: Event ~> Lake
event2Lake ev = makeStream \cb -> FRP.subscribe ev cb
