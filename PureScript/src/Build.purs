module Build where

import Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Parser.Comb (Comb(..))
import Parser.Comb.Comber (Comber(..), freezeTable, parse', printConflicts, printGrammarWsn, toAnsi)
import Parser.Languages.CSS as CSS
import Parser.Languages.Show as Show
import Parser.Languages.TMTTMT.Parser as TMTTMT
import Parser.Types (States(..))

main :: Effect Unit
main = launchAff_ do
  let
    process :: forall a. Comber a -> String -> Aff Unit
    process parser filename = do
      log $ printGrammarWsn toAnsi parser
      let Comber (Comb info) = parser
      log $ show $ info.entrypoints
      let dat@{ states: States states } = fst $ parse' parser
      log $ show (Array.length states) <> " states"
      log $ printConflicts toAnsi dat
      void $ try do FS.rm' ("./assets/json/" <> filename <> ".json.gz") { force: false, maxRetries: 0, recursive: false, retryDelay: 0 }
      FS.writeTextFile UTF8 ("./assets/json/" <> filename <> ".json") $ J.stringify $ freezeTable dat

  process Show.lazyTop "show-parser-states"
  process CSS.selector_list "css-parser-states"
  process TMTTMT.declarationsP "tmttmt-parser-states"
