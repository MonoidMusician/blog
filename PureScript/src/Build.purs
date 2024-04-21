module Build where

import Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Identity (Identity(..))
import Data.Monoid (power)
import Data.Newtype (un, unwrap)
import Data.String as String
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Parser.Comb (Comb(..))
import Parser.Comb.Comber (Comber(..), freezeTable, parse', printConflicts, printGrammarWsn, toAnsi)
import Parser.Comb.Run (combPrecedence, gatherPrecedences)
import Parser.Debug (thingy)
import Parser.Languages as Languages
import Parser.Languages.CSS as CSS
import Parser.Languages.Dhall as Dhall
import Parser.Languages.Show as Show
import Parser.Languages.TMTTMT.Parser as TMTTMT
import Parser.Lexing (applyPrecedence)
import Parser.Types (States(..))

main :: Effect Unit
main = launchAff_ do
  let
    process :: forall a. Comber a -> String -> Aff Unit
    process parser filename = do
      log filename
      -- log $ power "=" (String.length filename)
      -- log $ printGrammarWsn toAnsi parser
      let Comber (Comb info) = parser
      log $ show $ Array.nub $ info.entrypoints
      t0 <- liftEffect now
      let dat@{ states: States states } = fst $ parse' parser
      t1 <- liftEffect now
      log $ show (Array.length states) <> " states"
      log $ show (unwrap (unInstant t1) - unwrap (unInstant t0)) <> " milliseconds"
      log $ printConflicts toAnsi $ un Identity $ applyPrecedence (combPrecedence $ gatherPrecedences (Comb info)) $ States states
      void $ try do FS.rm' ("./assets/json/" <> filename <> ".json.gz") { force: false, maxRetries: 0, recursive: false, retryDelay: 0 }
      FS.writeTextFile UTF8 ("./assets/json/" <> filename <> ".json") $ J.stringify $ freezeTable dat

  log $ show $ thingy

  process Show.lazyTop "show-parser-states"
  process CSS.selector_list "css-parser-states"
  process TMTTMT.declarationsP "tmttmt-parser-states"
  process TMTTMT.typeP "tmttmt-types-parser-states"
  process Languages.json "json-parser-states"
  process Languages.arithmetic "arithmetic-parser-states"
  process Dhall.complete_dhall_file "dhall-parser-states"
