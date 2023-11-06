module Build where

import Prelude

import Data.Argonaut as J
import Data.Array ((!!))
import Data.Array as Array
import Data.Bitraversable (bitraverse)
import Data.Codec as C
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Monoid as M
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Traversable (fold, foldMap, for, for_, traverse_)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Parser.Comb (Comb(..), named, parseRegex')
import Parser.Examples (showPart)
import Parser.Languages (Comber, mainName, printPretty, showZipper, (/|\))
import Parser.Languages.CSS as CSS
import Parser.Languages.TMTTMT.Parser as TMTTMT
import Parser.Types (Fragment, OrEOF(..), Part(..), ShiftReduce(..), States(..), Zipper(..), decisionUnique)

main :: Effect Unit
main = launchAff_ do
  let
    normal :: Fragment (Either _ _) _ -> Maybe (Fragment _ _)
    normal frag = for frag case _ of
      Terminal (Continue a) -> Just (Terminal a)
      NonTerminal (Right a) -> Just (NonTerminal a)
      _ -> Nothing
    showItems pre items =
      for_ (unwrap items) \{ pName, rName, rule: Zipper l' r' } -> do
        for_ (normal l' /|\ normal r') \(l /\ r) ->
          log $ pre <> showPart (NonTerminal (fold pName)) <> "." <> foldMap show rName <> " = " <> showZipper (Zipper l r)

    process :: forall a. Comber a -> String -> Aff Unit
    process parser filename = do
      liftEffect $ printPretty $ named mainName parser
      let Comb info = parser
      log $ show $ info.entrypoints
      let dat@(_ /\ _ /\ States states) = fst $ parseRegex' mainName parser
      log $ show (Array.length states) <> " states"
      log "Conflicts:"
      conflicts <- states # foldMap \{ items, advance } ->
        advance # foldMap \sr -> M.guard (not decisionUnique sr) do
          log $ "- " <> show sr
          case sr of
            ShiftReduces s rs | Just st <- states !! s -> do
              log "  Shift to"
              showItems "  -> " st.items
              log "  Or reduce"
              for_ rs $ bitraverse hush identity >>> traverse_ \(name /\ num) -> do
                log $ "  <- " <> showPart (NonTerminal name) <> "." <> show num
            _ -> pure unit
          log "  With items"
          showItems "  - " items
          pure (Additive 1)
      log $ show $ unwrap conflicts
      void $ try do FS.rm' ("./assets/json/" <> filename <> ".json.gz") { force: false, maxRetries: 0, recursive: false, retryDelay: 0 }
      FS.writeTextFile UTF8 ("./assets/json/" <> filename <> ".json") $ J.stringify $ C.encode CSS.codec $
        dat

  process CSS.selector_list "css-parser-states"
  process TMTTMT.declarationsP "tmttmt-parser-states"
