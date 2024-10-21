module Build where

import Prelude

import Data.Argonaut (decodeJson, parseJson)
import Data.Argonaut as J
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), hush, isLeft)
import Data.Foldable (for_, maximum)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Aff as Aff
import Effect.Aff as Ex
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Now (now)
import Idiolect ((>==))
import Node.Encoding (Encoding(..))
import Node.FS (SymlinkType(..))
import Node.FS.Aff as FS
import Node.FS.Stats (isDirectory)
import Parser.Comb (Comb(..))
import Parser.Comb.Comber (Comber(..), compact, freezeTable, parse', printConflicts, toAnsi)
import Parser.Comb.Run (combPrecedence, gatherPrecedences)
import Parser.Debug (thingy)
import Parser.Languages as Languages
import Parser.Languages.CSS as CSS
import Parser.Languages.HFS as HFS
import Parser.Languages.Show as Show
import Parser.Languages.TMTTMT.Parser as TMTTMT
import Parser.Lexing (applyPrecedence)
import Parser.Types (States(..))
import PureScript.Highlight (highlightPandoc)
import Safe.Coerce (coerce)

main :: Effect Unit
main = launchAff_ do
  mainJson
  mainPurs

mainJson :: Aff Unit
mainJson = do
  let
    process :: forall a. Comber a -> String -> Aff Unit
    process parser filename = do
      log filename
      -- log $ power "=" (String.length filename)
      -- log $ printGrammarWsn toAnsi parser
      let Comber (Comb info) = parser
      let rules = Array.nub $ unwrap info.grammar
      log $ append "Grammar breadth: " $ show $ Array.length rules
      log $ append "Grammar depth: " $ show $ fromMaybe 0 $ maximum $ rules <#> _.rule >>> Array.length
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
  process HFS.parser "hatstack-parser-states"
  -- process Dhall.complete_dhall_file "dhall-parser-states"

rm :: String -> Aff Unit
rm file = FS.rm' file { force: false, maxRetries: 0, recursive: false, retryDelay: 0 }
rmr :: String -> Aff Unit
rmr file = FS.rm' file { force: false, maxRetries: 0, recursive: true, retryDelay: 0 }

check :: Boolean -> Aff Unit
check false = Aff.throwError (Aff.error "check")
check _ = pure unit

found :: Maybe ~> Aff
found Nothing = Aff.throwError (Aff.error "found")
found (Just r) = pure r

hm :: forall e. Either e ~> Aff
hm = found <<< hush

tryIt :: forall r. Aff r -> Aff (Maybe r)
tryIt = Aff.try >== hush

tryy :: forall a. Aff a -> Aff Unit
tryy = void <<< Aff.try

pathSep :: String -> String -> String
pathSep x y = x <> "/" <> y
infixr 2 pathSep as /./

templating :: String -> Aff String
templating contents = do
  template <- FS.readTextFile UTF8 "./assets/template.html"
  pure $ String.replace
    (String.Pattern """<div id="TEMPLATE_CONTENT"/>""")
    (String.Replacement (coerce String.replaceAll "$" "$$$$" contents))
    template

mainPurs :: Aff Unit
mainPurs = do
  let
    symlink x y = do
      tryy $ FS.unlink y
      tryy $ FS.symlink x y (FileLink {- Windows nonsense -})
  modules <- validModules =<< FS.readdir "./output"
  -- traverse_ Console.logShow modules
  tryy do
    FS.mkdir ("./assets/purs")
  for_ modules \m -> do
    tryy do FS.mkdir m.gathered
    let
      copyTo file source = tryy do
        sourcePath <- FS.realpath source
        symlink sourcePath (m.gathered/./file)
    let copyFrom source file = copyTo file (source/./file)
    let writeTo file contents = FS.writeTextFile UTF8 (m.gathered/./file) contents
    copyTo "source.purs" m.modulePath
    copyTo "index.js" $ m.compiled/./"index.js"
    copyTo "index-es.js" $ m.optimized/./"index.js"
    copyFrom m.compiled "foreign.js"
    copyTo "foreign-es.js" $ m.compiled/./"foreign.js"
    copyFrom m.compiled "corefn.json"
    copyFrom m.compiled "docs.json"
    copyTo "docs.md" m.docs.md
    copyTo "docs.html" m.docs.html

    source <- FS.readTextFile UTF8 m.modulePath
    r <- try do
      pure unit
      writeTo "source.purs.html" =<< templating (highlightPandoc source)
    case r of
      Right _ -> pure unit
      Left e -> do
        Console.log $ m.modulePath <> " failed"
        Console.log $ Ex.message e
  pure unit

type Module =
  { compiled :: String
  , docs ::
    { html :: String
    , md :: String
    }
  , gathered :: String
  , moduleName :: String
  , modulePath :: String
  , optimized :: String
  }

validModules :: Array String -> Aff (Array Module)
validModules = map compact <<< traverse \moduleName -> tryIt do
  let compiled = "./output/" <> moduleName
  let optimized = "./output-es/" <> moduleName
  let gathered = "./assets/purs/" <> moduleName
  let docs_ ty = "./generated-docs/" <> ty /./ moduleName <> "." <> ty
  let docs = { md: docs_"md", html: docs_"html" }
  stats <- FS.stat compiled
  check $ isDirectory stats
  corefnText <- FS.readTextFile UTF8 (compiled <> "/corefn.json")
  corefn <- hm $ parseJson corefnText
  ({ modulePath } :: { modulePath :: String }) <- hm $ decodeJson corefn
  whenM (isLeft <$> Aff.try (FS.stat modulePath)) do
    Console.log $ modulePath <> " went away"
    rmr compiled
    rmr optimized
    rmr gathered
    rm docs.md
    rm docs.html
  pure { moduleName, modulePath, compiled, optimized, gathered, docs }
