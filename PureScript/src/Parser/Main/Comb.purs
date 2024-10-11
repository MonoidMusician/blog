module Parser.Main.Comb where

import Prelude

import Ansi.Codes as Ansi
import Control.Monad.Except (runExcept)
import Control.Plus ((<|>))
import Data.Array (intercalate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Ref as Ref
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Idiolect (intercalateMap, (<#?>), (>==))
import JSURI (encodeURIComponent)
import Parser.Comb.Comber (Comber, Printer, parse, printGrammarWsn, toAnsi)
import Parser.Template (template)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import Riverdragon.Dragon (Dragon, renderEl)
import Riverdragon.Dragon.Bones ((.$), (.$$~), (:.), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy, sourceCode)
import Riverdragon.River (Lake, createRiverStore, makeLake, subscribe)
import Riverdragon.River.Beyond (dedup)
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Widget (Widget)

color :: Ansi.Color -> String
color = case _ of
  Ansi.Blue -> "cornflowerblue"
  x -> String.toLower (show x)

colorful :: Ansi.Color -> String -> Dragon
colorful x t = D.span
  [ D.style =:= "color:" <> color x ]
  (D.text t)

toDragon :: Printer Dragon
toDragon =
  { nonTerminal: colorful Ansi.Blue
  , literal: colorful Ansi.Yellow <<< show
  , regex: colorful Ansi.Green
  , rule: colorful Ansi.Cyan <<< show
  , meta: D.text
  , lines: foldMap (D.div[])
  }

mainForParser :: Comber (Dodo.Doc Void) -> Effect Unit
mainForParser parser = do
  log $ printGrammarWsn toAnsi parser
  window >>= document >== HTMLDocument.toNonElementParentNode >>= getElementById (ElementId "grammar") >>= traverse_ \grammarEl -> do
    renderEl grammarEl (printGrammarWsn toDragon parser)
  messageInBottle
    { parser: parse parser >>> case _ of
        Left err -> { success: false, result: err }
        Right doc -> { success: true, result: Dodo.print Dodo.plainText Dodo.twoSpaces doc }
    , grammar: \_ -> []
    }

type IntoGrammar m =
  { terminal :: String -> m
  , nonTerminal :: String -> m
  , regex :: String -> m
  , meta :: String -> m
  }

type SideChannel =
  { parser :: String -> { success :: Boolean, result :: String }
  , grammar :: forall m. Monoid m =>
    IntoGrammar m ->
    Array { name :: String, syntax :: m }
  }

foreign import messageInBottle :: SideChannel -> Effect Unit
foreign import installSideChannel :: (SideChannel -> Effect Unit) -> Effect Unit

sideChannel :: Lake SideChannel
sideChannel = makeLake \cb -> mempty <$ installSideChannel cb

fake :: CST.T.SourceRange
fake =
  join { start: _, end: _ } $
    join { line: _, column: _ } 0

fakeTok :: CST.T.Token -> CST.T.SourceToken
fakeTok value =
  { range: fake
  , leadingComments: []
  , trailingComments: []
  , value
  }

applyTemplate :: CST.T.Expr Void -> CST.T.Module Void
applyTemplate parserExpr = template #
  ( _Newtype <<< prop (Proxy :: Proxy "body")
  <<< _Newtype <<< prop (Proxy :: Proxy "decls")
  <<< traversed
  ) case _ of
    CST.T.DeclValue decl@{ name: CST.T.Name { name: CST.T.Ident "parser" } } ->
      CST.T.DeclValue decl { guarded = guarded }
      where
      guarded =
        CST.T.Unconditional (fakeTok CST.T.TokEquals) $
          CST.T.Where
            { bindings: Nothing
            , expr: parserExpr
            }
    decl -> decl

rename :: String -> CST.T.Module Void -> CST.T.Module Void
rename = identity
  <<< _Newtype <<< prop (Proxy :: Proxy "header")
  <<< _Newtype <<< prop (Proxy :: Proxy "name")
  <<< _Newtype <<< \newName _ ->
    { name: CST.T.ModuleName newName
    -- slight hack: should be TokUpperName (Just prefix) suffix
    , token: fakeTok $ CST.T.TokUpperName Nothing newName
    }

assemble :: String -> Either (NonEmptyArray CST.M.PositionedError) String
assemble = CST.parseExpr >>> case _ of
  CST.ParseSucceeded parserExpr -> Right do
    TC.printModule $
      rename "Parser.Temp" $
        applyTemplate parserExpr
  CST.ParseSucceededWithErrors _ es -> Left es
  CST.ParseFailed e -> Left (pure e)

compile :: String -> Aff (Either (Array String) String)
compile moduleString = do
  response <- fetch "http://127.0.0.1:6565/compile"
    { method: POST
    , body: moduleString
    } >>= _.json
  let
    getCode = readProp "js" >=> readString
    getErrors = pure
      >=> readProp "error"
      >=> readProp "contents"
      >=> readArray
      >=> traverse
        (readProp "message" >=> readString)
  case response of
    _ | Right code <- runExcept (getCode response) ->
        pure $ Right code
    _ | Right errors <- runExcept (getErrors response) ->
        pure $ Left errors
    _ -> throwError $ error "Bad response from TryPureScript"

bundle :: String -> String
bundle js =
  -- Adapted from TryPureScript client
  let
    importExportRegex :: Regex.Regex
    importExportRegex = either (\_ -> unsafeCrashWith "Invalid regex") identity
      $ Regex.regex """ from "../([^"]+)";$""" RegexFlags.noFlags
    replacement = " from \"http://localhost:7933/assets/ps/$1\";"
    codeWithRemappedImports = js
      # String.split (String.Pattern "\n")
      # map (Regex.replace importExportRegex replacement)
      # String.joinWith "\n"
  -- Actually call the `main` function
  in codeWithRemappedImports <> "\n\nmain();\n"

printErrors :: NonEmptyArray CST.M.PositionedError -> String
printErrors = intercalateMap "\n" printPositionedError

printPositionedError :: CST.M.PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error

throwEither :: forall e a. (e -> String) -> Either e a -> Aff a
throwEither f = either (throwError <<< error <<< f) pure

data Status
  = Compiling
  | Failed String
  | Compiled String

pipeline ::
  Lake String -> Lake Status
pipeline incoming = makeLake \sub -> do
  cancel <- Ref.new mempty
  unsub <- subscribe incoming \parserCombinator -> do
    log "Hi"
    launchAff_ =<< Ref.read cancel
    Ref.write mempty cancel
    log parserCombinator
    case assemble parserCombinator of
      Left errs -> sub $ Failed $ printErrors errs
      Right assembled -> do
        log "Compiling"
        sub Compiling
        fiber <- compile assembled # runAff case _ of
          Left kaboom -> sub $ Failed $ message kaboom
          Right (Left err) -> sub $ Failed $ intercalate "\n" err
          Right (Right compiled) -> do
            log ":3"
            Ref.write mempty cancel
            sub $ Compiled $ bundle compiled
        Ref.write (Aff.killFiber (error "Canceled due to new input") fiber) cancel
  pure do
    unsub
    launchAff_ =<< Ref.read cancel

embed :: Lake String -> Dragon
embed incomingRaw = eggy \shell -> do
  incoming <- shell.instStore do dedup incomingRaw
  pipelined <- shell.instStore do pipeline incoming
  gotParser <- shell.instStore sideChannel
  let
    bundled = pipelined <#?> case _ of
      Compiled result -> Just result
      _ -> Nothing
    status = mempty <$ gotParser <|> do
      pipelined <#> case _ of
        Compiling -> D.text "Compiling ..."
        Failed err -> D.pre [ D.style =:= "overflow-x: auto" ] (D.text err)
        Compiled _ -> D.text "Loading ..."
  pure $ D.Fragment
    [ D.div [ D.style =:= "white-space: pre" ] $ D.Replacing status
    , bundled >@ \latestBundle -> fold
      [ D.div [ D.id =:= "grammar" ] mempty
      , gotParser >@ \parser -> D.Egg do
          { send: setValue, stream: getValue } <- createRiverStore Nothing
          pure $ { destroy: mempty, dragon: _ } $ D.Fragment
            [ sourceCode "In" :."sourceCode unicode".$
                D.textarea
                  [ D.onInputValue =:= setValue
                  , D.value =:= ""
                  , D.style =:= "height: 15vh"
                  ]
            , sourceCode "Out" :."sourceCode unicode".$$~
                _.result <<< parser.parser <$> getValue
            ]
      , D.script
          [ D.attr "src" =:= do
              fromMaybe "" $ encodeURIComponent latestBundle <#> \escaped ->
                "data:text/javascript;utf8," <> escaped
          , D.prop "type" =:= "module"
          ]
      ]
    ]

widget :: Widget
widget _ = pure $ eggy \shell -> do
  let df = "D.text <$> string"
  { stream: valueSet, send: setValue } <- shell.track $ createRiverStore Nothing
  { stream: compiling, send: compileNow } <- shell.track $ createRiverStore Nothing
  lastValue <- shell.storeLast df valueSet
  pure $ D.Fragment
    [ sourceCode "PureScript" .$ D.textarea
        [ D.onInputValue =:= setValue
        , D.value =:= df
        , D.style =:= "height: 40vh"
        ]
    , D.div.$ D.buttonW "" "Compile!" (compileNow =<< lastValue)
    , embed compiling
    ]

