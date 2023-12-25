module Parser.Main.Comb where

import Prelude

import Ansi.Codes as Ansi
import Control.Monad.Except (runExcept)
import Control.Plus (empty, (<|>))
import Data.Argonaut as JSON
import Data.Array (intercalate, nub, (!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BooleanAlgebra.CSS (Vert, combineFold, printVerts, subsumptite)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), blush, either, hush)
import Data.Filterable (filter, filterMap, partitionMap, separate)
import Data.Foldable (fold, foldMap, oneOf, oneOfMap, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HeytingAlgebra (tt)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup.Foldable (intercalateMap)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.These (that)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Deku.Attribute (cb, prop', unsafeAttribute, xdata, (!:=), (<:=>))
import Deku.Control (switcher, text, text_)
import Deku.Control as DC
import Deku.Core (Domable, Nut, bussed, envy)
import Deku.DOM as D
import Deku.Toplevel (runInElement')
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Aff (affToEvent)
import FRP.Event (Event, fix, makeEvent, memoize, subscribe)
import FRP.Helpers (dedup)
import FRP.Memoize (memoBehFold, memoLast)
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object as Object
import Idiolect ((==<), (>==))
import JSURI (encodeURIComponent)
import Parser.Comb (Comb(..), Syntax, coalesce, printSyntax, printSyntax')
import Parser.Comb.Syntax (printSyntax'')
import Parser.Examples (showPart)
import Parser.Languages (Comber, printPretty, showFragment)
import Parser.Languages.CSS (mkCSSParser)
import Parser.Lexing (type (~), Rawr(..), Similar(..))
import Parser.Main (inputValidated)
import Parser.Template (template)
import Parser.Types (Part(..))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import Tidy (toDoc)
import Tidy as Tidy
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.AttrName (AttrName(..))
import Web.DOM.Document (createElement)
import Web.DOM.Document as Document
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.ElementName (ElementName(..))
import Web.DOM.Node (appendChild, removeChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event as Event
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Web.HTML.Window (document)
import Widget (Widget, adaptInterface)
import Widget.Types (SafeNut(..))

printUgly :: forall a. Comber a -> String
printUgly (Comb { prettyGrammar }) = fst $ nub prettyGrammar #
  traverse_ \(name /\ msyntax) ->
    msyntax # traverse_ \syntax ->
      flip Tuple unit $ showPart (NonTerminal name) <> " = " <> printSyntax showFragment syntax <> " .\n"

color :: Ansi.Color -> String
color = case _ of
  Ansi.Blue -> "cornflowerblue"
  x -> String.toLower (show x)

colorful :: Ansi.Color -> String -> Nut
colorful x t = D.span
  (D.Style !:= do "color:" <> color x)
  [ text_ t ]

renderPart :: Part String (String ~ Rawr) -> Nut
renderPart (Terminal (Similar (Left tok))) = colorful Ansi.Yellow $ show tok
renderPart (Terminal (Similar (Right re))) = colorful Ansi.Green $ show re
renderPart (NonTerminal nt) = colorful Ansi.Blue nt

renderParts :: Array (Part String (String ~ Rawr)) -> Nut
renderParts parts = intercalate (text_ " ") $ renderPart <$> parts

renderSyntax :: Syntax String (String ~ Rawr) -> Nut
renderSyntax syntax =
  intercalate (text_ " ") $
    map (either identity renderParts) $
      coalesce $ printSyntax'' text_ renderParts syntax

renderGrammar :: forall a. Comber a -> Nut
renderGrammar (Comb { prettyGrammar }) =
  D.ul_ $ nub prettyGrammar <#> \(name /\ msyntax) ->
    msyntax # foldMap \syntax ->
      D.li_ $ pure $
        renderPart (NonTerminal name) <> text_ " = " <> renderSyntax syntax <> text_ " .\n"

mainForParser :: Comber (Dodo.Doc Void) -> Effect Unit
mainForParser parser = do
  printPretty parser
  window >>= document >== HTMLDocument.toNonElementParentNode >>= getElementById (ElementId "grammar") >>= traverse_ \grammarEl -> do
    runInElement' grammarEl (renderGrammar parser)

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
    importRegex :: Regex.Regex
    importRegex = either (\_ -> unsafeCrashWith "Invalid regex") identity
      $ Regex.regex """^import (.+) from "../([^"]+)";$""" RegexFlags.noFlags
    replacement = "import $1 from \"http://localhost:7933/assets/ps/$2\";"
    codeWithRemappedImports = js
      # String.split (String.Pattern "\n")
      # map (Regex.replace importRegex replacement)
      # String.joinWith "\n"
  -- Actually call the `main` function
  in codeWithRemappedImports <> "\n\nmain();\n"

run :: String -> Effect (Effect Unit)
run js = do
  doc <- document =<< window
  script <- createElement (ElementName "script") (toDocument doc)
  setAttribute (AttrName "type") "module" script
  encodeURIComponent js # traverse_ \escaped -> do
    let
      src = "data:text/javascript;utf8," <> escaped
    setAttribute (AttrName "src") src script
  map (fromMaybe mempty) $ body doc >>= traverse \htmlElement -> do
    let bdy = HTMLElement.toNode htmlElement
    appendChild (Element.toNode script) bdy
    pure $ removeChild (Element.toNode script) bdy

printErrors :: NonEmptyArray CST.M.PositionedError -> String
printErrors = intercalateMap "\n" printPositionedError

printPositionedError :: CST.M.PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error

throwEither :: forall e a. (e -> String) -> Either e a -> Aff a
throwEither f = either (throwError <<< error <<< f) pure

-- widget :: Widget
-- widget _ = mempty <$ launchAff_ do
--   let parserCombinator = "string <#> D.text"
--   liftEffect $ log parserCombinator
--   assembled <- assemble parserCombinator # throwEither printErrors
--   compiled <- compile assembled >>= throwEither (intercalate "\n")
--   liftEffect $ void $ run $ bundle compiled

pipeline ::
  Event String -> Event (Either String String)
pipeline incoming = makeEvent \sub -> do
  cancel <- Ref.new mempty
  unsub <- subscribe incoming \parserCombinator -> do
    log "Hi"
    launchAff_ =<< Ref.read cancel
    Ref.write mempty cancel
    log parserCombinator
    case assemble parserCombinator of
      Left errs -> sub $ Left $ printErrors errs
      Right assembled -> do
        log "Compiling"
        sub $ Left "Compiling ..."
        fiber <- compile assembled # runAff case _ of
          Left kaboom -> sub $ Left $ message kaboom
          Right (Left err) -> sub $ Left $ intercalate "\n" err
          Right (Right compiled) -> do
            log ":3"
            Ref.write mempty cancel
            sub $ Right $ bundle compiled
        Ref.write (Aff.killFiber (error "Canceled due to new input") fiber) cancel
  pure do
    unsub
    launchAff_ =<< Ref.read cancel

embed :: Event String -> Nut
embed incomingRaw =
  envy $ memoLast (dedup incomingRaw) \incoming ->
  envy $ memoLast (pipeline incoming) \pipelined ->
  let
    { left: errors, right: bundled } = separate pipelined
    status = errors <|> "" <$ bundled
  in fold
    [ D.div (D.Style !:= "white-space: pre") [ text status ]
    , bundled # switcher \latestBundle -> fold
      [ D.div (D.Id !:= "grammar") []
      , flip D.script [] $ oneOf
          [ D.Src !:= do
              fromMaybe "" $ encodeURIComponent latestBundle <#> \escaped ->
                "data:text/javascript;utf8," <> escaped
          , pure $ unsafeAttribute { key: "type", value: prop' "module" }
          ]
      ]
    ]

widget :: Widget
widget _ = do
  pure $ SafeNut do
    let df = "D.text <$> string"
    bussed \setValue valueSet ->
      bussed \compileNow compiling -> fold
        [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "PureScript")) $
            pure $ D.pre_ $ pure $ D.code_ $ pure $
              flip D.textarea [] $ oneOf
                [ D.OnInput !:= updateTA setValue
                , D.Value !:= df
                , D.Style !:= "height: 40vh"
                ]
        , D.div_ $ pure $ D.button
            (D.OnClick <:=> (compileNow <$> (pure df <|> valueSet)))
            [ text_ "Compile!" ]
        , embed compiling
        ]
  where
  updateTA upd = cb $
    (Event.target >=> HTMLTextArea.fromEventTarget) >>>
      traverse_ \ta -> do
        value <- HTMLTextArea.value ta
        upd value


