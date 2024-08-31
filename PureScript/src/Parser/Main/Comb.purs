module Parser.Main.Comb where

import Prelude

import Ansi.Codes as Ansi
import Control.Monad.Except (runExcept)
import Control.Plus ((<|>))
import Data.Array (intercalate)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, oneOf, traverse_)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (Cb, cb, prop', unsafeAttribute, xdata, (!:=), (<:=>))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, Nut, bussed, envy)
import Deku.DOM as D
import Deku.Toplevel (runInElement')
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Effect.Console (log)
import Effect.Ref as Ref
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Helpers (dedup)
import FRP.Memoize (memoLast)
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Idiolect ((<#?>), (>==), intercalateMap)
import JSURI (encodeURIComponent)
import Parser.Comb.Comber (Comber, Printer, FullParseError, parse, printGrammarWsn, toAnsi)
import Parser.Lexing (FailReason(..), FailedStack(..), Similar(..), errorName, len, userErrors)
import Parser.Template (template)
import Parser.Types (ShiftReduce(..), unShift)
import Parser.Types as P
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST as CST
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Web.DOM.ElementId (ElementId(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event as Event
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLTextAreaElement as HTMLTextArea
import Web.HTML.Window (document)
import Widget (Widget)
import Widget.Types (SafeNut(..))

color :: Ansi.Color -> String
color = case _ of
  Ansi.Blue -> "cornflowerblue"
  x -> String.toLower (show x)

colorful :: Ansi.Color -> String -> Nut
colorful x t = D.span
  (D.Style !:= do "color:" <> color x)
  [ text_ t ]

toDeku :: forall lock payload. Printer (Domable lock payload)
toDeku =
  { nonTerminal: colorful Ansi.Blue
  , literal: colorful Ansi.Yellow <<< show
  , regex: colorful Ansi.Green
  , rule: colorful Ansi.Cyan <<< show
  , meta: text_
  , lines: foldMap (D.div_ <<< pure)
  }

mainForParser :: Comber (Dodo.Doc Void) -> Effect Unit
mainForParser parser = do
  log $ printGrammarWsn toAnsi parser
  window >>= document >== HTMLDocument.toNonElementParentNode >>= getElementById (ElementId "grammar") >>= traverse_ \grammarEl -> do
    runInElement' grammarEl (printGrammarWsn toDeku parser)
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

sideChannel :: Event SideChannel
sideChannel = makeEvent \cb -> mempty <$ installSideChannel cb

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
  Event String -> Event Status
pipeline incoming = makeEvent \sub -> do
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

embed :: Event String -> Nut
embed incomingRaw =
  envy $ memoLast (dedup incomingRaw) \incoming ->
  envy $ memoLast (pipeline incoming) \pipelined ->
  envy $ memoLast sideChannel \gotParser ->
  let
    bundled = pipelined <#?> case _ of
      Compiled result -> Just result
      _ -> Nothing
    status = mempty <$ gotParser <|> do
      pipelined <#> case _ of
        Compiling -> text_ "Compiling ..."
        Failed err -> D.pre (D.Style !:= "overflow-x: auto") [ text_ err ]
        Compiled _ -> text_ "Loading ..."
  in fold
    [ D.div (D.Style !:= "white-space: pre") [ switcher identity status ]
    , bundled # switcher \latestBundle -> fold
      [ D.div (D.Id !:= "grammar") []
      , gotParser # switcher \parser ->
          bussed \setValue getValue -> fold
          [ D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "In")) $
              pure $ D.pre_ $ pure $ D.code_ $ pure $
                flip D.textarea [] $ oneOf
                  [ D.OnInput !:= updateTA setValue
                  , D.Value !:= ""
                  , D.Style !:= "height: 15vh"
                  ]
          , D.div (D.Class !:= "sourceCode unicode" <|> pure (xdata "lang" "Out")) $
              pure $ D.pre_ $ pure $ D.code_ $ pure $ text $ _.result <<< parser.parser <$> getValue
          ]
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

updateTA :: (String -> Effect Unit) -> Cb
updateTA upd = cb $
  (Event.target >=> HTMLTextArea.fromEventTarget) >>>
    traverse_ \ta -> do
      value <- HTMLTextArea.value ta
      upd value


renderParseError :: forall lock payload. FullParseError -> Domable lock payload
renderParseError theError = case theError of
  CrashedStack s -> D.div_ [ text_ "Internal parser error: ", text_ s ]
  FailedStack info@{ lookupState, initialInput, currentInput, failedStack } ->
    D.div_
      [ case info.failReason of
          StackInvariantFailed -> text_ $ "Internal parser error: Stack invariant failed"
          UnknownState { state } -> text_ $ "Internal parser error: Unknown state " <> show state
          Uhhhh { token: cat } -> text_ $ "Internal parser error: Uhhhh " <> show cat
          UnknownReduction { reduction: nt /\ r } -> text_ $ "Internal parser error: UnknownReduction " <> show nt <> "#" <> show r
          MetaFailed {} -> text_ $ "Interal parser error: Meta failed"

          UserRejection { userErr } -> fold
            [ text_ "Parse error: User rejection"
            , listing userErr text_
            ]
          NoTokenMatches { advance } -> fold
            [ text_ "Parse error: No token matches"
            , text_ ", expected"
            , text_ case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          UnexpectedToken { advance, matched }
            | [ _meta /\ _air /\ cat0 /\ o /\ _i ] <- NEA.toArray matched -> fold
            [ text_ "Parse error: Unexpected token "
            , showCatTok cat0 o
            , text_ ", expected"
            , text_ case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          UnexpectedToken { advance, matched: matched } -> fold
            [ text_ "Parse error: Unexpected+ambiguous token "
            , listing (NEA.toArray matched) \(_meta /\ _air /\ cat0 /\ o /\ _i) ->
                showCatTok cat0 o
            , text_ "\nExpected"
            , text_ case Map.size advance of
                0 -> " nothing??"
                1 -> ""
                _ -> " one of"
            , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
                showCat cat
            ]
          AllActionsRejected { possibilities } -> fold
            [ text_ "Parse error: All actions rejected"
            , listing (NEA.toArray possibilities) \(poss /\ errs) -> fold
                [ text_ (show poss)
                , listing errs \err -> fold
                    [ text_ $ errorName err
                    , listing (userErrors err) text_
                    ]
                ]
            ]
          Ambiguity { filtered, userErr } -> fold
            [ text_ "Parse error: LR(1) ambiguity"
            , listing (NEA.toArray filtered) \(Tuple sr _) -> fold
                [ case sr of
                    Shift s -> text_ $ "Shift to " <> show s
                    ShiftReduces s rs -> text_ ("Shift to " <> show s <> ", or reduce ") <>
                      intercalateMap (text_ " or ") (uncurry showNTR <<< snd) rs
                    Reduces rs -> text_ "Reduce " <>
                      intercalateMap (text_ " or ") (uncurry showNTR <<< snd) rs
                , listing (Array.fromFoldable (unShift sr >>= lookupState)) \state ->
                    unwrap state.items # foldMap \item -> fold
                      [ showNTR item.pName item.rName
                      , text_ " = "
                      , case item.rule of
                          P.Zipper parsed toParse -> fold
                            [ intercalateMap (text_ " ") part parsed
                            , text_ " • "
                            , intercalateMap (text_ " ") part toParse
                            ]
                      ]
                ]
            , listing userErr text_
            ]
      , text_ $ "\nat character " <> show (1 + len initialInput - len currentInput)
      , text_ case initialInput of
          P.Continue s -> "\n  " <> show (String.drop (len initialInput - len currentInput) s)
          P.EOF -> ""
      -- , "\n"
      -- , show info.failedState
      -- , "\n"
      -- , "Stack: " <> show (stackSize failedStack)
      ]
  where
  listing :: forall a. Array a -> (a -> Domable lock payload) -> Domable lock payload
  listing items f = D.ul_ $ items <#> f >>> pure >>> D.li_

  showNTR (Left x) Nothing = text_ (x <> "#")
  showNTR (Right x) (Just r) = text_ (x <> "#" <> show r)
  showNTR _ _ = text_ "???"

  showCat = part <<< P.Terminal
  showCatTok = case _, _ of
    P.EOF, P.EOF -> text_ "$ (EOF)"
    P.Continue (Similar (Left s)), P.Continue s' | s == s' -> text_ (show s)
    P.Continue (Similar (Right r)), P.Continue s -> text_ (show s <> " (" <> show r <> ")")
    _, _ -> text_ "??"

  part = case _ of
    P.NonTerminal (Right v) -> text_ v
    P.NonTerminal (Left v) -> text_ (v <> "#")
    P.Terminal P.EOF -> text_ "$"
    P.Terminal (P.Continue (Similar (Left s))) -> text_ (show s)
    P.Terminal (P.Continue (Similar (Right r))) -> text_ (show r)
    P.InterTerminal _ -> unsafeCrashWith "Comb.part"

