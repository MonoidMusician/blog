module Runtime.Live where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Compactable (compact, separate)
import Data.Either (Either(..), either)
import Data.Filterable (filter, filterMap)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spyWith)
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Idiolect (intercalateMap, (<#?>), (\|/))
import JSURI (encodeURIComponent)
import Parser.Languages.HTML as HTML
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (ParseError(..), RecoveredError, printParseError)
import PureScript.CST.Layout as CST.Layout
import PureScript.CST.Lexer as CST.Lexer
import PureScript.CST.Parser as CST.Parser
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Print as CST.Print
import PureScript.CST.TokenStream as CST.TokenStream
import PureScript.CST.Types as CST.T
import PureScript.Highlight as PureScript.Highlight
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon.Bones (($$), (.$), (=:=))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Breath (microtask)
import Riverdragon.Dragon.Wings (hatching, sourceCode)
import Riverdragon.River (Lake, River, Stream, dam, makeLake, subscribe)
import Riverdragon.River as River
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (affToLake, counter)
import Runtime (cacheAff, fetchText, specialCache)
import Runtime as Runtime
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Widget (sessionStorageInterface)

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

--------------------------------------------------------------------------------
-- Module manipulation functions
--------------------------------------------------------------------------------

overrideValue ::
  { nameSearch :: CST.T.Ident
  , exprReplace :: CST.T.Expr Void
  } -> CST.T.Module Void -> CST.T.Module Void
overrideValue { nameSearch, exprReplace } template = template #
  ( _Newtype <<< prop (Proxy :: Proxy "body")
  <<< _Newtype <<< prop (Proxy :: Proxy "decls")
  <<< traversed
  ) case _ of
    CST.T.DeclValue decl@{ name: CST.T.Name { name } } | name == nameSearch ->
      CST.T.DeclValue decl { guarded = guarded }
      where
      guarded =
        CST.T.Unconditional (fakeTok CST.T.TokEquals) $
          CST.T.Where
            { bindings: Nothing
            , expr: exprReplace
            }
    decl -> decl

renameModuleTo :: String -> CST.T.Module Void -> CST.T.Module Void
renameModuleTo = identity
  <<< _Newtype <<< prop (Proxy :: Proxy "header")
  <<< _Newtype <<< prop (Proxy :: Proxy "name")
  <<< _Newtype <<< \newName _ ->
    { name: CST.T.ModuleName newName
    -- slight hack: should be TokUpperName (Just prefix) suffix
    , token: fakeTok $ CST.T.TokUpperName Nothing newName
    }

addImports :: Array (CST.T.ImportDecl Void) -> CST.T.Module Void -> CST.T.Module Void
addImports = identity
  <<< _Newtype <<< prop (Proxy :: Proxy "header")
  <<< _Newtype <<< prop (Proxy :: Proxy "imports")
  <<< flip (<>)

addDecls :: Array (CST.T.Declaration Void) -> CST.T.Module Void -> CST.T.Module Void
addDecls = identity
  <<< _Newtype <<< prop (Proxy :: Proxy "body")
  <<< _Newtype <<< prop (Proxy :: Proxy "decls")
  <<< flip (<>)

filterDecls :: (CST.T.Declaration Void -> Boolean) -> CST.T.Module Void -> CST.T.Module Void
filterDecls = identity
  <<< _Newtype <<< prop (Proxy :: Proxy "body")
  <<< _Newtype <<< prop (Proxy :: Proxy "decls")
  <<< filter

-- | Compile against the TryPureScript API, returning JavaScript source.
compile :: String -> Aff (Either (Array String) String)
compile moduleString = do
  compileURL <- Runtime.configurable "compileURL" "https://tryps.veritates.love/compile"
  response <- fetch compileURL
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

bundle :: String -> String -> String
bundle codeURL js =
  -- Adapted from TryPureScript client
  let
    importExportRegex :: Regex.Regex
    importExportRegex = either (\_ -> unsafeCrashWith "Invalid regex") identity
      $ Regex.regex """ from "../([^"]+)";$""" RegexFlags.noFlags
    replacement = " from \"" <> codeURL <> "/$1\";"
    codeWithRemappedImports = js
      # String.split (String.Pattern "\n")
      # map (Regex.replace importExportRegex replacement)
      # String.joinWith "\n"
  -- Actually call the `main` function
  in codeWithRemappedImports <> "\n\nmain();\n"

data Status
  = Fetching String
  | ParseErrors Boolean (NonEmptyArray CST.M.PositionedError)
  | Compiling String
  | CompileErrors (Array String)
  | Compiled String
  | Crashed String

fetchTemplate :: String -> Aff
  (Either (NonEmptyArray CST.M.PositionedError) (CST.T.Module Void))
fetchTemplate templateURL =
  cacheAff (specialCache (Proxy :: Proxy (CST.T.Module _)) "templateCache") [templateURL] do
    runRecoveredParser (Left identity) <$> fetchText templateURL

compileInterface :: forall flow. String -> (Stream flow String -> Dragon) -> String -> Dragon
compileInterface sessionStorageName embed df = hatching \shell -> do
  editorValue@{ send: setValue } <- sessionStorageInterface <@> sessionStorageName
  let valueSet = fromMaybe df <<< filter (notEq "") <$> River.alwaysBurst editorValue.loopback
  defaultValue <- fromMaybe df <<< Array.last <$> River.burstOf valueSet
  lastValue <- shell.storeLast defaultValue valueSet
  { stream: compiling, send: compileNow } <- shell.track $ River.createRiverStore Nothing
  pure $ D.Fragment
    [ sourceCode "PureScript" .$ D.textarea
        [ D.onInputValue =:= setValue
        , D.value =:= defaultValue
        , D.style =:= "height: 40svh"
        , D.asCodeInput
        ]
    , D.div.$ D.buttonW "" "Compile!" (compileNow =<< lastValue)
    , embed compiling
    ]

-- | Split off useful rivers from the main status stream.
ofPipeline :: Lake Status -> Effect
  { asScript :: River Dragon
  , bundled :: River String
  , compileStatus :: River Dragon -- Does not include when you reset stuff!
  , compiled :: River String
  , pipelined :: River Status
  , templated :: River String
  , highlighted :: Dragon
  , destroy :: Effect Unit
  }
ofPipeline pipelinedLake = do
  { stream: pipelined, destroy: destroy0 } <- River.store $ pipelinedLake
  { stream: templated, destroy: destroy1 } <- River.store $ pipelined # filterMap case _ of
    Compiling code -> Just code
    _ -> Nothing
  { stream: compiled, destroy: destroy2 } <- River.store $ pipelined # filterMap case _ of
    Compiled code -> Just code
    _ -> Nothing
  -- the browser does not want to eval the same JavaScript script tag again,
  -- so we prepend a unique int
  { stream: bundled, destroy: destroy3 } <- River.store $
    map (\(code /\ i) -> "/*"<>show i<>"*/" <> code) $
      counter $ pipelined <#?> case _ of
        Compiled result -> Just result
        _ -> Nothing
  let
    preScroll = D.pre [ D.style =:= "overflow-x: auto" ]
    asScript = bundled <#> \latestBundle ->
      D.script
        -- We need to set `type="module"` before `src` in Riverdragon, because
        -- it applies attributes in order!
        [ D.prop "type" =:= "module"
        , D.attr "src" =:= do
            fromMaybe "" $ encodeURIComponent latestBundle <#> \escaped ->
              "data:text/javascript;utf8," <> escaped
        ]
  pure
    { pipelined, templated, compiled
    , destroy: destroy0 <> destroy1 <> destroy2 <> destroy3
    , bundled, asScript, highlighted: highlighting templated
    , compileStatus:
        pipelined <#> case _ of
          Fetching _ -> D.text "Fetching PS template ..."
          ParseErrors inTemplate errors -> preScroll $ errors #
            intercalateMap (D.br[]) (D.text <<< printPositionedError)
          Compiling _ -> D.text "Typechecking PS, compiling to JS ..."
          CompileErrors errs -> preScroll $ errs #
            intercalateMap (D.br[]) D.text
          Compiled _ -> D.text "Loading many ES modules ..."
          Crashed err -> preScroll $$ err
    }

-- | Run a compile server for the passed in source code
pipeline :: forall parsed.
  { templateURL :: String
  , parseUser :: Either (CST.T.Module Void -> parsed Void) (CST.M.Parser (parsed RecoveredError))
  , templating :: CST.T.Module Void -> parsed Void -> CST.T.Module Void
  } -> Lake String -> Lake Status
pipeline behavior incoming = makeLake \sub -> do
  cancel <- Bed.rolling
  let
    cancelFiber fiber = launchAff_ (Aff.killFiber (error "Canceled due to new input") fiber)
    superviseAff :: forall r. Aff r -> (Either Aff.Error r -> Effect Unit) -> Effect Unit
    superviseAff act cb = do
      -- Microtask keeps the Aff from canceling itself ^^;
      fiber <- act # runAff \r -> microtask (cancel mempty *> cb r)
      cancel (cancelFiber fiber)
  unsub <- subscribe incoming \userString -> do
    cancel mempty
    sub $ Fetching behavior.templateURL
    superviseAff (fetchTemplate behavior.templateURL) case _ of
      Left kaboom -> do
        sub $ Crashed $ message kaboom
      Right (Left errs) -> do
        sub $ ParseErrors false errs
      Right (Right template) -> do
        case runRecoveredParser behavior.parseUser userString of
          Left errs -> sub $ ParseErrors false errs
          Right userParsed -> do
            let assembled = TC.printModule $ behavior.templating template userParsed
            sub $ Compiling assembled
            superviseAff (compile assembled) case _ of
              Left kaboom -> do
                sub $ Crashed $ message kaboom
              Right (Left err) -> do
                sub $ CompileErrors err
              Right (Right compiled) -> do
                codeURL <- Runtime.configurable "codeURL" "https://tryps.veritates.love/assets/purs"
                sub $ Compiled $ bundle codeURL compiled
  pure do
    unsub
    cancel mempty

-- | Turn a token stream into an array of strings, for debugging.
lexemes :: CST.TokenStream.TokenStream -> Array String
lexemes = lexemes' \_ tok -> tok

lexemes' :: (CST.T.Token -> String -> String) -> CST.TokenStream.TokenStream -> Array String
lexemes' f = Array.reverse <<< Array.fromFoldable <<< go List.Nil
  where
  go acc = CST.TokenStream.step >>> case _ of
    CST.TokenStream.TokenCons { value } _ stream _ ->
      let tok = CST.Print.printTokenWithOption CST.Print.ShowLayout value
      in go (List.Cons (f value tok) acc) stream
    CST.TokenStream.TokenError _ error _ _ ->
      let err = printParseError error
      in List.Cons err acc
    CST.TokenStream.TokenEOF _ _ -> acc


-- | Lex a bunch of top-level decls (i.e. no module header)
lexing :: String -> CST.TokenStream.TokenStream
lexing str = CST.TokenStream.TokenStream $ pure $
    firstToken start (CST.Lexer.lexWithState layoutStart start str) layoutStart
  where
  start = { line: 0, column: 0 }
  firstToken = CST.TokenStream.TokenCons $ fakeTok $ CST.T.TokLayoutStart 0
  layoutStart =
    Tuple start CST.Layout.LytWhere List.:
    Tuple start CST.Layout.LytRoot List.:
    List.Nil

runRecoveredParser :: forall r.
  Either (CST.T.Module Void -> r Void) (CST.M.Parser (r RecoveredError)) ->
  String ->
  Either (NonEmptyArray CST.M.PositionedError) (r Void)
runRecoveredParser (Left fromModule) s =
  case flip CST.M.runParser CST.Parser.parseModule (CST.Lexer.lexModule s) of
    Right (_ /\ errs) | Just errs' <- NEA.fromArray errs -> Left errs'
    Right (r /\ _) -> Right (fromModule (unsafeCoerce r))
    Left err -> Left (pure err)
runRecoveredParser (Right p) s =
  case flip CST.M.runParser p (spyWith "lexemes" lexemes $ lexing s) of
    Right (_ /\ errs) | Just errs' <- NEA.fromArray errs -> Left errs'
    Right (r /\ _) -> Right (unsafeCoerce r)
    Left err -> Left (pure err)

printErrors :: NonEmptyArray CST.M.PositionedError -> String
printErrors = intercalateMap "\n" printPositionedError

printPositionedError :: CST.M.PositionedError -> String
printPositionedError { error, position } =
  "[" <> show (position.line + 1) <> ":" <> show (position.column + 1) <> "] " <> printParseError error

fetchHighlight :: String -> Dragon
fetchHighlight url = highlighting $ compact (affToLake (fetchText url))

highlighting :: forall flow. Stream flow String -> Dragon
highlighting stream = D.Replacing $ dam stream <#>
  do PureScript.Highlight.highlight' >>> Dodo.print HTML.toDragon Dodo.twoSpaces >>> sourceCode "PureScript" []


data ImportsExprDecls e =
  ImportsExprDecls
    (Array (CST.T.ImportDecl e))
    (CST.T.Expr e)
    (Array (CST.T.Declaration e))

-- | Parse a file with a single expression amongst many other declarations
-- | (no module header).
importsExprDecls :: CST.M.Parser (ImportsExprDecls RecoveredError)
importsExprDecls = ado
  tokLayoutStart
  _ <- CST.M.many tokLayoutSep -- Leading comments and such
  anyDecls1 <- parseAnyDecls
  expr <- CST.Parser.parseExpr <* sep
  anyDecls2 <- parseAnyDecls
  tokLayoutEnd <* CST.M.eof
  let
    { left: imports1, right: decls1 } = separate anyDecls1
    { left: imports2, right: decls2 } = separate anyDecls2
    imports = imports1 <> imports2
    decls = decls1 <> decls2
  in ImportsExprDecls imports expr decls
  where
  parseAnyDecls = CST.M.many ((CST.Parser.parseImportDecl \|/ CST.M.try CST.Parser.parseDecl) <* sep)

  sep = tokLayoutSep <|> CST.M.lookAhead tokLayoutEnd

  expect f = CST.M.take \{ value } ->
    if f value then pure unit else
    Left $ UnexpectedToken value

  tokLayoutStart = expect case _ of
    CST.T.TokLayoutStart _ -> true
    _ -> false

  tokLayoutEnd = expect case _ of
    CST.T.TokLayoutEnd _ -> true
    _ -> false

  tokLayoutSep = expect case _ of
    CST.T.TokLayoutSep _ -> true
    _ -> false

