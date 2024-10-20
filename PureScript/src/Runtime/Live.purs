module Runtime.Live where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Compactable (compact)
import Data.Either (Either(..), either)
import Data.Lens (traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Dodo as Dodo
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, message, runAff, throwError)
import Effect.Aff as Aff
import Effect.Console (log)
import Fetch (Method(..), fetch)
import Foreign (readArray, readString)
import Foreign.Index (readProp)
import Idiolect (intercalateMap)
import Parser.Languages.HTML as HTML
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST.Errors (RecoveredError, printParseError)
import PureScript.CST.Lexer as CST.Lexer
import PureScript.CST.Parser as CST.Parser
import PureScript.CST.Parser.Monad as CST.M
import PureScript.CST.Types as CST.T
import PureScript.Highlight as PureScript.Highlight
import Riverdragon.Dragon (Dragon)
import Riverdragon.Dragon as D
import Riverdragon.Dragon.Breath (microtask)
import Riverdragon.Dragon.Wings (sourceCode)
import Riverdragon.River (Lake, Stream, dam, makeLake, subscribe)
import Riverdragon.River.Bed as Bed
import Riverdragon.River.Beyond (affToLake)
import Runtime (cacheAff, fetchText, specialCache)
import Runtime as Runtime
import Tidy.Codegen as TC
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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
                log ":3"
                codeURL <- Runtime.configurable "codeURL" "https://tryps.veritates.love/assets/purs"
                sub $ Compiled $ bundle codeURL compiled
  pure do
    unsub
    cancel mempty

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
  case flip CST.M.runParser p (CST.Lexer.lex s) of
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
