{-# LANGUAGE DeriveAnyClass, DerivingVia, OverloadedStrings, DefaultSignatures, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Replace case with maybe" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Use record patterns" -}
{- HLINT ignore "Use const" -}

-- This is a small experiment in generating WASM from a functional language. The
-- goal is to experiment with the Haskell side of this, making sure everything
-- is extensible. Focusing mostly on how isomorphic types can be compiled
-- in various ways, and what kind of optimizations can come from the functional
-- layer (not how the WASM backend itself can be optimized). The theory is that
-- a lot of work has been done on optimizing low level details in the appropriate
-- runtimes and JITs, but the biggest wins are often from optimizing the high
-- level stuff first, especially where types and effects are known.
--
-- The goal is to have extensible user types (with their own codegen),
-- extensible code (embedding foreign code and typeclass-like generated code),
-- and extensible analyses. See https://blog.veritates.love/functions_as_data.html
--
-- The main ingredients are:
--
-- - WASM backend:
--   - A declarative model of WASM GC types
--   - A simple "AST" for WASM (*sparkling text*, with parentheses and spaces)
--   - A monad to assemble WASM, generating functions and types as needed
-- - A functional front-end:
--   - a really simple AST for expressions
--   - custom types, STyp/SynthType, that control their own codegen
--   - intrinsic types and strict type checking
--   - a basic parser
-- - Eventually: static analysis and optimizations
--
-- Interesting optimizations:
-- - Fusing traversals and accumulation, like map/reduce fusion, but where you
--   keep the intermediate list too?
--   - In general, I think there is room for a standard library to have a
--     handful of general purpose fused align-map-reduce-with-key operations and
--     derive more specialized implementations from that
-- - Optimized data types for the desired operations
--   - string/list builders
--   - transparently memoizing information like list size
-- - Imperative-like code:
--   - Using mutable variables instead of distantly threaded state
--   - Using control flow instead of repeated case analysis
--   - Generate code like hand-written imperative loops!
--   - Depends on effect analysis to pull off
-- - More standard analyses
--   - Unpacking on the stack where possible, ignoring unused fields
--   - Functions vs closures
-- - Out of scope: laziness and strictness analysis
module WASMFP where

import Prelude hiding (unwords, break, filter, lex)
import Data.Word (Word64, Word32)
import Data.Int (Int64, Int32)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import Data.Data (Data, Typeable, Proxy(..))
import Control.DeepSeq (NFData(rnf), force)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Control.Monad.Reader.Class (MonadReader(ask, local))
import Witherable (Filterable(mapMaybe, filter))
import Control.Monad (when, unless, void, join, zipWithM_, (<=<))
import qualified Data.Typeable as Typeable
import Control.Monad.RWS.CPS (RWST, MonadState (state), MonadWriter (tell, listen, pass), modify', censor, gets, runRWST, MonadTrans (lift), MonadIO (liftIO))
import Data.Functor ((<&>))
import qualified Data.Text.Internal.StrictBuilder as Builder
import Data.Foldable (traverse_, for_, Foldable (toList), foldrM, fold, foldMap, asum)
import qualified Data.ByteString.Builder as ByteString.Builder
import GHC.Base (Semigroup(sconcat), NonEmpty((:|)), coerce, Coercible, Alternative (many), (<|>), empty, Type)
import Data.Foldable1 (Foldable1 (foldMap1))
import Data.String (IsString (fromString))
import Data.Function (on, (&))
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import Data.Traversable (for)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Char (isPrint, chr, isAscii, isAlpha, isAlphaNum)
import Data.Bits ((.&.), (.>>.), (.<<.))
import qualified Data.Text.IO.Utf8 as T
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PPT
import Data.Monoid (All(All), Any (Any), Sum (Sum), Product (Product))
import Control.Category ((>>>))
import Data.Semigroup (Min (Min), Max (Max))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified ParseMachine as P
import Control.Applicative (optional)
import GHC.IO (unsafePerformIO, evaluate)
import Control.Exception (try, ErrorCall (ErrorCall))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Error.Class (MonadError(throwError))
import qualified System.Command as Cmd
import System.Exit (ExitCode(ExitSuccess))
import qualified Debug.Trace as Dbg
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import Data.Either (fromRight)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State.Strict (evalState)
import qualified Data.List.NonEmpty as NEL
import GHC.Stack (HasCallStack)
import Control.Applicative.Backwards (Backwards(Backwards, forwards))

-- Invoke wasmtime (from $PATH) with the given WASM source, with a main()
-- function. Prints the source on error.
execWASM :: Text -> IO Text
execWASM fullModule = do
  (Cmd.Exit status, Cmd.Stdout result) <- Cmd.command [ Cmd.Stdin (T.unpack fullModule) ] "wasmtime"
    [ "run", "-W", "all-proposals", "--invoke", "main", "-" ]
  when (status /= ExitSuccess) do
    T.putStrLn fullModule
    error "Running WASM module failed"
  pure $ T.pack result

-- Generate a function to show a type.
-- Falls back to displaying the typename.
showT :: STyp -> Expr
showT t = mkShowFun case t of
  S TBool -> byCases
    [ ("True", (Map.empty, ETxt "True"))
    , ("False", (Map.empty, ETxt "False"))
    ]
  S TTxt -> vvalue
  _ -> fallback
  where
  mkShowFun = EFun (SFun [t] STxt) [Bind "value"]
  vvalue = EVar t "value"
  byCases = ECase t vvalue STxt . (, fallback) . Map.fromList
  fallback = ETxt $ "<" <> T.show t <> ">"

newtype CShow = CShow STyp
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

pattern SemShow :: STyp -> ForeignSemantics
pattern SemShow t = Sem (CShow t)

instance ToWASM CShow
instance ToExpr CShow where
  toExpr (CShow t) = mkShowFun case t of
    S TBool -> byCases
      [ ("True", (Map.empty, ETxt "True"))
      , ("False", (Map.empty, ETxt "False"))
      ]
    S TTxt -> vvalue
    _ -> fallback
    where
    mkShowFun = EFun (SFun [t] STxt) [Bind "value"]
    vvalue = EVar t "value"
    byCases = ECase t vvalue STxt . (, fallback) . Map.fromList
    fallback = ETxt $ "<" <> T.show t <> ">"
instance ToFuncDef CShow where
  toFuncDef (CShow t) = FuncDef Nothing [wtyp t] [wtyp TIOData] []
    (toWASM (CShow t))
instance Semantics CShow where
  semTyp (CShow t) = SFun [t] (SIOData)
  semCode (CShow t) = funcCall (CShow t)


-- Some basic tests for this module.
main :: IO ()
main = do
  let
    u32s = SArr SU32
    u32ss = SArr u32s
    texts = SArr (STyp TTxt)
    dropExpr e = genExpr e <* tell "drop"
    example name = genFunc (Just name) ([], []) []
    exampleT name oTs = genFunc (Just name) ([], wtyp <$> oTs) []
    stdprint = ESem (ForeignCall (SFun [STxt] SU32) mempty "print")
    stdprintln = EFun (SFun [STxt] SU32) [Bind "input"] $
      ELet
        [ (EApp stdprint [EVar STxt "input"], Bind "len")
        , (EApp stdprint [ETxt "\n"], Discard)
        ] $ EVar SU32 "len"

    showBool = EFun (SFun [S TBool] STxt) [Bind "cond"] $
      ECase (S TBool) (EVar (S TBool) "cond") STxt
        ( Map.singleton "True" (Map.empty, ETxt "True")
        , ETxt "False"
        )
  let
    go = do
      t <- T.getLine
      when (t /= "") do
        T.putStrLn $ T.show $ lex t
        T.putStrLn $ T.show $ run parseType t
        let parsed = run parseSomeExpr t
        T.putStrLn $ T.show $ parsed
        case parsed of
          Left _ -> pure ()
          Right expr -> do
            (_, _, code) <- runWASM $ genExpr expr
            T.putStrLn $ genWASMC code
            T.putStrLn $ T.show $ eval expr Map.empty
        T.putStrLn $ T.show $ parse t
        go
  -- T.putStrLn "Ready!"
  -- go
  fullModule <- genWAT do
    testFn1 <- genFunc (Just "retest") ([], [wtyp SU32]) [] do
      genExpr $ ELet [(EU64 157842, Bind "here")] (EU32 1312)
    testFn2 <- genFunc (Just "simple") ([], []) [] do
      withVar (Just "here") SU64 \local1 -> do
        sexp "local.get" [ local1 ]
        tell "drop"
      withVar (Just "here") SU64 \local1 -> do
        sexp "local.get" [ local1 ]
        tell "drop"
        withVar (Just "here2") SU32 \local2 -> do
          sexp "local.get" [ local2 ]
          tell "drop"
          withVar (Just "here3") SU64 \local3 -> do
            sexp "local.get" [ local3 ]
            tell "drop"
    tell $ PARENS [ "i32.const", "42" ]
    tell "drop"
    sexp "call" [ testFn1 ]
    sexp "call" [ testFn2 ]
    tell "drop"
    dropExpr $ EArray (S $ TArr SU32) []
    _ <- example "nested_array" do
      dropExpr $ ELet
        [ (EArray u32s [EU32 0, EU32 1], "arr") ]
        $ EArray u32ss
          [ EVar u32s "arr"
          , EArray u32s []
          , EVar u32s "arr"
          ]
    _ <- example "bitarray13_literal" do
      dropExpr $ EArray (S TBitarray)
        [ ETrue, EFalse, ETrue, ETrue ]
    _ <- example "bitarray13_computes" do
      dropExpr $ EArray (S TBitarray)
        [ eIf ETrue ETrue ETrue, eIf EFalse EFalse EFalse, ETrue, ETrue ]
    _ <- example "if_true" do
      dropExpr $ eIf ETrue EFalse ETrue
    _ <- example "if_tuple_true" do
      dropExpr $ eIf (eFst $ eTuple ETrue EFalse) EFalse ETrue
    _ <- exampleT "if_tuples" [S $ TTuple (S TBool) SU32] do
      genExpr $ eIf (eSnd $ eTuple ETrue EFalse) (eTuple ETrue (EU32 1)) (eTuple EFalse (EU32 0))
    _ <- example "test" do
      dropExpr $ ETxt "¡test!"
    dropExpr $ EArrayFill (SArr SU32) (EU32 4) $ EFun (SFun [SU32] SU32) [Bind "idx"] $
      EApp stdprint [EIndex mempty (EArray texts $ ETxt . (<> "\n") <$> T.words "test me hellow wowlrd") (EVar SU32 "idx")]
    dropExpr $ EApp stdprint [ETxt "test me hellow wowlrd\n"]
    censor mempty $ funcDef IOByteLength
    censor mempty $ funcDef IOFlatten
    pure ()
  -- T.putStrLn fullModule
  T.writeFile "/tmp/test.wat" fullModule
  T.putStrLn =<< execWASM fullModule
  T.putStrLn =<< execWASM =<< genWAT do
    genExpr ETrue
    genExpr showBool
    genExpr stdprintln
    tell "drop"
    genExpr EFalse
    genExpr showBool
    genExpr stdprintln
    tell "drop"
  let
    recTest = do
      let below = Just (RRef "list")
      _ <- regTypeGroup $ Map.fromList
        [ ("list", plainTyp $ WS [])
        , ("nil", SubTyp False below $ WS [])
        , ("cons", SubTyp False below $ WS [Mut WI64, Mut $ WR Nul $ HRTyp $ RRef "list"])
        ]
      _ <- regCTyp (Just "closure") baseClosureC
      _ <- genWTyp $ wtyp $ TClo
        (Just (HCTyp (WS [Mut WI64])))
        (Just (HCTyp (WF [WI64] [WI64])))
      pure ()
  T.putStrLn =<< genWAT recTest
  -- T.putStrLn =<< execWASM =<< genWAT recTest
  pure ()

-- Translate from the expression type to WASM code (written into the writer
-- monad, of course with all dependencies being written into the environment).
-- The syntax is really basic, but ADTs and arrays have type-directed codegen,
-- so they can have any behavior as given by typeclass implementations.
genExpr :: Expr -> WASM
genExpr = \case
  EF64 v -> tell $ genConst v
  EF32 v -> tell $ genConst v
  EU64 v -> tell $ genConst v
  EU32 v -> tell $ genConst v
  ES64 v -> tell $ genConst v
  ES32 v -> tell $ genConst v
  ETxt v -> do
    ty <- genType (Just "Text") (STyp TTxt)
    let bin = ByteString.UTF8.fromString (T.unpack v)
    let n = ByteString.length bin
    i <- fastSlow
      do gets (Map.lookup bin . binlits)
      do
        (i, segment) <- gets rodata
        modify' \modul -> modul
          { rodata = (i + n, segment <> ByteString.Builder.byteString bin)
          , binlits = Map.insert bin i (binlits modul)
          }
        pure i
    sexp "array.new_data" [ ty, "$.rodata", genConst (I i :: Word32), genConst (I n :: Word32) ]
  EError _ _ msg -> tell $ "unreachable" <> COMMENT msg

  ELet binds e -> foldr doBind (genExpr e) binds
  EVar _ name -> do
    lexScope <- ask <&> lexical
    sexp "local.get" [ lexScope ! name ## coerce name ]

  EFun t binds body -> do
    (inputs, output) <- case t of
      SFun inputs output -> pure (inputs, output)
      _ -> error "Not a function type"
    fn <- genFunc Nothing (wtyp <$> inputs, [wtyp output]) binds (genExpr body)
    tell $ QUAL "call" fn
  EApp fun args -> do
    traverse_ genExpr args
    genExpr fun

  ECons t name fields -> do
    (mkconstr t ! name) ((,) <*> genExpr <$> Map.fromList fields)
  ECase t scrutinee o (cases, fallback) -> do
    genExpr scrutinee
    desc <- case isDataType t of
      Nothing -> error "Not a data type"
      Just d -> pure d
    unconstr t [wtyp o] (Map.mapWithKey (bindCase desc) cases, genExpr fallback)

  EArray t items -> arraylit t ((,) <*> genExpr <$> items)
  EArrayFill t len getter -> arrayfill t (len, genExpr len) (getter, genExpr getter)
  ELength expr -> arraylength (infer expr) (expr, genExpr expr)
  EIndex _ arr idx -> arrayindex (infer arr) (arr, genExpr arr) (idx, genExpr idx)

  EForeign sem -> semCode sem
  where
  doBind :: (Expr, Bind) -> WASM -> WASM
  doBind (expr, Discard) = (genExpr expr *> tell "drop" *>)
  doBind (expr, Bind name) = \inner ->
    withLexVar (Just name) (infer expr) \(_, var) -> do
      genExpr expr
      sexp "local.set" [ var ## coerce name ]
      inner
  bindCase :: Map Name [(Name, STyp)] -> Name -> (Map Name Bind, Expr) -> Map Name WASM -> WASM
  bindCase desc caseName (requested, body) available = do
    let
      fieldTypes :: Map Name STyp
      fieldTypes = Map.fromList $ desc ! caseName
      bindField (_, Discard) inner = inner
      bindField (fieldName, Bind name) inner = do
        withLexVar (Just name) (fieldTypes ! fieldName) \(_, vbound) -> do
          available ! fieldName
          sexp "local.set" [ vbound ## coerce name ]
          inner
    foldr bindField (genExpr body) (Map.toList requested)


--------------------------------------------------------------------------------
-- Miscellaneous helpers/background
--------------------------------------------------------------------------------

(<@>) :: Functor f => f (a -> b) -> a -> f b
ff <@> a = (\f -> f a) <$> ff

(/|\) :: Applicative f => f a -> f b -> f (a, b)
(/|\) = liftA2 (,)

(\|/) :: Alternative f => f a -> f b -> f (Either a b)
(\|/) = \l r -> Left <$> l <|> Right <$> r

(!) :: Ord k => Map k v -> HasCallStack => k -> v
kvs ! k = case Map.lookup k kvs of
  Nothing -> error "Missing key"
  Just v -> v

-- Lookup for associative lists
listup :: forall k v. Eq k => k -> [(k, v)] -> Maybe v
listup k = fmap snd . List.find ((== k) . fst)

-- Grab the output of a monadic action *instead* of writing it
outputOf :: forall w m. MonadWriter w m => m () -> m w
outputOf = censor mempty . fmap snd . listen

-- Try the fast path first then the slow path
fastSlow :: forall m r. Monad m => m (Maybe r) -> m r -> m r
fastSlow fastPath slowPath = do
  firstTry <- fastPath
  case firstTry of
    Just r -> pure r
    Nothing -> slowPath


-- A really useful pattern synonym! why have i not seen it before?
pattern Coerce :: Coercible outer inner => inner -> outer
pattern Coerce c <- (coerce -> c) where
  Coerce c = coerce c

-- Also useful: coerce integral types
pattern I :: (Integral b, Integral a) => (Integral a) => b -> a
pattern I i <- (fromIntegral -> i) where
  I i = fromIntegral i

-- Datatype for places where something can be recursive or not
data Recursive rec non = Recurse rec | Direct non
  deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

recurses :: forall rec non. (rec -> non) -> Recursive rec non -> non
recurses f (Recurse r) = f r
recurses _ (Direct r) = r



-- A binding (for pattern matching): currently a name or an underscore.
data Bind
  = Bind Name
  | Discard
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
instance IsString Bind where fromString = Bind . fromString

-- A name (could be interned).
newtype Name = Name Text
  deriving newtype (Eq, Ord, Show, NFData, IsString)
  deriving stock (Generic, Data)

-- Unnamed
unn :: Name
unn = Name mempty

-- Numbers, as a lazy list, just for caching
_nums :: [Text]
_nums = T.pack . show <$> enumFrom @Int 0

_intNames :: [Name]
_intNames = coerce _nums



--------------------------------------------------------------------------------
-- The functional programming language side of things
--------------------------------------------------------------------------------


-- The AST for expressions
data Expr
  -- Literals
  = EF64 !Double
  | EF32 !Float
  | EU64 !Word64
  | EU32 !Word32
  | ES64 !Int64
  | ES32 !Int32
  | ETxt !Text

  -- Error, with its result type and severity in Effect
  | EError STyp Effect !Text

  -- A named constructor with named fields in order of evaluation
  | ECons STyp !Name ![(Name, Expr)]
  -- A case construct: scrutinize the left expression of the given type,
  -- handle each case (binding the fields to variables for the expression)
  -- and a fallback case, which can be an error
  | ECase STyp Expr STyp (Map Name (Map Name Bind, Expr), Expr)
  -- An array literal
  | EArray STyp [Expr]
  -- Fill an array by giving its length and a function called for each index
  | EArrayFill STyp Expr Expr
  -- Get the length of an array
  | ELength Expr
  -- Unsafe index into an array
  | EIndex Effect Expr Expr

  -- A let expression: multiple names to bind in order (not recursive let)
  | ELet [(Expr, Bind)] Expr
  -- A variable reference
  | EVar STyp !Name
  -- An n-ary lambda expression
  | EFun STyp [Bind] Expr
  -- Function application (n-ary)
  | EApp Expr [Expr]

  -- Foreign code
  | EForeign ForeignSemantics
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (NFData)

_genConst :: Show t => Text -> t -> WASMC
_genConst t v = QUAL (t <> ".const") (INSTR $ T.show v)

class ConstExpr t where
  constExpr :: t -> Expr
  genConst :: t -> WASMC
instance ConstExpr Float where
  constExpr = EF32
  genConst = _genConst "f32"
instance ConstExpr Double where
  constExpr = EF64
  genConst = _genConst "f64"
instance ConstExpr Word64 where
  constExpr = EU64
  genConst = _genConst "i64"
instance ConstExpr Word32 where
  constExpr = EU32
  genConst = _genConst "i32"
instance ConstExpr Int64 where
  constExpr = ES64
  genConst = _genConst "i64"
instance ConstExpr Int32 where
  constExpr = ES32
  genConst = _genConst "i32"
instance ConstExpr Text where
  constExpr = ETxt
  genConst _ = undefined

-- Each expression is intrinsically typed, and furthermore, it should not
-- require great computation to get its type.
infer :: Expr -> STyp
infer = \case
  EF64 _ -> SF64
  EF32 _ -> SF32
  EU64 _ -> SU64
  EU32 _ -> SU32
  ES64 _ -> SS64
  ES32 _ -> SS32
  ETxt _ -> STyp TTxt
  EError t _ _ -> t
  ECons t _ _ -> t
  ECase _ _ o _ -> o
  EArray t _ -> t
  EArrayFill t _ _ -> t
  ELength _ -> S $ TPrm Uns WI32
  EIndex _ arr _ -> case isArrayType (infer arr) of
    Just (TArr field) -> field
    _ -> error "Not an array type"
  ELet _ e -> infer e
  EVar t _ -> t
  EFun t _ _ -> t
  EApp fun args -> case infer fun of
    SFun inputs output ->
      case length args `compare` length inputs of
        EQ -> output
        LT -> SFun (drop (length args) inputs) output
        GT -> error "Too many arguments to function"
    _ -> error "Not a function type"
  EForeign sem -> semTyp sem

-- Fully typecheck an expression
check :: Expr -> ReaderT (Map Name STyp) (Either Text) ()
check = \case
  EF64 _ -> pure ()
  EF32 _ -> pure ()
  EU64 _ -> pure ()
  EU32 _ -> pure ()
  ES64 _ -> pure ()
  ES32 _ -> pure ()
  ETxt _ -> pure ()
  EError _ _ _ -> pure ()
  ECons t name (Map.fromList -> provided) -> do
    spec <- case isDataType t of
      Nothing -> throwError "Not a data type"
      Just spec -> pure spec
    expected <- case Map.lookup name spec of
      Nothing -> throwError "Constructor not named in the data type"
      Just expected -> pure expected
    for_ expected \(fieldName, fieldType) -> do
      case Map.lookup fieldName provided of
        Nothing -> throwError "Missing field"
        Just value -> expect fieldType value
      -- TODO: warning for excess fields
  ECase t e o (cases, fallback) -> do
    expect t e
    spec <- case isDataType t of
      Nothing -> throwError "Not a data type"
      Just spec -> pure spec
    for_ (Map.intersectionWith (,) spec cases) \(Map.fromList -> given, (binds, body)) -> do
      bounds <- for (Map.mapWithKey (,) binds) \(name, bind) ->
        case (Map.lookup name given, bind) of
          (Nothing, Bind _) -> throwError "Field not present"
          -- TODO: warning for excess fields
          (Nothing, Discard) -> pure (undefined, Discard)
          (Just ty, _) -> pure (ty, bind)
      local (<> gatherBinders id bounds) do
        expect o body
    expect o fallback
  EArray t items -> do
    inner <- unArray t
    traverse_ (expect inner) items
  EArrayFill t size getter -> do
    inner <- unArray t
    expect SU32 size
    expect (SFun [SU32] inner) getter
  ELength e -> do
    void $ unArray =<< inferCheck e
  EIndex _ _ i -> expect SU32 i

  ELet [] body -> check body
  ELet ((value, Discard) : binds) body -> do
    check value
    check (ELet binds body)
  ELet ((value, Bind name) : binds) body -> do
    check value
    local (Map.insert name (infer value)) do
      check (ELet binds body)
  EVar t name -> do
    locals <- ask
    case Map.lookup name locals of
      Nothing -> throwError $ "Missing variable: " <> T.pack (show name)
      Just t' -> identical t t'
  EFun t args body -> do
    (inputs, output) <- unFun t
    when (length args > length inputs) do
      throwError "Too many arguments to function"
    local (<> gatherBinders id (zip inputs args)) do
      expect output body
  EApp fun args -> do
    (inputs, _output) <- unFun =<< inferCheck fun
    when (length args > length inputs) do
      throwError "Too many arguments to function"
    zipWithM_ expect inputs args
  EForeign _sem -> pure ()
  where
  unArray (isArrayType -> Just (TArr t)) = pure t
  unArray _ = throwError "Expected array type"
  unFun (SFun inputs output) = pure (inputs, output)
  unFun _ = throwError "Expected function type"

  inferCheck expr = infer expr <$ check expr
  identical tyE tyA | tyE /= tyA = throwError "Type mismatch"
  identical _ _ = lift $ Right ()
  expect ty expr = identical ty =<< inferCheck expr

-- A pure evaluator for an expression.
eval :: Expr -> Map Name Expr -> Expr
eval = \case
  e@(EF64 _) -> pure e
  e@(EF32 _) -> pure e
  e@(EU64 _) -> pure e
  e@(EU32 _) -> pure e
  e@(ES64 _) -> pure e
  e@(ES32 _) -> pure e
  e@(ETxt _) -> pure e
  e@(EError _ _ _) -> pure e
  ECons t name fields -> ECons t name <$> traverse (traverse eval) fields
  ECase _ value _ (cases, fallback) -> do
    (name, fields) <- eval value >>= \case
      ECons _ name fields -> pure (name, fields)
      _ -> error "Scrutinee not a constructor"
    let
      (bindFields, body) = Map.lookup name cases
        & fromMaybe (Map.empty, fallback)
      binds = gatherBinders id $
        Map.intersectionWith (,) (Map.fromList fields) bindFields
    local (<> binds) do eval body
  EArray t fields -> EArray t <$> traverse eval fields
  EArrayFill t size getter -> eval size >>= \case
    EU32 0 -> pure $ EArray t []
    EU32 sz -> eval $ EArray t $ (\i -> EApp getter [EU32 i]) <$> [0..(sz-1)]
    size' -> EArrayFill t size' <$> eval getter
  ELength (EArrayFill _ size _) -> pure size
  ELength arr -> do
    fields <- eval arr >>= \case
      EArray _ fields -> pure fields
      _ -> error "Not an array"
    pure $ EU32 $ I $ length fields
  EIndex _ (EArrayFill _ size getter) idx -> do
    sz <- eval size >>= \case
      EU32 sz -> pure sz
      _ -> error "Array size needs to be u32"
    i <- eval idx >>= \case
      EU32 i -> pure i
      _ -> error "Array index needs to be u32"
    when (i >= sz) do error "Array index out of bounds"
    eval $ EApp getter [EU32 i]
  EIndex _ arr idx -> do
    fields <- eval arr >>= \case
      EArray _ fields -> pure fields
      _ -> error "Not an array"
    i <- eval idx >>= \case
      EU32 i -> pure i
      _ -> error "Array index needs to be u32"
    pure $ fields !! I i
  ELet binds e ->
    local (<> gatherBinders id binds) do eval e
  EVar _ var -> do
    vars <- ask
    case Map.lookup var vars of
      Nothing -> error $ "Missing var: " <> show var
      Just value -> pure value
  EFun _ [] body -> eval body
  e@(EFun _ _ _) -> pure e
  EApp fun args -> eval fun >>= \case
    EFun (S funT) binds body -> do
      let
        added = gatherBinders id $ zip args binds
        binds' = drop (Map.size added) binds
        rest = S $ applySome (I $ Map.size added) funT
      local (<> added) do
        eval (EFun rest binds' body)
    EForeign sem -> fromMaybe (error "Cannot evaluate foreign semantics") <$> semEval sem args
    _ -> error "Not a function"
  EForeign _sem -> error "Cannot evaluate foreign semantics"

{-
@joinlift2 : (x -> y -> Maybe z) -> Maybe x -> Maybe y -> Maybe z
| f, Just x, Just y & f x y ?= Just z := Just z
| _, _, _ := Nothing

@joinlift2 : (x -> y -> Maybe z) -> Maybe x -> Maybe y -> Maybe z
| f, Just x, Just y & f x y ??
| _, _, _, Just z := Just z
| _, _, _, Nothing := Nothing
| _, _, _ := Nothing
-}

gatherBinders :: Foldable f => (i -> (o, Bind)) -> f i -> Map Name o
gatherBinders f = Map.fromList . mapMaybe (mkBinder . f) . toList
  where
  mkBinder (value, Bind name) = Just (name, value)
  mkBinder _ = Nothing

-- An existential that means that synthetic types are extendable by the
-- Haskell library user. `spec` must implement `Typeable` and so on.
data STyp = forall spec. SynthType spec => STyp !spec

instance Eq STyp where
  STyp spec0 == STyp spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 == spec2
    | otherwise = False
instance Ord STyp where
  STyp spec0 `compare` STyp spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 `compare` spec2
    | otherwise = Typeable.typeOf spec0 `compare` Typeable.typeOf spec2
instance NFData STyp where
  rnf (STyp spec) = rnf spec
instance Show STyp where
  show (STyp spec) = show spec

-- Unpack the existential, if it is the expected `spec` type.
pattern S :: SynthType spec => SynthType spec => spec -> STyp
pattern S spec <- STyp (Typeable.cast -> Just (spec :: spec)) where
  S spec = STyp spec

-- ADTs as a synthetic data type: many constructors with many fields each.
-- Each field can either be directly recursive (not mutually recursive), or an
-- external type. Substitute the recursion with `unrollTDat`.
newtype TDat = TDat (Map Name [(Name, Recursive () STyp)])
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SDat :: Map Name [(Name, Recursive () STyp)] -> STyp
pattern SDat constructors = S (TDat constructors)
-- Mutually recursive types
data TMutDat = TMutDat Name (Map Name (Map Name [(Name, Recursive Name STyp)]))
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SMutDat :: Name -> Map Name (Map Name [(Name, Recursive Name STyp)]) -> STyp
pattern SMutDat which group = S (TMutDat which group)
-- Plain functions as a synthetic data type (not closures)
data TFun = TFun [STyp] STyp
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SFun :: [STyp] -> STyp -> STyp
pattern SFun is o = S (TFun is o)
-- Data type underlying closures, with a closure type (containing the data)
-- and a dedicated (unboxed) function type.
data TClo = TClo (Maybe HTyp) (Maybe HTyp)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SClo :: Maybe HTyp -> Maybe HTyp -> STyp
pattern SClo dat fun = S (TClo dat fun)
-- Arrays
newtype TArr = TArr STyp
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SArr :: STyp -> STyp
pattern SArr t = S (TArr t)
-- Primitive types
data TPrm = TPrm Sgn WTyp
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SPrm :: Sgn -> WTyp -> STyp
pattern SPrm sgn typ = S (TPrm sgn typ)
-- Text type (byte array, UTF-8 assumed)
data TTxt = TTxt
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern STxt :: STyp
pattern STxt = S TTxt
-- IOList/IOData type (text or array of IOData)
data TIOL = TIOList | TIOData | TIOText
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
pattern SIOList :: STyp
pattern SIOList = S TIOList
pattern SIOData :: STyp
pattern SIOData = S TIOData
pattern SIOText :: STyp
pattern SIOText = S TIOText


--------------------------------------------------------------------------------
-- The WASMM monad for generating a WASM file
--------------------------------------------------------------------------------


-- WASM reader context
data WASMR = WASMR
  { scope :: Map WTyp [Int]
  -- ^ the indices of each local in scope of the specified type
  , lexical :: Map Name WASMC
  -- ^ WASM indices of each local in scope in the surface language
  }
-- WASM state context
data WASMS = WASMS
  { funcs :: GenIdx (Maybe Name, WASMC, WASMC) (([WTyp], [WTyp]), SomeWASM)
  -- ^ function bodies generated so far
  , types :: GenIdx (Maybe Name, Map Name SubTyp) WASMC
  -- ^ composite types generated so far (struct, array, func)
  , typeGroups :: GenIdx (Map Name WASMC) (Map Name SubTyp)
  -- ^ recursive type groups generated so far, with their internal naming
  , locals :: GenIdx (Maybe Name) ((WTyp, WASMC), Int)
  -- ^ locals by type and de Bruijn level
  , freshBlock :: Int
  -- ^ Fresh number for block names
  , globals :: Map Name ((WTyp, WASMC), WASMC)
  -- ^ global variables by type and initializer (FIXME)
  , initCode :: WASMC
  -- ^ init function
  , rwdata :: (Int, ByteString.Builder.Builder)
  -- ^ binary data, readwrite
  , rodata :: (Int, ByteString.Builder.Builder)
  -- ^ binary data, readonly
  , binlits :: Map ByteString.ByteString Int
  -- ^ location of existing binary literals in .rodata

  -- TODO: memory and tables, imports and exports
  }
-- WASM writer context: code for the current function/initializer
type WASMW = WASMC
-- The WASM monad
newtype WASMM t = WASM (RWST WASMR WASMW WASMS IO t)
  deriving newtype (Functor, Applicative, Monad, MonadReader WASMR, MonadState WASMS, MonadIO)
-- Writes WASM code
type WASM = WASMM ()
-- Returns WASM code
type WASMMC = WASMM WASMC


-- A helper for generating types, functions, et cetera, into a linear index
-- in a way so they are deduplicated. The idea is that they are deduplicated
-- by value, with its position in the index and metadata from the first
-- usage.
data GenIdx meta val = GenIdx
  !(Map val (Int, meta))
  ![(val, meta)] -- reverse order


-- Put a new item into the index
gen :: forall meta val. Ord val => GenIdx meta val -> val -> meta -> (GenIdx meta val, Int, meta)
gen g@(GenIdx validx stack) val newMeta =
  case Map.lookup val validx of
    Just (idx, oldMeta) -> (g, idx, oldMeta)
    Nothing ->
      let
        idx = Map.size validx
        g' = GenIdx (Map.insert val (idx, newMeta) validx) ((val, newMeta) : stack)
      in (g', idx, newMeta)

-- Look up an item in the index
isGenned :: forall meta val. Ord val => GenIdx meta val -> val -> Maybe (Int, meta)
isGenned (GenIdx validx _) val = Map.lookup val validx

-- Reserve a place in the index (with speculative metadata) and fulfill it later
-- with accurate metadata
reserve :: forall meta val. Ord val => GenIdx meta val ->
  val -> meta -> (GenIdx meta val, Int, meta -> GenIdx meta val -> GenIdx meta val)
reserve g@(GenIdx validx stack) val placeholder =
  case Map.lookup val validx of
    Just (idx, _) -> (g, idx, \_ _ -> g)
    Nothing ->
      let
        idx = Map.size validx
        g' = GenIdx (Map.insert val (idx, placeholder) validx) ((val, placeholder) : stack)
      in (g', idx, _fulfillAt val idx)

-- Helper for replacing an entry
_fulfillAt :: Ord val => val -> Int -> meta -> GenIdx meta val -> GenIdx meta val
_fulfillAt val idx newMeta (GenIdx validx' stack') =
  let (l, r) = List.break ((== val) . fst) stack'
  in GenIdx (Map.insert val (idx, newMeta) validx') (l <> ((val, newMeta) : List.drop 1 r))

-- Reserve multiple items for sure, without checking whether those values
-- exist already in the index. For dealing with mutual recursion, basically.
reserveNForSure ::
  forall t each collected ret meta val.
    Ord val =>
    Traversable t =>
  GenIdx meta val ->
  t each ->
  (t (each, Int) -> collected) ->
  (each -> Int -> collected -> (val, meta, (meta -> GenIdx meta val -> GenIdx meta val) -> ret)) ->
  (GenIdx meta val, t ((val, (Int, meta)), ret), collected)
reserveNForSure (GenIdx validx stack) each collect prelim =
  let
    numbered = Map.size validx & evalState do
      for each \e -> state \c -> ((e, c), c+1)
    collected = collect numbered
    adding :: t ((val, (Int, meta)), ret)
    adding = numbered <&> \(e, idx) ->
      let (val, meta, willFulfill) = prelim e idx collected
      in ((val, (idx, meta)), willFulfill (_fulfillAt val idx))
    placeholders = fst <$> toList adding
    validx' = Map.union (Map.fromList placeholders) validx
    stack' = reverse (fmap snd <$> placeholders) <> stack
  in (GenIdx validx' stack', adding, collected)


newGenIdx :: forall meta val. GenIdx meta val
newGenIdx = GenIdx Map.empty []

-- Return the list of items generated, in order
genned :: forall meta val. GenIdx meta val -> [(val, meta)]
genned (GenIdx _ stack) = List.reverse stack


-- An AST for WASM code. Well, not really. It is just a slightly abstracted
-- textual representation
data WASMC
  = INSTR !Text
  | SEXP !Text ![WASMC]
  | QUAL !Text WASMC
  | CONCAT ![WASMC]
  | GROUP ![WASMC]
  | PARENS ![WASMC]
  | COMMENT Text
  deriving (Generic, NFData)
instance Semigroup WASMC where
  CONCAT [] <> m = m
  m <> CONCAT [] = m
  m <> n = CONCAT [m, n]
  sconcat (x :| y) = CONCAT (x : y)
instance Monoid WASMC where
  mempty = CONCAT []
  mconcat = CONCAT
instance Eq WASMC where
  (==) = (==) `on` genWASMC' False
instance Ord WASMC where
  compare = compare `on` genWASMC' False
instance IsString WASMC where
  fromString = INSTR . fromString
instance Show WASMC where
  show = T.unpack . genWASMC

-- Convert it into text
genWASMC :: WASMC -> Text
genWASMC = genWASMC' True

genWASMC' :: Bool -> WASMC -> Text
genWASMC' includeComments = PPT.renderStrict . PP.layoutPretty PP.defaultLayoutOptions . PP.sep . go
  where
  _c = PP.pretty
  _t = PP.pretty
  inner = fmap (PP.nest 0) . go
  go :: WASMC -> [PP.Doc ()]
  go (INSTR "") = mempty
  go (INSTR t) = [_t t]
  go (SEXP c items) = go (PARENS (INSTR c : items))
  go (QUAL c item) = go (SEXP c [item])
  go (CONCAT items) = go =<< items
  go (GROUP items) = case go =<< items of
    [] -> mempty
    components -> [PP.group (PP.sep components)]
  go (PARENS c) = [_c '(' <> PP.nest 2 (PP.sep (inner =<< c)) <> _c ')']
  go (COMMENT content) | includeComments = [_t $ "(; " <> content <> " ;)"]
  go (COMMENT _) = []

-- Check if a WASMC is empty
contentless :: WASMC -> Bool
contentless (CONCAT items) = all contentless items
contentless (INSTR t) = t == ""
contentless (COMMENT _) = True
contentless _ = False

instance MonadWriter WASMC WASMM where
  tell = WASM . tell
  listen (WASM act) = do
    (a, w) <- WASM $ listen act
    pure (a, w)
  pass (WASM act) = WASM $ pass act
instance Semigroup m => Semigroup (WASMM m) where (<>) = liftA2 (<>)
instance Monoid m => Monoid (WASMM m) where mempty = pure mempty

sexp :: MonadWriter WASMC m => Text -> [WASMC] -> m ()
sexp c is = tell $ SEXP c is

sexps :: MonadWriter WASMC m => Text -> [[WASMC]] -> m ()
sexps c iss = tell $ CONCAT $ SEXP c <$> iss

sexpOf :: MonadWriter WASMC m => Text -> [m ()] -> m ()
sexpOf c is = sexp c =<< traverse outputOf is

comment :: MonadWriter WASMC m => Text -> m ()
comment = tell . COMMENT

(##) :: WASMC -> Text -> WASMC
instr ## content = instr <> COMMENT content
infixl 0 ##

wasmID :: Name -> WASMC
wasmID (Name name) = INSTR $ "$" <> name

wasmIdx :: Int -> WASMC
wasmIdx idx = INSTR $ _nums !! idx

wasmLen :: Int -> WASMC
wasmLen idx = INSTR $ T.show idx

wasmStr :: Text -> WASMC
wasmStr contents = INSTR $ "\"" <> contents <> "\"" -- FIXME!



class ToExpr code where
  toExpr :: code -> Expr
instance ToExpr Expr where toExpr = id
data SomeExpr = forall code. (Existentiable code, ToExpr code) => SomeExpr code
instance NFData SomeExpr where rnf (SomeExpr c) = rnf c
instance Show SomeExpr where show (SomeExpr c) = show c
instance Eq SomeExpr where
  (SomeExpr c0) == (SomeExpr c2)
    | Just c1 <- Typeable.cast c2 = c0 == c1
    | otherwise = False
instance Ord SomeExpr where
  SomeExpr c0 `compare` SomeExpr c2
    | Just c1 <- Typeable.cast c0 = c1 `compare` c2
    | otherwise = Typeable.typeOf c0 `compare` Typeable.typeOf c2
instance ToExpr SomeExpr where
  toExpr (SomeExpr c) = toExpr c
instance ToWASM SomeExpr

class ToWASM code where
  toWASM :: code -> WASM
  default toWASM :: ToExpr code => code -> WASM
  toWASM = genExpr . toExpr
instance ToWASM WASMC where toWASM = tell

-- A type for a static token representing some WASM, which does not mind being
-- deduplicated (having its WASMC output copied directly). Includes the
-- WASMC type but not WASM.
--
-- Currently used for function definitions, to enable recursive functions to be
-- generated nicely (by deduplicating their `SomeWASM` handles, not their
-- actual code).
data SomeWASM = forall code. (Existentiable code, ToWASM code) => SomeWASM code
instance NFData SomeWASM where rnf (SomeWASM c) = rnf c
instance Show SomeWASM where show (SomeWASM c) = show c
instance Eq SomeWASM where
  (SomeWASM c0) == (SomeWASM c2)
    | Just c1 <- Typeable.cast c2 = c0 == c1
    | otherwise = False
instance Ord SomeWASM where
  SomeWASM c0 `compare` SomeWASM c2
    | Just c1 <- Typeable.cast c0 = c1 `compare` c2
    | otherwise = Typeable.typeOf c0 `compare` Typeable.typeOf c2
instance ToWASM SomeWASM where
  toWASM (SomeWASM c) = toWASM c

data FuncDef def = FuncDef
  { fdName :: Maybe Name
  , fdInputs :: [WTyp]
  , fdOutputs :: [WTyp]
  , fdLexical :: [Bind]
  , fdDefinition :: def
  }
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)

class ToFuncDef def where
  toFuncDef :: def -> FuncDef WASM

-- Generate the body as WASM
newtype FuncBody def = FuncBody def
  deriving stock (Show, Generic, Data, Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, NFData)
instance ToFuncDef def => ToWASM (FuncBody def) where
  toWASM (FuncBody def) = fdDefinition (toFuncDef def)

-- Call the function
newtype FuncCall def = FuncCall def
  deriving stock (Show, Generic, Data, Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, NFData)


funcDef :: (ToFuncDef def, Existentiable def) => def -> WASMMC
funcDef def = case toFuncDef def of
  FuncDef name inputs outputs lexical _definition ->
    genFunc' name (inputs, outputs) lexical (Right (SomeWASM (FuncBody def)))

funcCall :: (ToFuncDef def, Existentiable def) => def -> WASM
funcCall def = sexp "call" =<< sequence [ funcDef def ]

genFunc :: Maybe Name -> ([WTyp], [WTyp]) -> [Bind] -> WASM -> WASMMC
genFunc name tys lexical =
  genFunc' name tys lexical . Left

-- Generate a WASM function definition. Returns code to invoke it
genFunc' :: Maybe Name -> ([WTyp], [WTyp]) -> [Bind] -> Either WASM SomeWASM -> WASMMC
genFunc' name (inputs, outputs) lexical definition = do
  gennedFunctions <- gets funcs
  let
    doIt = genFuncInner (inputs, outputs) lexical definition
    makeReference idx name' = maybe (wasmIdx idx) wasmID name'
  case definition of
    Right keyful ->
      let
        genKey = ((inputs, outputs), keyful)
      in case isGenned gennedFunctions genKey of
        Just (idx, (name', _, _)) -> pure (makeReference idx name')
        Nothing -> do
          let (genning, idx, fulfill) = reserve gennedFunctions genKey (name, undefined, undefined)
          modify' \modul -> modul { funcs = genning }
          (genT, body, _) <- doIt
          makeReference idx name <$ modify' \modul ->
            modul { funcs = funcs modul & fulfill (name, genT, body) }
    Left _ -> do
      (genT, body, wasDefined) <- doIt
      state \modul ->
        let (g, idx, (name', _, _)) = gen (funcs modul) ((inputs, outputs), wasDefined) (name, genT, body)
        in (makeReference idx name', modul { funcs = g })

genFuncInner :: ([WTyp], [WTyp]) -> [Bind] -> Either WASM SomeWASM -> WASMM (WASMC, WASMC, SomeWASM)
genFuncInner (inputs, outputs) lexical definition = do
  genT <- CONCAT <$>
    (fmap (QUAL "param")  <$> traverse genWTyp inputs) <>
    (fmap (QUAL "result") <$> traverse genWTyp outputs)
  let
    addParam paramT (locals, scope) = do
      paramWasm <- genWTyp paramT
      let (g', _, _) = gen locals ((paramT, paramWasm), maybe 0 length $ Map.lookup paramT scope) Nothing
      pure (g', Map.alter (Just . (Map.size scope :) . fromMaybe []) paramT scope)
    makeLocal (((_, localT), _idx), localName) =
      QUAL "local" $ foldMap wasmID localName <> localT
  parameters <- foldrM addParam (newGenIdx, Map.empty) inputs
  -- Save, clear, and eventually restore the quasi-reader state (local to the function)
  restore <- gets \(WASMS { locals, freshBlock }) ->
    modify' \modul -> modul { locals = locals, freshBlock = freshBlock }
  modify' \modul -> modul { locals = fst parameters, freshBlock = 0 }
  let
    willDefine :: WASM
    willDefine = either id toWASM definition
  -- Extract the code for the body of the function
  code <- outputOf $
    -- Reserve the parameters in scope
    local (\here -> here { scope = snd parameters, lexical = gatherBinders id $ zip (wasmIdx <$> [0..]) lexical }) $
    -- Run the code for the definition
    willDefine
  let
    wasDefined :: SomeWASM
    wasDefined = fromRight (SomeWASM code) definition
  -- Make definitions for the locals that were generated
  functionLocals <- gets \(WASMS { locals }) ->
    foldMap makeLocal (List.drop (List.length $ genned $ fst parameters) (genned locals))
  let body = functionLocals <> code
  -- Restore the quasi-reader state
  restore
  pure (genT, body, wasDefined)

genType :: SynthType styp => HasCallStack => Maybe Name -> styp -> WASMMC
genType name = getWTyp name . wtyp

getWTyp :: HasCallStack => Maybe Name -> WTyp -> WASMMC
getWTyp name (WR _ heapType) = genHTyp name heapType
getWTyp _ _ = error "Not a reference type"

registerType :: Maybe Name -> WASMC -> WASMMC
registerType name code =
  state \modul ->
    let
      (g, idx, (mname, _)) = gen (types modul) code (name, Map.empty)
      info = case mname of
        Just reg | Just alias <- name, alias /= reg ->
          COMMENT (coerce reg <> " as " <> coerce alias)
        Just reg -> COMMENT (coerce reg)
        _ -> mempty
    in (GROUP [ wasmIdx idx, info ], modul { types = g })

genWTyp :: HasCallStack => WTyp -> WASMMC
genWTyp = genWTypWith (genHTyp Nothing)

genWTypWith :: HasCallStack => Applicative m => (HTyp -> m WASMC) -> WTyp -> m WASMC
genWTypWith genHTyp = \case
  WF64  -> pure "f64"
  WF32  -> pure "f32"
  WI64  -> pure "i64"
  WI32  -> pure "i32"
  WI16  -> pure "i16"
  WI8   -> pure "i8"
  WV128 -> pure "v128"
  WR nul heapType -> SEXP "ref" <$> sequenceA
    [ pure if nul == Nul then "null" else mempty
    , genHTyp heapType
    ]

genHTyp :: HasCallStack => Maybe Name -> HTyp -> WASMMC
genHTyp name = genHTypWith (regRTyp name Map.empty)

genHTypWith :: HasCallStack => Applicative m => (RTyp -> m WASMC) -> HTyp -> m WASMC
genHTypWith regRTyp = \case
  HI31 -> pure "i31"
  HCls HAny      -> pure "any"
  HCls HEq       -> pure "eq"
  HCls HStruct   -> pure "struct"
  HCls HArray    -> pure "array"
  HCls HNone     -> pure "none"
  HCls HFunc     -> pure "func"
  HCls HNoFunc   -> pure "nofunc"
  HCls HExtern   -> pure "extern"
  HCls HNoExtern -> pure "noextern"
  HRTyp ty -> regRTyp ty

-- Toposort in 5 lines of code
toposort :: forall k. Ord k => NFData k => Map k (Set k) -> [k]
toposort = join . reverse . go [] where
  go :: [[k]] -> Map k (Set k) -> [[k]]
  go acc entries =
    let (sel, remaining) = Map.partition Set.null entries
    in if Map.null sel then acc else
      go (Map.keys sel : acc) (remaining <&> (Set.\\ Map.keysSet sel))

-- Traverse the various Haskell types of WASM types
data TypTrav m = TypTrav
  { overWTyp :: WTyp -> m WTyp
  , overCTyp :: CTyp -> m CTyp
  , overRTyp :: RTyp -> m RTyp
  , overHTyp :: HTyp -> m HTyp
  , overSubTyp :: SubTyp -> m SubTyp
  }

-- Handle `RRef name` with `RTyp name group`
promoteFromGroup :: forall m. Applicative m => Map Name SubTyp -> TypTrav m
promoteFromGroup group = promo where
  promo = TypTrav
    { overWTyp = \case
        WR nul ht ->
          WR nul <$> overHTyp promo ht
        wt -> pure wt
    , overCTyp = \case
        WS wts -> WS <$> traverse (traverse (overWTyp promo)) wts
        WA wt -> WA <$> traverse (overWTyp promo) wt
        WF is os -> WF
          <$> traverse (overWTyp promo) is
          <*> traverse (overWTyp promo) os
    , overRTyp = \case
        RRef name -> pure $ RTyp name group
        c@(RTyp _ _) -> pure c
        CTyp ct -> CTyp <$> overCTyp promo ct
    , overHTyp = \case
        HRTyp rt -> HRTyp <$> overRTyp promo rt
        ht -> pure ht
    , overSubTyp = \(SubTyp f b i) ->
        SubTyp f
        <$> traverse (overRTyp promo) b
        <*> overCTyp promo i
    }

-- We need to preload every external type, so we can allocate the recursive type
-- group without anything shifting around, and find the other types that we need
-- to include in the recursive group (e.g. intermediate structs and such that
-- reference once or more types from the recursive group).
preloadInGroup :: Map Name SubTyp -> RTyp -> WASMM (Maybe (Set RTyp))
preloadInGroup group =
  \case
    RRef _ -> pure $ Just Set.empty
    RTyp _ each
      | each == group -> pure $ Just Set.empty
      | otherwise -> error "Mutual recursion?"
    CTyp t -> preCTyp t
  where
  preMut :: Mut WTyp -> WASMM (Maybe (Set RTyp))
  preMut (Mut t) = preWTyp t
  preMut (Imm t) = preWTyp t
  preRTyp :: RTyp -> WASMM (Maybe (Set RTyp))
  preRTyp = \case
    RRef _ -> pure $ Just Set.empty
    RTyp _ each
      | each == group -> pure $ Just Set.empty
      | otherwise -> error "Mutual recursion?"
    CTyp t -> do
      -- If this type had a recursive reference,
      -- include it in the recursive group itself
      accumulated <- preCTyp t
      case accumulated of
        Nothing -> Nothing <$ regRTyp Nothing Map.empty (CTyp t)
        Just acc -> pure $ Just $ Set.insert (CTyp t) acc
  preCTyp :: CTyp -> WASMM (Maybe (Set RTyp))
  preCTyp = \case
    WS ts -> foldMap preMut ts
    WA t -> preMut t
    WF i o -> foldMap preWTyp (i <> o)
  preWTyp :: WTyp -> WASMM (Maybe (Set RTyp))
  preWTyp = \case
    WR _ t -> preHTyp t
    _ -> mempty
  preHTyp :: HTyp -> WASMM (Maybe (Set RTyp))
  preHTyp = \case
    HRTyp ty -> preRTyp ty
    _ -> mempty

-- After preloading and allocating the indices for each type in the group, we can
-- materialize a type directly to WASMC now, with no side effects.
materializeInGroup :: HasCallStack => Map WASMC Int -> Map Name SubTyp -> Map RTyp Int -> SubTyp -> WASMC
materializeInGroup previousTypes original allocated =
  force . matSubTyp
  where
  matSubTyp = runIdentity . genSubTypWith
    (pure . wasmIdx . getRTyp) (pure . matCTyp)
  matCTyp :: CTyp -> WASMC
  matCTyp = runIdentity . genCTypWith (pure . matWTyp)
  matWTyp :: WTyp -> WASMC
  matWTyp = runIdentity . genWTypWith (pure . matHTyp)
  matHTyp :: HTyp -> WASMC
  matHTyp = runIdentity . genHTypWith (pure . wasmIdx . getRTyp)
  getRTyp :: RTyp -> Int
  -- Either the RTyp is allocated
  getRTyp (RRef name) = allocated ! RRef name
  getRTyp (RTyp name each) | each == original =
    allocated ! RRef name
  getRTyp t | Just r <- allocated Map.!? t = r
  -- Or it is already generated
  -- (and does not reference this rec group)
  getRTyp t = previousTypes ! matSubTyp
    case t of
      RTyp name each -> each ! name
      CTyp c -> plainTyp c

-- Promote a composite type to a subtype, which is what gets registered
plainTyp :: CTyp -> SubTyp
plainTyp = SubTyp False Nothing

admix :: Map Name SubTyp -> Set RTyp -> Map RTyp SubTyp
admix orig added = force $
  Map.mapKeys RRef orig `Map.union`
  Map.fromSet awa added
  where
  awa :: RTyp -> SubTyp
  awa (CTyp t) = plainTyp t
  awa (RRef name) = orig ! name
  awa (RTyp name group) = group ! name

-- A group needs to be initialized in order of subtyping. The order does not
-- matter for references, though: any type can reference any other type in the
-- group. But subtyping needs to be a DAG, which is enforced by source order.
sortGroup :: Map Name SubTyp -> Map RTyp SubTyp -> [(RTyp, SubTyp)]
sortGroup group wholeGroup =
  fmap (\k -> (k, wholeGroup ! k)) $
    toposort $ foldMap dep . stExtends <$> wholeGroup
  where
  promote = runIdentity . overCTyp (promoteFromGroup group)
  byTyp :: Map RTyp (Maybe Name)
  byTyp = Map.fromList $ Map.toList wholeGroup <&> \case
    -- FIXME: is CTyp Promote the right thing?
    (RRef name, sub) -> (CTyp $ promote $ stIs sub, Just name)
    (rt, _) -> (rt, Nothing)
  dep :: RTyp -> Set RTyp
  dep (RRef name) = Set.singleton (RRef name)
  dep (RTyp name each) | each == group = Set.singleton (RRef name)
  dep t = case Map.lookup t byTyp of
    Nothing -> Set.empty
    Just Nothing -> Set.singleton t
    Just (Just name) -> Set.singleton (RRef name)

-- Register a recursive type group at once, returning the map of each type
-- to its WASM index.
regTypeGroup :: HasCallStack => Map Name SubTyp -> WASMM (Map Name WASMC)
regTypeGroup group = fastSlow
  do gets $ fmap snd . flip isGenned group . typeGroups
  do
    let promote = runIdentity . overCTyp (promoteFromGroup group)
    added <- group & foldMap
      (foldMap (foldMap (preloadInGroup group) .) [ stExtends, Just . CTyp . stIs ])
    let mixed = admix group (fromMaybe Set.empty added)
    let sorted = sortGroup group mixed
    GenIdx previousTypes _ <- gets types
    traversed <- state \modul ->
      let
        (g, traversed, _coll) = reserveNForSure
          (types modul) sorted
          (Map.fromList . fmap \((key, _), idx) -> (key, idx))
          \(desc, sub) _here collected ->
            let
              codeForSubTyp = materializeInGroup (fst <$> previousTypes) group collected
              name = case desc of
                RRef n -> Just n
                _ -> Nothing
            in (codeForSubTyp sub, (name, group), \_ -> ())
      in (traversed, modul { types = g })
    pure $ Map.fromList $ traversed & mapMaybe \case
      ((v, (_, (Just name, _))), ()) -> Just (name, v)
      _ -> Nothing

-- Register a subtype on its own, returning its registered WASM index
regSubTyp :: Maybe Name -> Map Name SubTyp -> SubTyp -> WASMMC
regSubTyp name group = registerType name <=< genSubTyp group

-- Generate the WASM code describing a subtype
genSubTyp :: Map Name SubTyp -> SubTyp -> WASMMC
genSubTyp group = genSubTypWith (regRTyp Nothing group) (genCTypWith genWTyp)

genSubTypWith :: forall m. Applicative m => (RTyp -> m WASMC) -> (CTyp -> m WASMC) -> SubTyp -> m WASMC
genSubTypWith regRTyp genCTyp (SubTyp final below inner) =
  SEXP "sub" <$> sequenceA
    [ pure if final then "final" else mempty
    , below & maybe (pure mempty) regRTyp
    , genCTyp inner
    ]

-- Register a recursive type, returning its registered WASM index
regRTyp :: Maybe Name -> Map Name SubTyp -> RTyp -> WASMMC
regRTyp name group = \case
  CTyp t -> regSubTyp name group $ plainTyp t
  RTyp which group' -> regSubTyp (prefer which) group' (group' ! which)
  RRef which -> regSubTyp (prefer which) group (group ! which)
  where
  prefer (Name (T.unpack -> '_' : _)) = name
  prefer which = Just which

-- Register a composite type, via plainTyp
regCTyp :: Maybe Name -> CTyp -> WASMMC
regCTyp name = regSubTyp name Map.empty . plainTyp

-- Generate the WASM code describing a composite type
genCTyp :: CTyp -> WASMM WASMC
genCTyp = genCTypWith genWTyp

genCTypWith :: forall m. Applicative m => (WTyp -> m WASMC) -> CTyp -> m WASMC
genCTypWith genWTyp = \case
  WS fields -> SEXP "struct" <$> traverse genField fields
  WA itemT -> SEXP "array" <$> traverse genMut [itemT]
  WF inputs outputs -> SEXP "func" <$> liftA2 (<>)
    (fmap (QUAL "param")  <$> traverse genWTyp inputs)
    (fmap (QUAL "result") <$> traverse genWTyp outputs)
  where
  genField ty = QUAL "field" <$> genMut ty
  genMut (Mut ty) = QUAL "mut" <$> genWTyp ty
  genMut (Imm ty) = genWTyp ty

-- Generate a whole WASM module
genModule :: WASM -> WASMM WASMC
genModule codeForMain = do
  _ <- genFunc (Just "fd_write") ([], []) [] mempty
  _ <- genFunc (Just "print") ([wtyp TTxt], [WI32]) ["text"] do
    withLexVar (Just "idx") SU32 \(eidx, vidx) -> do
      let
        getIdx = sexp "local.get" [ vidx ]
        getLen = genExpr $ ELength (EVar STxt "text")
      sexp "local.set" [ vidx, genConst @Word32 0 ]
      flow Nothing mempty \(continue, break) -> do
        getIdx
        getLen
        tell "i32.ge_u"
        sexp "br_if" [ break ]

        getIdx
        genExpr $ EIndex mempty (EVar STxt "text") eidx
        tell "i32.store"

        getIdx
        sexp "i32.add" [ genConst @Word32 1 ]
        sexp "local.set" [ vidx ]

        sexp "br" [ continue ]
      sexp "i32.store" [ genConst @Word32 65500, genConst @Word32 0 ]
      sexp "i32.store" [ genConst @Word32 65504, SEXP "local.get" [ vidx ] ]
      sexp "call" [ wasmID "fd_write", CONCAT $ genConst @Word32 <$> [ 1, 65500, 1, 65508 ] ]
      tell "drop"
      sexp "local.get" [ vidx ]
  _ <- genFunc (Just "main") ([], []) [] codeForMain
  hasStart <- gets $ not . contentless . initCode
  when hasStart do
    void $ genFunc (Just "_start") ([], []) [] . tell =<< gets initCode
  gets \(WASMS { funcs, types, locals = _, globals, initCode = _, rwdata, rodata }) ->
    SEXP "module" $ join
      [ [ SEXP "start" [ wasmID "_start" ] | hasStart ]
      , [ SEXP "import"
          [ wasmStr "wasi_snapshot_preview1", wasmStr "fd_write"
          , SEXP "func" [ wasmID "fd_write", SEXP "param" [ "i32", "i32", "i32", "i32" ], SEXP "result" [ "i32" ] ]
          ]
        , SEXP "memory" [ wasmLen 1 ]
        , SEXP "export" [ wasmStr "memory", SEXP "memory" [ wasmIdx 0 ] ]
        ]
      , Map.toList globals <&> \(name, ((_, ty), code)) ->
          SEXP "global" [ wasmID name, ty, code ]
      , let
          maybeRec f xs@(x :| _) | Map.null (snd (snd x)) = foldMap f xs
          maybeRec f xs =  SEXP "rec" $ f <$> toList xs
        in NEL.groupWith (snd.snd) (genned types) <&> maybeRec
          \(code, (name, _)) ->
            SEXP "type" [ foldMap wasmID name, code ]
      , genned funcs <&> \(_, (name, ty, code)) ->
          if name == Just "fd_write" then mempty else
          SEXP "func" [ foldMap wasmID name, CONCAT [ SEXP "export" [ wasmStr "main" ] | name == Just "main" ], ty, code ]
      , [ genData ".data" rwdata ]
      , [ genData ".rodata" rodata ]
      ]
  where
  genData _ (0, _) = mempty
  genData name (_, dataSegment) = SEXP "data"
    [ wasmID name, printData dataSegment ]
  printData :: ByteString.Builder.Builder -> WASMC
  printData d = INSTR . Builder.toText . quotes $
    ByteString.Lazy.unpack (ByteString.Builder.toLazyByteString d)
      & foldMap \byte ->
        let
          ch = chr (fromIntegral byte)
          digit = ((['0'..'9'] <> ['A'..'F']) !!) . fromIntegral
        in if isAscii ch && isPrint ch
          then Builder.fromChar ch
          else Builder.fromChar '\\'
            <> Builder.fromChar (digit ((byte .>>. 0x4) .&. 0xF))
            <> Builder.fromChar (digit (byte .&. 0xF))
  quotes contents = Builder.fromChar '"' <> contents <> Builder.fromChar '"'

-- Generate a WASM module into text
genWAT :: WASM -> IO Text
genWAT codeForMain = runWASM (genModule codeForMain)
  <&> \(moduleCode, _, _) -> genWASMC moduleCode

-- Run the WASM monad
runWASM :: WASMM a -> IO (a, WASMS, WASMW)
runWASM (WASM runIt) = runRWST runIt
  WASMR
  { scope = Map.empty
  , lexical = Map.empty
  }
  WASMS
  { funcs = newGenIdx
  , types = newGenIdx
  , typeGroups = newGenIdx
  , locals = newGenIdx
  , freshBlock = 0
  , globals = Map.empty
  , initCode = mempty
  , rwdata = (0, mempty)
  , rodata = (0, mempty)
  , binlits = Map.empty
  }
-- Locally run WASM
captureWASM :: WASMM (WASMM r -> IO r)
captureWASM = do
  r <- ask
  s <- gets id
  let
    runner :: forall r. WASMM r -> IO r
    runner (WASM runIt) =
      runRWST runIt r s
        <&> \(a, _, _) -> a
  pure runner


-- | Get a variable that is valid for the duration of the continuation.
withVar :: SynthType styp => Maybe Name -> styp -> (WASMC -> WASMM r) -> WASMM r
withVar name (wtyp -> ty) cont = do
  here@(WASMR { scope }) <- ask
  let tyidx = maybe 0 length $ Map.lookup ty scope
  tyWasm <- genWTyp ty
  (scope', var) <- state \modul ->
    let (g, idx, _) = gen (locals modul) ((ty, tyWasm), tyidx) name
        scope' = Map.alter (Just . (idx :) . fromMaybe []) ty scope
    in ((scope', maybe (wasmIdx idx) wasmID name), modul { locals = g })
  local (const $ here { scope = scope' }) $ cont var
-- | This variable is visible to expressions.
withLexVar :: SynthType styp => Maybe Name -> styp -> ((Expr, WASMC) -> WASMM r) -> WASMM r
withLexVar name styp cont = do
  here@(WASMR { scope, lexical }) <- ask
  let ty = wtyp styp
  let tyidx = maybe 0 length $ Map.lookup ty scope
  tyWasm <- genWTyp ty
  (scope', lexical', var) <- state \modul ->
    let (g, idx, _) = gen (locals modul) ((ty, tyWasm), tyidx) name
        scope' = Map.alter (Just . (idx :) . fromMaybe []) ty scope
        lexical' = name & maybe lexical \n ->
          Map.insert n (wasmIdx idx) lexical
    in ((scope', lexical', (maybe undefined (EVar (S styp)) name, maybe (wasmIdx idx) wasmID name)), modul { locals = g })
  local (const $ here { scope = scope', lexical = lexical' }) $ cont var


-- Generate the type for a block (parameters and results). The difference
-- between parameters and results describes the change in the stack. Everything
-- not described in parameters remains on the stack, it just is not visible to
-- the code in the block.
mkBlockType :: ([STyp], [STyp]) -> WASMMC
mkBlockType (inputs, outputs) =
  (quals "param"  <$> traverse (genWTyp . wtyp) inputs) <>
  (quals "result" <$> traverse (genWTyp . wtyp) outputs)
  where
  quals _ [] = mempty
  quals name contents = SEXP name contents

-- Get the next block ID
incrementBlock :: WASMMC
incrementBlock = do
  i <- state \modul ->
    (freshBlock modul, modul { freshBlock = freshBlock modul + 1 })
  pure $ wasmID $ _intNames !! i


data WithFallback f t = f t :?? t
  deriving (Functor, Foldable, Traversable, Eq, Ord)

wrapBlock :: Text -> [WASMC] -> WASMM a -> WASMM a
wrapBlock instr args = censor $
  SEXP instr . \code -> args <> [code]

-- Control flow: provide backwards and forwards branching labels at once
-- (loop and block, respectively).
flow :: Maybe Name -> ([STyp], [STyp]) -> ((WASMC, WASMC) -> WASMM r) -> WASMM r
flow _name inputsOutputs cont = do
  blockType <- mkBlockType inputsOutputs
  idxI <- incrementBlock
  wrapBlock "loop" [ idxI, blockType ] do
    idxO <- incrementBlock
    wrapBlock "block" [ idxO, blockType ] do
      cont (idxI, idxO)

-- Generate a branch table.
brTable ::
  Traversable f => Foldable g =>
  ([STyp], [STyp]) ->
  -- Branches
  f branch ->
  -- Intro
  (forall label. f (label, branch) -> WASMM (WithFallback g (Maybe label))) ->
  -- Each branch
  (branch -> WASMM (Bool, r)) ->
  WASMM (f r)
brTable inputsOutputs requested intro createBranch = do
  blockType <- mkBlockType inputsOutputs
  flow Nothing inputsOutputs \(_, break) -> do
    branches <- forwards $ for requested \branchData -> Backwards do
      label <- incrementBlock
      pure (label, branchData)
    forwards $ for_ branches \(label, _) -> Backwards do
      tell $ GROUP [ "block", label, blockType ]
    labelAssignments <- intro branches
    sexp "br_table" $ fromMaybe break <$> toList labelAssignments
    for branches \(label, branchData) -> do
      tell $ GROUP [ "end", label ]
      (shouldFallthrough, result) <- createBranch branchData
      unless shouldFallthrough do
        sexp "br" [ break ]
      pure result

-- Generate a series of branches for a case block.
brSequence ::
  Traversable f =>
  ([STyp], [STyp]) ->
  f ((WASMC, WASMC) -> WASMM (Bool, r)) ->
  WASMM r -> -- fallback
  WASMM (WithFallback f r)
brSequence inputsOutputs cases fallback = do
  blockType <- mkBlockType inputsOutputs
  break <- incrementBlock
  wrapBlock "block" [ break, blockType ] do
    brSequenceInner inputsOutputs cases fallback break

brSequenceInner ::
  Traversable f =>
  ([STyp], [STyp]) ->
  f ((WASMC, WASMC) -> WASMM (Bool, r)) ->
  WASMM r -> -- fallback
  WASMC -> -- break label
  WASMM (WithFallback f r)
brSequenceInner inputsOutputs cases fallback break = do
  blockType <- mkBlockType inputsOutputs
  branches <- for cases \branchData -> do
    label <- incrementBlock
    pure (label, branchData)
  forwards $ for_ branches \(label, _) -> Backwards do
    tell $ GROUP [ "block", label, blockType ]
  main <- for branches \(label, branchData) -> do
    (shouldFallthrough, result) <- branchData (label, break)
    unless shouldFallthrough do
      sexp "br" [ break ]
    tell $ GROUP [ "end", label ]
    pure result
  (main :??) <$> fallback

-- Generate a series of casts. The source type is the last of the parameters
-- stack and must be a ref, along with the type for each branch.
brCasts ::
  Traversable f =>
  ([STyp], [STyp]) ->
  f (WASMM (WTyp {- must be a ref -}, r)) ->
  WASMM r ->
  WASMM (WithFallback f r)
brCasts inputsOutputs cases fallback = do
  input <- genWTyp $ wtyp $ List.last (fst inputsOutputs)
  blockType <- mkBlockType inputsOutputs
  break <- incrementBlock
  wrapBlock "block" [ break, blockType ] do
    brSequenceInner (inputsOutputs <> ([], [List.last (fst inputsOutputs)]))
      ( cases <&> \item (continue, _) ->
          retroactively do
            (ty, r) <- item
            ty' <- genWTyp ty
            pure ((False, r), SEXP "br_on_cast_fail" [ continue, input, ty' ])
      ) fallback break


data ExactUsage usage = NoUsage | NoHint | ExactUsage usage
  deriving (Eq, Ord, Generic, NFData)

instance Eq usage => Monoid (ExactUsage usage) where
  mempty :: Eq usage => ExactUsage usage
  mempty = NoUsage
instance Eq usage => Semigroup (ExactUsage usage) where
  NoUsage <> u = u
  u <> NoUsage = u
  ExactUsage u1 <> ExactUsage u2 | u1 == u2 = ExactUsage u1
  _ <> _ = NoHint

-- Nice properties for existentially hidden data
type Existentiable hidden = (Typeable hidden, NFData hidden, Ord hidden, Show hidden)

type Usable usage = (Existentiable usage, Monoid usage)

-- A synthetic type that specifies exactly how it is codegenned.
class (Existentiable spec, Usable (UsageHint spec)) => SynthType spec where
  type family UsageHint spec :: Type
  type instance UsageHint spec = ()

  -- The raw WASM type it compiles to
  wtyp :: spec -> WTyp

  -- How to compile it, if it is a data type
  isDataType :: spec -> Maybe (Map Name [(Name, STyp)])
  isDataType _ = Nothing
  mkconstr :: spec -> Map Name (Map Name (Expr, WASM) -> WASM)
  mkconstr _ = error "Not a constructor"
  unconstr :: spec -> [WTyp] -> (Map Name (Map Name WASM -> WASM), WASM {- fallback -}) -> WASM
  unconstr _ = error "Not a constructor"

  -- How to compile it, if it is an array-like type
  isArrayType :: spec -> Maybe TArr
  isArrayType _ = Nothing
  arraylit :: spec -> [(Expr, WASM)] -> WASM
  arraylit _ = error "Not an array"
  arrayfill :: spec -> (Expr, WASM) -> (Expr, WASM) -> WASM
  arrayfill _ = error "Not an array"
  arraylength :: spec -> (Expr, WASM) -> WASM
  arraylength _ = error "Not an array"
  arrayindex :: spec -> (Expr, WASM) -> (Expr, WASM) -> WASM
  arrayindex _ = error "Not an array"

data SomeUsageHint
  = forall spec. SynthType spec => SomeUsageHint !(Proxy spec) !(UsageHint spec)
  | NoneHint
  | NoUsageAtAll

instance Eq SomeUsageHint where
  (SomeUsageHint _ u0) == (SomeUsageHint _ u2)
    | Just u1 <- Typeable.cast u2 = (u0 == u1)
    | otherwise = False
  NoneHint == NoneHint = True
  NoUsageAtAll == NoUsageAtAll = True
  _ == _ = False
instance Ord SomeUsageHint where
  SomeUsageHint _ u0 `compare` SomeUsageHint _ u2
    | Just u1 <- Typeable.cast u0 = u1 `compare` u2
    | otherwise = Typeable.typeOf u0 `compare` Typeable.typeOf u2
  NoneHint `compare` NoneHint = EQ
  NoUsageAtAll `compare` NoUsageAtAll = EQ

  SomeUsageHint _ _ `compare` _ = LT
  _ `compare` SomeUsageHint _ _ = GT
  NoneHint `compare` NoUsageAtAll = LT
  NoUsageAtAll `compare` NoneHint = GT

instance Show SomeUsageHint where
  show (SomeUsageHint _ u) = show u
  show NoneHint = "NoneHint"
  show NoUsageAtAll = "NoUsageAtAll"

instance Monoid SomeUsageHint where
  mempty = NoUsageAtAll
instance Semigroup SomeUsageHint where
  NoUsageAtAll <> u = u
  u <> NoUsageAtAll = u
  (SomeUsageHint p u0) <> (SomeUsageHint _ u2)
    | Just u1 <- Typeable.cast u2 = SomeUsageHint p (u0 <> u1)
  _ <> _ = NoneHint

instance NFData SomeUsageHint where
  rnf = \case
    NoneHint -> ()
    NoUsageAtAll -> ()
    SomeUsageHint !_p u -> rnf u

instance SynthType STyp where
  type instance UsageHint STyp = SomeUsageHint
  wtyp (STyp spec) = wtyp spec
  isDataType (STyp spec) = isDataType spec
  mkconstr (STyp spec) = mkconstr spec
  unconstr (STyp spec) = unconstr spec
  isArrayType (STyp spec) = isArrayType spec
  arraylit (STyp spec) = arraylit spec
  arrayfill (STyp spec) = arrayfill spec
  arraylength (STyp spec) = arraylength spec
  arrayindex (STyp spec) = arrayindex spec

applySome :: Word -> TFun -> TFun
applySome (I arity) (TFun inputs output) | arity <= length inputs =
  TFun (drop arity inputs) output
applySome _ _ = error "Arity too large"


--------------------------------------------------------------------------------
-- A declarative model for WASM types, so that they can be created from anywhere
-- and inserted into the module on demand, with appropriate references.
--
-- The main difficulty is recursive type groups and subtypes.
--------------------------------------------------------------------------------


data Mut t = Mut !t {- mutable -} | Imm !t {- immutable -}
  deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)
data Sgn = Sgn {- signed -} | Uns {- unsigned -}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
data Nul = Nul {- nullable -} | Non {- nonnullable -}
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
-- Basic WASM types that live on the stack, plus packed types that are unpacked
-- into i32 on the stack.
data WTyp
  = WF64 | WF32   -- Floats f64/f32
  | WI64 | WI32   -- Integers i64/i32
  | WI16 | WI8    -- Packed integers for structs/arrays
  | WV128         -- Vector register v128
  | WR !Nul !HTyp -- Reference to a garbage-collected heap type
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
-- Heap types, the target of references. This includes composite types
-- (structs, arrays, and functions) and their recursive and subtyped forms, plus
-- the means to classify them (any struct, any array, any function, etc.), and
-- the i31 type which is left for implementations to use as an immediate, not
-- actually heap allocated.
data HTyp -- Heap types
  = HCls !HCls -- Abstract classified reference types
  | HRTyp !RTyp -- Composite types
  | HI31 -- Special i31
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
pattern HCTyp :: CTyp -> HTyp
pattern HCTyp t = HRTyp (CTyp t)
-- Heap type classifiers
data HCls = HAny | HEq | HStruct | HArray | HNone | HFunc | HNoFunc | HExtern | HNoExtern
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
-- Composite types (for type declarations)
data CTyp
  = WS ![Mut WTyp] -- Struct, with individual field types
  | WA !(Mut WTyp) -- Array, with its item type
  | WF ![WTyp] ![WTyp] -- Function, with input and output types
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
-- Subtypes can be declared final and declared to extend another nominal type
-- (that must be structurally compatible with the type definition).
data SubTyp = SubTyp
  { stFinal :: !Bool
  , stExtends :: !(Maybe RTyp)
  , stIs :: !CTyp
  }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)
-- A potentially-recursive type can be an ordinary composite type (technically
-- redundant but really nice to have), or a particular case chosen out of a
-- recursive group `Map Name SubTyp`, always kept together. Within the subtypes
-- comprising this recursive group, `RRef` is valid to refer to that particular
-- group: no nesting is possible, and it should not be necessary. If mutual
-- recursion is needed, it should be aggregated into a larger group.
--
-- This enables a simple representation of recursive type groups without
-- imposing any kind of global coherence requirements, just local coherence
-- of names within a single group.
data RTyp
  = RTyp !Name !(Map Name SubTyp) -- Recursive types and subtypes
  | RRef !Name -- Recursive references, only valid within a recursive group
  | CTyp !CTyp -- Plain composite groups
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)


-- The specification for HCls with regard to HTyp
-- inside a recursive type group
matches :: Map Name SubTyp -> HCls -> HTyp -> Bool
matches _ HAny = const True
matches _ HNone = const False
matches g HEq = \case
  HRTyp (RTyp i ts) -> matches ts HEq (HCTyp $ stIs $ ts ! i)
  HRTyp (RRef i) -> matches g HEq (HCTyp $ stIs $ g ! i)
  HRTyp (CTyp (WF _ _)) -> False
  HRTyp (CTyp _) -> True
  HCls HEq -> True
  HCls HStruct -> True
  HCls HArray -> True
  HCls _ -> False
  HI31 -> True
matches g HStruct = \case
  HRTyp (RTyp i ts) -> matches ts HStruct (HCTyp $ stIs $ ts ! i)
  HRTyp (RRef i) -> matches g HStruct (HCTyp $ stIs $ g ! i)
  HCls HStruct -> True
  HRTyp (CTyp (WS _)) -> True
  _ -> False
matches g HArray = \case
  HRTyp (RTyp i ts) -> matches ts HArray (HCTyp $ stIs $ ts ! i)
  HRTyp (RRef i) -> matches g HArray (HCTyp $ stIs $ g ! i)
  HCls HArray -> True
  HRTyp (CTyp (WA _)) -> True
  _ -> False
matches g HFunc = \case
  HRTyp (RTyp i ts) -> matches ts HFunc (HCTyp $ stIs $ ts ! i)
  HRTyp (RRef i) -> matches g HFunc (HCTyp $ stIs $ g ! i)
  HCls HFunc -> True
  HRTyp (CTyp (WF _ _)) -> True
  _ -> False
matches g HNoFunc = not . matches g HFunc
matches g HExtern = \case
  HRTyp (RTyp i ts) -> matches ts HExtern (HCTyp $ stIs $ ts ! i)
  HRTyp (RRef i) -> matches g HExtern (HCTyp $ stIs $ g ! i)
  HCls HExtern -> True
  _ -> False
matches g HNoExtern = not . matches g HExtern

-- The unpacking type: extend i8, i16 -> i32
unpack :: WTyp -> WTyp
unpack WI8 = WI32
unpack WI16 = WI32
unpack t = t

-- The STyp for primitive integer types, including sign information
pattern SF64, SF32, SU64, SU32, SS64, SS32, SU31, SS31 :: STyp
pattern SF64 = S (TPrm Sgn WF64)
pattern SF32 = S (TPrm Sgn WF32)
pattern SU64 = S (TPrm Uns WI64)
pattern SU32 = S (TPrm Uns WI32)
pattern SS64 = S (TPrm Sgn WI64)
pattern SS32 = S (TPrm Sgn WI32)
pattern SU31 = S (TPrm Uns (WR Non HI31))
pattern SS31 = S (TPrm Sgn (WR Non HI31))

-- Unroll TDat, giving `STyp` instead of `Recursive () STyp`
unrollTDat :: TDat -> Map Name [(Name, STyp)]
unrollTDat t@(TDat constructors) =
  constructors <&> fmap (fmap (recurses (const (S t))))

-- Unroll TMutDat, giving `STyp` instead of `Recursive Name STyp`
unrollTMutDat :: TMutDat -> Map Name [(Name, STyp)]
unrollTMutDat (TMutDat which group) =
  group ! which <&> fmap (fmap (recurses (\name -> SMutDat name group)))

instance SynthType TDat where
  -- Codegenned as a recursive group with a _supertype struct which the
  -- individual constructors subtype
  wtyp (unrollTDat -> constructors) = WR Non $ HRTyp $ RTyp "_supertype" $
    Map.insert "_supertype" (plainTyp $ WS []) $
      constructors <&> plainTyp . WS . fmap (Mut . wtyp . snd)
  isDataType = Just . unrollTDat
instance SynthType TMutDat where
  -- Codegenned as a recursive group where each type gets its own section for
  -- its supertype and constructors, ty._self and ty.con1, ty.con2, etc.
  wtyp (TMutDat which group) = WR Non $ HRTyp $ RTyp (suffix which "_supertype") $
    group & Map.foldMapWithKey \ty constructors ->
      Map.insert (suffix ty "_supertype") (plainTyp $ WS []) $
        Map.fromList $ Map.toList constructors <&>
          \(k, fields) -> (suffix ty k,) $ plainTyp $ WS $
            fields <&> Mut . wtyp . recurses (\other -> SMutDat other group) . snd
    where
    suffix (Name name) (Name suffix) = Name (name <> "." <> suffix)
  isDataType = Just . unrollTMutDat
instance SynthType TFun where
  wtyp (TFun inputs output) = WR Non $ HCTyp $ WF (wtyp <$> inputs) [wtyp output]

{-# NOINLINE baseClosureC #-}
baseClosureC :: CTyp
baseClosureC = WS
  -- boxed partial application
  [ Imm $ WR Non $ HCTyp $ WF [WR Nul (HCls HAny)] [WR Nul (HCls HAny)]
  -- unboxed partial application
  , Imm $ WR Nul (HCls HFunc)
  -- closure data
  , Imm $ WR Nul (HCls HAny)
  ]

{-# NOINLINE baseClosureH #-}
baseClosureH :: RTyp
baseClosureH = RTyp "closure" $ Map.singleton "closure" $
  SubTyp False Nothing baseClosureC

instance SynthType TClo where
  wtyp (TClo Nothing Nothing) = WR Non $ HRTyp baseClosureH
  wtyp (TClo clo fun) = WR Non $ HRTyp $ RTyp "_someclosure" $ Map.singleton "_someclosure" $
    SubTyp False (Just baseClosureH) $ WS
      [ Imm $ WR Non $ HCTyp $ WF [WR Nul (HCls HAny)] [WR Nul (HCls HAny)]
      , Imm $ WR Nul $ fromMaybe (HCls HFunc) fun
      , Imm $ WR Nul $ fromMaybe (HCls HAny) clo
      ]
instance SynthType TArr where
  wtyp (TArr t) = WR Non $ HCTyp $ WA (Mut (wtyp t))
  isArrayType = Just
  arraylit t items = do
    ty <- genType Nothing t
    comment "create array literal:"
    traverse_ snd items
    sexp "array.new_fixed" [ ty, wasmLen $ length items ]
  arrayfill t (_, len) (_, getter) = do
    ty <- genType Nothing t
    comment "create array:"
    withVar (Just "len") SU32 \vlen -> do
      len
      sexp "local.tee" [ vlen ## "array length" ]
      withVar (Just "array") t \varr -> do
        sexp "array.new_default" [ ty ## T.show ty ]
        sexp "local.set" [ varr ## "array" ]
        withVar (Just "idx") SU32 \vidx -> do
          genExpr $ EU32 0
          sexp "local.set" [ vidx ## "index" ]
          comment "fill array:"
          flow (Just "initialize_array") ([] :: [STyp], [] :: [STyp]) \(continue, break) -> do
            sexp "br_if"
              [ break
              , SEXP "local.get" [ vidx ]
              , SEXP "local.get" [ vlen ]
              , SEXP "i32.ge_u" []
              ]

            sexpOf "array.set"
              [ tell ty
              , sexp "local.get" [ varr ]
              , sexp "local.get" [ vidx ]
              , sexp "local.get" [ vidx ]
                *> getter
              ]

            sexp "local.get" [ vidx ]
            sexp "i32.add" [ genConst (1 :: Word32) ]
            sexp "local.set" [ vidx ]
            sexp "br" [ continue ]
        sexp "local.get" [ varr ]
  arraylength _t (_, expr) = do
    expr
    tell "array.len"
  arrayindex t (_, arr) (_, idx) = do
    ty <- genType Nothing t
    arr
    idx
    sexp "array.get" [ ty ] -- TODO: packed
instance SynthType TPrm where
  wtyp (TPrm _ t) = t
instance SynthType TTxt where
  wtyp TTxt = WR Non $ HCTyp $ WA (Mut WI8)
  isArrayType TTxt = Just (TArr (S (TPrm Uns WI8)))
  arraylength TTxt (_, expr) = do
    expr
    tell "array.len"
  arrayindex TTxt (_, arr) (_, idx) = do
    ty <- genType Nothing TTxt
    arr
    idx
    sexp "array.get_u" [ ty ]


-- An enum synthetic type, represented as an i31 for tags
newtype TEnum = TEnum [Name]
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

-- Special case: void type, no constructors
pattern TVoid :: TEnum
pattern TVoid = TEnum []

instance SynthType TEnum where
  wtyp (TEnum _) = wtyp SU31
  isDataType (TEnum names) = Just $ Map.fromList $ names <&> (, [])
  mkconstr (TEnum names) = Map.fromList $ zip [0..] names <&>
    \(tag, name) -> (name,) \_fields -> do
      sexp "i32.const" [ wasmIdx tag ## coerce name ]
      tell "ref.i31"
  unconstr self@(TEnum names) o (cases, fallback) = do
    tell "i31.get_u"
    void $ brTable ([S self], S . TPrm Uns <$> o)
      do (cases <@> Map.empty) :?? fallback
      do pure . \(labels :?? oops) -> fmap fst <$> (names <&> flip Map.lookup labels) :?? Just oops
      do ((False, ()) <$)

-- A struct with named fields in datatype representation order
data TStruct = TStruct Name [(Name, STyp)]
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance SynthType TStruct where
  wtyp (TStruct _ fields) = WR Non $ HCTyp $ WS (Mut . wtyp . snd <$> fields)
  isDataType (TStruct recName fields) = Just $
    Map.singleton recName fields
  mkconstr self@(TStruct recName fields) = Map.singleton recName \fieldValues -> do
    comment "create struct:"
    for_ fields \(name, _) -> snd $ fieldValues ! name
    ty <- genType (Just recName) self
    sexp "struct.new" [ ty ## coerce recName ]
  unconstr self@(TStruct recName fields) _ (cases, fallback) =
    case Map.lookup recName cases of
      Nothing -> fallback
      Just withFields -> do
        ty <- genType (Just recName) self
        withVar Nothing self \var -> do
          -- Save the record to a variable
          sexp "local.set" [ var ]
          -- Generate all the getters, for the code block to use
          withFields $ Map.fromList $ zip [0..] fields <&>
            \(i, (name, _fieldT)) -> (name,) do
              -- Project out this field from the variable
              sexp "local.get" [ var ]
              sexp "struct.get" [ ty, wasmIdx i ## coerce name ] -- FIXME: packed fields

fieldGetters :: TStruct -> Map Name WASM
fieldGetters self@(TStruct recName fields) =
  Map.fromList $ zip [0..] fields <&>
    \(i, (name, _fieldT)) -> (name,) do
      ty <- genType (Just recName) self
      -- Project out this field from the stack
      sexp "struct.get" [ ty, wasmIdx i ] -- FIXME: packed fields

eField :: Expr -> Name -> Expr
eField t which = case infer t of
  S (TStruct name fields) ->
    let fieldT = (Map.fromList fields ! which) in
    ECase (infer t) t fieldT
      (Map.singleton name (Map.singleton which (Bind which), EVar fieldT which), undefined)
  _ -> error "Not a struct"

-- A tuple is a special struct, with fst and snd fields
pattern TTuple :: STyp -> STyp -> TStruct
pattern TTuple x y = TStruct "Tuple" [("fst", x), ("snd", y)]

eTuple :: Expr -> Expr -> Expr
eTuple x y = ECons (STyp $ TTuple (infer x) (infer y)) "Tuple" [("fst", x), ("snd", y)]

eFst :: Expr -> Expr
eFst t = case infer t of
  S (TTuple xT _yT) ->
    ECase (infer t) t xT (Map.singleton "Tuple" (Map.singleton "fst" (Bind "fst"), EVar xT "fst"), undefined)
  _ -> error "Not a tuple"

eSnd :: Expr -> Expr
eSnd t = case infer t of
  S (TTuple _xT yT) ->
    ECase (infer t) t yT (Map.singleton "Tuple" (Map.singleton "snd" (Bind "snd"), EVar yT "snd"), undefined)
  _ -> error "Not a tuple"


data TBool = TBool
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

pattern ETrue, EFalse :: Expr
pattern ETrue = ECons (S TBool) "True" []
pattern EFalse = ECons (S TBool) "False" []

eIf :: Expr -> Expr -> Expr -> Expr
eIf cond ifTrue ifFalse = ECase (S TBool) cond (infer ifTrue)
  ( Map.singleton "True" (Map.empty, ifTrue), ifFalse )

instance SynthType TBool where
  wtyp TBool = wtyp SU31
  isDataType TBool = Just $ Map.fromList
    [ ("False", []), ("True", []) ]
  mkconstr TBool = Map.fromList
    [ ("False", \_ -> do
        sexp "i32.const" [ wasmIdx 0 ## "False" ]
        tell "ref.i31"
      )
    , ("True", \_ -> do
        sexp "i32.const" [ wasmIdx 1 ## "True" ]
        tell "ref.i31"
      )
    ]
  unconstr TBool outputs (cases, fallback) =
    case (Map.lookup "False" cases, Map.lookup "True" cases) of
      (Nothing, Nothing) -> fallback
      (whenFalse, whenTrue) -> do
        tell "i31.get_u"
        tell . SEXP "if" =<< sequence
          [ foldMap (QUAL "result") <$> traverse genWTyp outputs
          , QUAL "then" <$> outputOf do fromMaybe (const fallback) whenTrue Map.empty
          , QUAL "else" <$> outputOf do fromMaybe (const fallback) whenFalse Map.empty
          ]


data TBitarray = TBitarray
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

bitarrayBacker :: TStruct
bitarrayBacker = TStruct "bitarray" [("bitlen", SU64), ("words", STyp (TArr SU64))]

chunksOf :: Int -> [t] -> [[t]]
chunksOf l = go 0 []
  where
  go :: Int -> [t] -> [t] -> [[t]]
  go _ [] [] = []
  go _ acc [] = [List.reverse acc]
  go n acc ts | n == l = List.reverse acc : go 0 [] ts
  go n acc (t : ts) = go (n+1) (t : acc) ts

idxedChunksOf :: Int -> [t] -> [(Int, [(Int, Int, t)])]
idxedChunksOf l = zip [0..] . chunksOf l . zipWith (\i -> (i `div` l, i `mod` l,)) [0..]

retroactively :: MonadWriter w m => m (a, w) -> m a
retroactively writing = pass $ writing <&> \(r, prewrite) -> (r, (prewrite <>))

instance SynthType TBitarray where
  wtyp TBitarray = wtyp bitarrayBacker
  isArrayType TBitarray = Just (TArr (STyp TBool))
  arraylit t items = do
    wrapperT <- genType Nothing t
    innerT <- genType Nothing (TArr SU64)
    comment "number of bits:"
    tell $ genConst @Word64 $ fromIntegral $ length items
    for_ (items & idxedChunksOf 64) \(word, wordBits) -> do
      comment $ "word #" <> T.show word
      retroactively do
        knownBits <- for wordBits \(_word, bit, (expr, value)) ->
          case expr of
            ETrue -> pure (1 .<<. bit)
            EFalse -> pure 0
            _ -> do
              tell $ genConst @Word64 (1 .<<. bit)
              tell $ genConst @Word64 0
              value
              tell "i31.get_u"
              tell "select"
              tell "i64.or"
              pure 0
        pure ((), genConst @Word64 (sum knownBits))
    comment "create array of words"
    sexp "array.new_fixed" [ innerT, wasmIdx $ (length items + 63) `div` 64 ]
    comment "store bitarray with length"
    sexp "struct.new" [ wrapperT ]
  -- arrayfill t len getter = do
  --   wrapperT <- genType Nothing t
  --   innerT <- genType Nothing (TArr (STyp TBool))
  --   withVar (Just "bitlen") SU32 \vbitlen -> do
  --     arrayfill (TArr (STyp TBool))
  --       do
  --         len
  --         sexp "local.tee" [ vbitlen ]
  --         sexp "i32.add" [ genConst @Word32 63 ]
  --         sexp "i32.div_u" [ genConst @Word32 64 ]
  --       do
  --         withVar (Just "idx") SU32 \vidx -> do
  --           sexp "local.tee" [ vidx ]
  --           withVar (Just "i") SU32 \vi -> do
  --             sexp "local.set" [ vi, genConst @Word32 0 ]
  --             flow (Just "word_bits") ([SU64], [SU64]) \(continue, _break) -> do
  --               _
  --     withVar (Just "array") t \varr -> do
  --       sexp "array.new_default" [ ty ]
  --       sexp "local.set" [ varr ]
  --       withVar (Just "idx") SU32 \vidx -> do
  --         genExpr $ EU32 0
  --         sexp "local.set" [ vidx ]
  --         flow (Just "initialize_array") ([] :: [STyp], [] :: [STyp]) \(continue, _break) -> do
  --           sexp "local.get" [ vidx ]
  --           getter

  --           sexp "local.get" [ vidx ]
  --           sexp "array.set" [ ty ]

  --           sexp "local.get" [ vidx ]
  --           sexp "i32.add" [ genConst "i32" (1 :: Word32) ]
  --           sexp "local.tee" [ vidx ]
  --           sexp "local.get" [ vlen ]
  --           sexp "i32.lt_u" []
  --           sexp "br_if" [ continue ]
  --       sexp "local.get" [ varr ]
  arraylength _t (_, expr) = do
    expr
    fieldGetters bitarrayBacker ! "bitlen"
  arrayindex t (_, arr) (_, idx) = do
    ty <- genType Nothing t
    arr
    withVar (Just "idx") SU32 \vidx -> do
      idx
      sexp "local.set" [ vidx ]
      fieldGetters bitarrayBacker ! "words"
      sexp "local.get" [ vidx ]
      sexp "i32.const" [ "64" ]
      tell "i32.div_u"
      sexp "array.get" [ ty ]
      sexp "local.get" [ vidx ]
      sexp "i32.const" [ "64" ]
      tell "i32.rem_u"
      tell "i64.shr_u"


instance SynthType TIOL where
  wtyp TIOList = WR Non $ HCTyp $ WA $ Mut $ wtyp TIOData
  wtyp TIOText = wtyp TTxt
  wtyp TIOData = WR Non $ HCls HArray -- supertype of TIOList and TIOText

  isArrayType TIOList = Just (TArr SIOData)
  isArrayType TIOText = isArrayType TTxt
  isArrayType TIOData = Nothing

  arraylit t items = do
    ty <- genType Nothing t
    comment "create array literal:"
    traverse_ snd items
    sexp "array.new_fixed" [ ty, wasmLen $ length items ]
  arraylength _ (_, expr) = do
    expr
    tell "array.len"
  arrayindex TIOText (_, arr) (_, idx) = do
    ty <- genType Nothing TIOText
    arr
    idx
    sexp "array.get_u" [ ty ]
  arrayindex t (_, arr) (_, idx) = do
    ty <- genType Nothing t
    arr
    idx
    sexp "array.get" [ ty ]

  isDataType TIOData = Just $ Map.fromList
    [ ("IOList", [("data", SIOData)])
    , ("IOText", [("data", SIOText)])
    ]
  isDataType _ = Nothing

  mkconstr _ = Map.fromList $ ["IOList", "IOText"] <&> (, \fields -> snd $ fields ! "data")
  unconstr _ o (cases, fallback) = do
    void $ brCasts ([SIOData], S . TPrm Uns <$> o)
      [ withLexVar Nothing TIOList \(_expr, var) -> do
          sexp "local.set" [ var ]
          cases ! "IOList" $ Map.singleton "data" $ sexp "local.get" [ var ]
          pure (wtyp TIOList, ())
      , withLexVar Nothing TIOText \(_expr, var) -> do
          sexp "local.set" [ var ]
          cases ! "IOText" $ Map.singleton "data" $ sexp "local.get" [ var ]
          pure (wtyp TIOText, ())
      ] fallback

data IOFun = IOByteLength | IOFlatten | IOFlattenAux
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving anyclass (NFData)

instance ToFuncDef IOFun where
  toFuncDef IOByteLength = FuncDef (Just "iobytelength") [wtyp TIOData] [WI32] [] do
    withVar (Just "length") (TPrm Uns WI32) \var -> do
      sexp "local.set" [ var, genConst @Word32 0 ]
      let
        acc = do
          sexp "local.get" [ var ]
          sexp "i32.add" []
          sexp "local.set" [ var ]
      sexp "local.get" [ wasmIdx 0 ]
      unconstr TIOData []
        ( Map.fromList
          [ ("IOList", \fields -> do
              ty <- genType Nothing TIOList
              let getter = fields ! "data"
              withVar (Just "idx") (TPrm Uns WI32) \vidx -> do
                sexp "local.set" [ vidx, genConst @Word32 0 ]
                flow Nothing mempty \(continue, break) -> do
                  sexp "local.get" [ vidx ]
                  arraylength TIOList (undefined, getter)
                  sexp "i32.ge_u" []
                  sexp "br_if" [ break ]

                  getter
                  sexp "local.get" [ vidx ]
                  sexp "array.get" [ ty ]
                  sexp "call" =<< sequence [ funcDef IOByteLength ]
                  acc

                  sexp "local.get" [ vidx ]
                  sexp "i32.add" [ genConst @Word32 1 ]
                  sexp "local.set" [ vidx ]
                  sexp "br" [ continue ]
            )
          , ("IOText", \fields -> do
              arraylength TIOText (undefined, fields ! "data")
              acc
            )
          ]
        , tell "unreachable"
        )
      sexp "local.get" [ var ]
  toFuncDef IOFlatten = FuncDef (Just "ioflatten") [wtyp TIOData] [wtyp TIOText] [] do
    withVar (Just "buffer") TTxt \var -> do
      sexp "array.new_default" =<< sequence
        [ genType Nothing TTxt
        , outputOf do
            sexp "local.get" [ wasmIdx 0 ]
            sexp "call" . pure =<< funcDef IOByteLength
        ]
      sexp "local.tee" [ var ]
      tell $ genConst @Word32 0
      sexp "local.get" [ wasmIdx 0 ]
      sexp "call" . pure =<< funcDef IOFlattenAux
      tell "drop"
      sexp "local.get" [ var ]
  toFuncDef IOFlattenAux = FuncDef (Just "ioflatten_aux") [wtyp TIOText, WI32, wtyp TIOData] [WI32] [] do
    sexp "local.get" [ wasmIdx 2 ]
    unconstr TIOData []
      ( Map.fromList
        [ ("IOList", \fields -> do
            ty <- genType Nothing TIOList
            let getter = fields ! "data"
            withVar (Just "idx") (TPrm Uns WI32) \vidx -> do
              sexp "local.set" [ vidx, genConst @Word32 0 ]
              flow Nothing mempty \(continue, break) -> do
                sexp "local.get" [ vidx ]
                arraylength TIOList (undefined, getter)
                sexp "i32.ge_u" []
                sexp "br_if" [ break ]

                sexp "local.get" [ wasmIdx 0 ]
                sexp "local.get" [ wasmIdx 1 ]
                getter
                sexp "local.get" [ vidx ]
                sexp "array.get" [ ty ]
                sexp "call" =<< sequence [ funcDef IOFlattenAux ]

                sexp "local.set" [ wasmIdx 1 ]

                sexp "local.get" [ vidx ]
                sexp "i32.add" [ genConst @Word32 1 ]
                sexp "local.set" [ vidx ]
                sexp "br" [ continue ]
          )
        , ("IOText", \fields -> do
            sexp "array.copy" =<< sequence
              [ genType Nothing TTxt
              , genType Nothing TTxt
              , outputOf do sexp "local.get" [ wasmIdx 0 ] -- dest
              , outputOf do sexp "local.get" [ wasmIdx 1 ] -- dest_idx
              , outputOf do fields ! "data" -- src
              , pure do genConst @Word32 0 -- src_idx
              , SEXP "array.len" <$> sequence
                  [outputOf (fields ! "data")] -- len
              ]

            arraylength TIOText (undefined, fields ! "data")
            sexp "local.get" [ wasmIdx 1 ]
            sexp "i32.add" []
            sexp "local.set" [ wasmIdx 1 ]
          )
        ]
      , tell "unreachable"
      )
    sexp "local.get" [ wasmIdx 1 ]

instance Semantics IOFun where
  semTyp IOByteLength = SFun [SIOData] SU32
  semTyp IOFlatten = SFun [SIOData] (S TTxt)
  semTyp IOFlattenAux = SFun [SIOText, SU32, SIOData] SU32

  semCode = funcCall


-- Another existential type, for foreign semantics
data ForeignSemantics = forall sem. Semantics sem => ForeignSemantics sem

pattern Sem :: Semantics sem => Semantics sem => sem -> ForeignSemantics
pattern Sem sem <- ForeignSemantics (Typeable.cast -> Just (sem :: sem)) where
  Sem sem = ForeignSemantics sem

pattern ESem :: Semantics sem => Semantics sem => sem -> Expr
pattern ESem sem = EForeign (Sem sem)

pattern (:$$:) :: Semantics sem => Semantics sem => sem -> [Expr] -> Expr
pattern sem :$$: args = EApp (ESem sem) args

class Existentiable sem => Semantics sem where
  semTyp :: sem -> STyp
  semCode :: sem -> WASM
  semEval :: sem -> [Expr] -> Map Name Expr -> Maybe Expr
  semEval _ _ _ = Nothing
  semExprs :: sem -> [Expr]
  semExprs = mempty
  semEffects :: sem -> Effects
  semEffects = mempty

instance Eq ForeignSemantics where
  ForeignSemantics spec0 == ForeignSemantics spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 == spec2
    | otherwise = False
instance Ord ForeignSemantics where
  ForeignSemantics spec0 `compare` ForeignSemantics spec2
    | Just spec1 <- Typeable.cast spec0 = spec1 `compare` spec2
    | otherwise = Typeable.typeOf spec0 `compare` Typeable.typeOf spec2
instance NFData ForeignSemantics where
  rnf (ForeignSemantics spec) = rnf spec
instance Show ForeignSemantics where
  show (ForeignSemantics spec) = show spec
instance Semantics ForeignSemantics where
  semTyp (ForeignSemantics sem) = semTyp sem
  semCode (ForeignSemantics sem) = semCode sem
  semEval (ForeignSemantics sem) = semEval sem
  semExprs (ForeignSemantics sem) = semExprs sem
  semEffects (ForeignSemantics sem) = semEffects sem


--------------------------------------------------------------------------------
-- Static analysis
--------------------------------------------------------------------------------


data Effect
  = Pure -- ^ no effect here!
  | Benign -- ^ guaranteed by compiler. hopefully. (e.g. pattern matching failure)
  | Important -- ^ user-inserted, make sure to respect it
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
instance Semigroup Effect where
  Pure <> ef = ef
  ef <> Pure = ef
  Important <> _ = Important
  _ <> Important = Important
  Benign <> Benign = Benign
instance Monoid Effect where mempty = Pure

-- A description of side effects that code may have
  -- Dupable; Dupable under duress
  -- Dropable
  -- Reorderable
  -- Interpretable
data Effects = Effects
  { efForeign :: Effect
  -- ^ Foreign Benign means that it is implemented using foreigns but can be
  -- interpreted purely still. Foreign Important means that it can only be
  -- interpreted at runtime and anything could happen.
  , efError :: Effect
  -- ^ Error Benign is for pattern match failures. Error Important is for
  -- user errors.
  , efNonTerm :: Effect
  -- ^ (Probably not going to use this)
  , efMutation :: Effect
  -- ^ Mutation. I guess maybe ST is Benign mutation, anything else is Important.
  , efDebug :: Effect
  -- ^ Debugging effects
  , efOrdering :: Effect
  -- ^ Ordering dependence. For Important Errors and for most forms of mutation.
  -- Disjoint mutations and I guess semilattice-style mutations can be
  -- reorderable.
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup Effects where
  e1 <> e2 = Effects
    { efForeign = efForeign e1 <> efForeign e2
    , efError = efError e1 <> efError e2
    , efNonTerm = efNonTerm e1 <> efNonTerm e2
    , efMutation = efMutation e1 <> efMutation e2
    , efDebug = efDebug e1 <> efDebug e2
    , efOrdering = efOrdering e1 <> efOrdering e2
    }
instance Monoid Effects where
  mempty = Effects
    { efForeign = mempty
    , efError = mempty
    , efNonTerm = mempty
    , efMutation = mempty
    , efDebug = mempty
    , efOrdering = mempty
    }
deriving via Effects instance Semigroup (Sequence Effects)
deriving via Effects instance Monoid (Sequence Effects)


data ForeignCall = ForeignCall STyp Effects Name
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance Semantics ForeignCall where
  semTyp (ForeignCall t _ _) = t
  semCode (ForeignCall _ _ name) = tell $ SEXP "call" [ wasmID name ]
  semEval _ _ _ = Nothing
  semExprs _ = []
  semEffects (ForeignCall _ ef _) = ef


-- https://blog.veritates.love/monoids_in_public.html#top-down-traversals
data Visit m
  -- Skip evaluating the children
  = ShortCircuit m
  -- Evaluate the children and append this value
  | Append m
  -- Customize the result of evaluating the children
  | Continue (m -> m)

instance Monoid m => Monoid (Visit m) where mempty = ShortCircuit mempty
instance Semigroup m => Semigroup (Visit m) where
  Append m1 <> Append m2 = Append (m1 <> m2)
  ShortCircuit m1 <> ShortCircuit m2 = ShortCircuit (m1 <> m2)
  Append m1 <> ShortCircuit m2 = Append (m1 <> m2)
  ShortCircuit m1 <> Append m2 = Append (m1 <> m2)
  f1 <> f2 = Continue (cont f1 <> cont f2)
    where
    cont = \case
      Continue f -> f
      Append m -> (m <>)
      ShortCircuit m -> const m

visit ::
  forall m.
    Monoid m =>
  (Expr -> m) ->
  (Expr -> m)
visit f = visit' (Append . f)

visit' ::
  forall m.
    Monoid m =>
  (Expr -> Visit m) ->
  (Expr -> m)
visit' f = go
  where
  go e = case f e of
    -- Avoid computing `children e`
    ShortCircuit m -> m
    -- Just append, like normal `visit`
    Append m -> m <> children e
    -- Allow it to manipulate `children e`
    -- however it wants to based on `e`
    Continue mm -> mm (children e)

  -- Recurse into the children
  children = \case
    EF64 _ -> mempty
    EF32 _ -> mempty
    EU64 _ -> mempty
    EU32 _ -> mempty
    ES64 _ -> mempty
    ES32 _ -> mempty
    ETxt _ -> mempty
    EError _ _ _ -> mempty
    ECons _ _ fields -> foldMap (go . snd) fields
    ECase _ scrutinee _ (cases, fallback) -> go scrutinee <> foldMap (go . snd) cases <> go fallback
    EArray _ items -> foldMap go items
    EArrayFill _ len getter -> go len <> go getter
    ELength e -> go e
    EIndex _ arr idx -> go arr <> go idx
    ELet binds e -> foldMap (go . fst) binds <> go e
    EVar _ _ -> mempty
    EFun _ _ body -> go body
    EApp fun args -> go fun <> foldMap go args
    EForeign sem -> foldMap go (semExprs sem)

allForeign :: (forall sem. Semantics sem => sem -> Bool) -> Expr -> Bool
allForeign p = coerce . visit' \case
  EForeign (ForeignSemantics sem)
    | not (p sem) -> ShortCircuit (All False)
  _ -> Append mempty

anyForeign :: (forall sem. Semantics sem => sem -> Bool) -> Expr -> Bool
anyForeign p = coerce . visit' \case
  EForeign (ForeignSemantics sem)
    | not (p sem) -> ShortCircuit (Any True)
  _ -> Append mempty



-- Inject a single sample into a monoid, useful for seeing through layers,
-- sharing samples across multiple metrics (like min and max), and so on.
class Semigroup m => Sample s m where
  sample :: s -> m

samples :: forall m s f. Sample s m => Monoid m => Foldable f => f s -> m
samples = foldMap sample

samples1 :: forall m s f. Sample s m => Foldable1 f => f s -> m
samples1 = foldMap1 sample

instance Sample s m => Sample s (Maybe m) where
  sample = Just . sample
instance (Sample s m1, Sample s m2) => Sample s (m1, m2) where
  sample s = (sample s, sample s)
instance Ord o => Sample o (Min o) where sample = coerce
instance Ord o => Sample o (Max o) where sample = coerce
instance Num n => Sample n (Sum n) where sample = coerce
instance Num n => Sample n (Product n) where sample = coerce
instance Sample Bool Any where sample = coerce
instance Sample Bool All where sample = coerce

-- | Newtype specifically for *sequential* combining, since disjunctive
-- combining is the natural one to specify.
newtype Sequence m = Sequence m
  deriving newtype (Eq, Ord, NFData)
pattern Branch :: Sequence m -> m
pattern Branch c = Coerce c

type StaticAnalysis m = (Monoid m, Monoid (Sequence m))

class (Semigroup m, Semigroup (Sequence m)) => StaticAnalysis1 m where
  -- Should be idempotent and commute with `(<>) @m`
  withPureBranch :: m -> m
  default withPureBranch :: StaticAnalysis m => m -> m
  withPureBranch = (<>) (Branch mempty)


(~|~) :: forall m. Semigroup m => Sequence m -> Sequence m -> Sequence m
m1 ~|~ m2 = Sequence (Branch m1 <> Branch m2)

(->-) :: forall m. Semigroup (Sequence m) => m -> m -> m
m1 ->- m2 = Branch (Sequence m1 <> Sequence m2)

analyzeBranches :: forall f m. Foldable f => StaticAnalysis m => f m -> m
analyzeBranches = fold @f @m

analyzeSequence :: forall f m. Foldable f => StaticAnalysis m => f m -> m
analyzeSequence = Branch . foldMap Sequence

summarizeCases :: forall f m. Foldable f => Monoid m => f (Sequence m) -> Sequence m
summarizeCases = Sequence . foldMap Branch


-- | Has no case-aware behavior.
newtype Simple m = Simple m
  deriving newtype (Eq, Ord, NFData)

deriving newtype instance Semigroup m => Semigroup (Simple m)
deriving newtype instance Monoid m => Monoid (Simple m)
deriving newtype instance Semigroup m => Semigroup (Sequence (Simple m))
deriving newtype instance Monoid m => Monoid (Sequence (Simple m))

instance Semigroup (Sequence m) => Semigroup (Sequence (Maybe m)) where
  (<>) = coerce ((<>) @(Maybe (Sequence m)))
instance Semigroup (Sequence m) => Monoid (Sequence (Maybe m)) where
  mempty = coerce (mempty @(Maybe (Sequence m)))

deriving newtype instance Semigroup (Sequence ())
deriving newtype instance Monoid (Sequence ())
-- Tropical semirings, like `Bounds o`
deriving via (Sum o) instance Num o => Semigroup (Sequence (Max o))
deriving via (Sum o) instance Num o => Semigroup (Sequence (Min o))
-- `All`: Definitely happens in a course of control flow: all branches,
-- any instruction along them
deriving via Any instance Semigroup (Sequence All)
deriving via Any instance Monoid (Sequence All)
-- `Any`: Could happen somewhere in a course of control flow (not `deriving via All`!)
deriving via Any instance Semigroup (Sequence Any)
deriving via Any instance Monoid (Sequence Any)

-- | Let the default `Set` be `OverSet`, with unions everywhere
deriving via Set o instance Ord o => Semigroup (Sequence (Set o))
deriving via Set o instance Ord o => Monoid (Sequence (Set o))

-- | For under-approximation of requirements,
-- | sequencing is union and branching is intersection.
newtype UnderSet o = UnderSet (Set o)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance Ord o => Semigroup (UnderSet o) where
  (<>) = coerce (Set.intersection @o)
deriving via Set o instance Ord o => Semigroup (Sequence (UnderSet o))
deriving via Set o instance Ord o => Monoid (Sequence (UnderSet o))

-- | Sequencing and branching are both union, for
-- | over-approximation/tallying what occurs.
newtype OverSet o = OverSet (Set o)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance Ord o => Semigroup (Sequence (OverSet o))
deriving newtype instance Ord o => Monoid (Sequence (OverSet o))

-- | The converse of `UnderSet`, for agreement under sequencing.
-- | Maybe useful for tracking what constructors a value may have,
-- | for example.
newtype AgreeSet s = AgreeSet (Set s)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
  deriving newtype (Semigroup, Monoid)

instance Ord o => Semigroup (Sequence (AgreeSet o)) where
  (<>) = coerce (Set.intersection @o)


-- Union, but with both operations inside. For tabulating information, e.g.
-- by variable.
instance (Ord k, Semigroup (Sequence v)) => Semigroup (Sequence (MonoidalMap k v)) where
  (<>) = coerce (MonoidalMap.unionWith ((<>) @(Sequence v)))



-- After an error, analysis does not apply (unreachable control flow).
data Flowy m = EndsInDisaster m | CanEndCleanly m | EndsCleanly m
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance Monoid (Sequence m) => Monoid (Sequence (Flowy m)) where
  mempty = Sequence (EndsCleanly (Branch mempty))
instance Semigroup (Sequence m) => Semigroup (Sequence (Flowy m)) where
  -- Cut off analysis after a guaranteed error
  Sequence (EndsInDisaster m) <> _ = Sequence (EndsInDisaster m)
  -- Still ends in error
  Sequence (flowed -> m1) <> Sequence (EndsInDisaster m2) = coerce (EndsInDisaster @m) $ m1 ->- m2
  -- Ends cleanly
  Sequence (EndsCleanly m1) <> Sequence (EndsCleanly m2) = coerce (EndsCleanly @m) $ m1 ->- m2
  -- Mixed cases
  Sequence (flowed -> m1) <> Sequence (flowed -> m2) = coerce (CanEndCleanly @m) $ m1 ->- m2
instance Semigroup m => Semigroup (Flowy m) where
  -- All branches end in disaster still
  EndsInDisaster m1 <> EndsInDisaster m2 = EndsInDisaster $ m1 <> m2
  -- All branches end cleanly still
  EndsCleanly m1 <> EndsCleanly m2 = EndsCleanly $ m1 <> m2
  -- Mixed cases
  (flowed -> m1) <> (flowed -> m2) = CanEndCleanly $ m1 <> m2
instance Sample s m => Sample s (Flowy m) where
  sample = EndsCleanly . sample

instance StaticAnalysis1 m => StaticAnalysis1 (Flowy m) where
  withPureBranch (flowed -> m) = CanEndCleanly (withPureBranch m)

flowed :: Flowy m -> m
flowed (EndsInDisaster m) = m
flowed (CanEndCleanly m) = m
flowed (EndsCleanly m) = m

-- Add identities
data MaybeAnalysis m = PureAnalysis | AbsurdAnalysis | SomeAnalysis (Flowy m)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance StaticAnalysis1 m => Semigroup (MaybeAnalysis m) where
  AbsurdAnalysis <> m = m
  m <> AbsurdAnalysis = m
  PureAnalysis <> PureAnalysis = PureAnalysis
  SomeAnalysis m <> PureAnalysis = SomeAnalysis (withPureBranch m)
  PureAnalysis <> SomeAnalysis m = SomeAnalysis (withPureBranch m)
  SomeAnalysis m1 <> SomeAnalysis m2 = SomeAnalysis (m1 <> m2)
instance StaticAnalysis1 m => Monoid (MaybeAnalysis m) where
  mempty = AbsurdAnalysis

instance Semigroup (Sequence m) => Monoid (Sequence (MaybeAnalysis m)) where
  mempty = Sequence PureAnalysis
instance Semigroup (Sequence m) => Semigroup (Sequence (MaybeAnalysis m)) where
  Sequence PureAnalysis <> m = m
  m <> Sequence PureAnalysis = m
  Sequence AbsurdAnalysis <> _ = Sequence AbsurdAnalysis
  Sequence (SomeAnalysis (flowed -> m)) <> Sequence AbsurdAnalysis =
    Sequence (SomeAnalysis (EndsInDisaster m))
  Sequence (SomeAnalysis m1) <> Sequence (SomeAnalysis m2) = Sequence (SomeAnalysis (m1 ->- m2))

unMaybeAnalysis :: forall m. StaticAnalysis m => MaybeAnalysis m -> m
unMaybeAnalysis (SomeAnalysis m) = flowed m
unMaybeAnalysis PureAnalysis = analyzeSequence []
unMaybeAnalysis AbsurdAnalysis = analyzeBranches []



-- Maintain bounds for a summable value (e.g. occurrences).
data Bounds o = Bounds { bndMin :: !o, bndMax :: !o }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
bound :: o -> Bounds o
bound = join Bounds
instance Ord o => Semigroup (Bounds o) where
  Bounds minL maxL <> Bounds minR maxR = Bounds (min minL minR) (max maxL maxR)
instance Num o => Semigroup (Sequence (Bounds o)) where
  Sequence (Bounds minL maxL) <> Sequence (Bounds minR maxR) =
    Sequence $ Bounds (minL + minR) (maxL + maxR)
instance Ord o => Sample o (Bounds o) where
  sample = bound

-- Track the proportion of cases where something occurs or does not.
data Proportion n = Proportion { pWith :: !n, pWithout :: !n }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)
instance Num n => Semigroup (Proportion n) where
  Proportion wiL woL <> Proportion wiR woR = Proportion (wiL + wiR) (woL + woR)
instance Num n => Semigroup (Sequence (Proportion n)) where
  -- This assumes that cases are independent
  Sequence (Proportion sL1 sL2) <> Sequence (Proportion sR1 sR2) =
    Sequence $ Proportion (sL * sR - s2) s2
    where
    s2 = sL2 * sR2
    sR = sR1 + sR2
    sL = sL1 + sL2

-- Overall usage of something, including the bounds on the number of times
-- it will be used and the proportion of cases where it is used.
-- This automatically handles control flow (cutting off analysis after errors)
-- and empty cases (via `Maybe`).
newtype Demand = Demand (Flowy (Maybe (Bounds Int, Proportion Int)))
  deriving newtype (Eq, Ord, Semigroup, NFData)
pattern Demanded :: Demand -> Flowy (Maybe (Bounds Int, Proportion Int))
pattern Demanded dd = Coerce dd

instance Sample Bool Demand where sample = mkDemand

mkDemand :: Bool -> Demand
mkDemand True = demand
mkDemand False = noDemand

demand, noDemand :: Demand
demand = Demand (CanEndCleanly (Just (bound 1, Proportion 1 0)))
noDemand = Demand (CanEndCleanly (Just (bound 0, Proportion 0 1)))

ddBounds :: Demand -> Bounds Int
ddBounds = Demanded >>> flowed >>> maybe (bound 0) fst
ddProportion :: Demand -> Proportion Int
ddProportion = Demanded >>> flowed >>> maybe (Proportion 0 0) snd

ddAlways :: Demand -> Bool
ddAlways = (== 0) . pWithout . ddProportion

ddNever :: Demand -> Bool
ddNever = (== 0) . pWith . ddProportion

acrossCases :: forall m. StaticAnalysis m => (Expr -> m) -> Expr -> m
acrossCases f = acrossCases' (Append . f)

-- Uses `Monoid (Sequence m)` for sequencing and `Monoid m` for summing
-- case branches.
acrossCases' :: forall m. StaticAnalysis m => (Expr -> Visit m) -> Expr -> m
acrossCases' f = Branch . visit' \case
  EError _ _ _ -> ShortCircuit (Sequence mempty)
  expr@(ECase _ scrutinee _ (cases, fallback)) ->
    let
      go = Sequence . acrossCases' f
      children = go scrutinee <> do
        -- Sum across the case branches (with `Monoid m` instead of `Monoid (Sequence m)`)
        summarizeCases (Map.elems (go . snd <$> cases)) ~|~ go fallback
    in ShortCircuit case coerce (f expr) :: Visit (Sequence m) of
      ShortCircuit m -> m
      Append m -> m <> children
      Continue mm -> mm children
  expr -> coerce (f expr) :: Visit (Sequence m)


--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------



data TOK
  = T_NAME Name
  | T_NUMBER Text
  | T_STRING Text
  | T_ Text
  deriving (Eq, Ord, Show, Generic, NFData)
data SYNTAX
  = S_NAME
  | S_NUMBER
  | S_STRING
  | S_ Text
  deriving (Eq, Ord, Show, Generic, NFData)

symbolTable :: Set Text
symbolTable = Set.fromList
  [ "@"
  , "|", "&"
  , ":=", "?=", "??", "?"
  , "(", ")"
  , "{", "}"
  , "[", "]"
  , ",", ";", ":", "::"
  , "\\", "λ", "."
  , "->"
  , "#", "!"
  , "_"
  ]

classify :: TOK -> SYNTAX
classify (T_NAME _) = S_NAME
classify (T_NUMBER _) = S_NUMBER
classify (T_STRING _) = S_STRING
classify (T_ t) = S_ t

lex :: Text -> [TOK]
lex = maybe [] fst . P.lex tokenizer . fmap (join (,)) . T.unpack

parse :: Text -> Maybe (Map Name Expr)
parse = P.lexAndParse (id, classify) (parser <$ tokenizer) . T.unpack

type LEX = P.Lexer Char Char TOK

tokenizer :: LEX ()
tokenizer =
  void $ wsTok *> many (oneTok <* wsTok)
  where
  wsTok = P.skipToken $ many (P.token ' ' <|> P.token '\n' <|> P.token '\r' <|> P.token '\t')

  oneTok = asum
    [ symbols
    , P.parseToken $ T_STRING . T.pack <$> P.jsonString
    , P.parseToken $ T_NUMBER . T.pack . fst <$> P.withSource P.jsonNumber
    , P.parseToken $ T_NAME . Name . T.pack <$> do
        (:) <$> P.satisfies isAlpha <*> many (P.satisfies isAlphaNum)
    ]

  symbols :: LEX ()
  symbols = foldr (\str more -> P.Lexer (P.try $ P.keyword (T.unpack str) (T_ str)) <|> more) empty symbolTable

type PARSE = P.ParseMachine SYNTAX TOK

run :: NFData r => PARSE r -> Text -> Either String r
run p s = unsafePerformIO do
  happened <- try do
    case P.lexAndParse (id, classify) (p <$ tokenizer) . T.unpack $ s of
      Nothing -> pure Nothing
      Just r -> evaluate $ force $ Just r
  pure case happened of
    Left (ErrorCall e :: ErrorCall) -> Left e
    Right Nothing -> Left "Parse error"
    Right (Just r) -> Right r

parser :: PARSE (Map Name Expr)
parser = fmap Map.fromList $ many $ asum
  [ do
    t"@"
    name <- tNAME
    (name,) <$> asum
      [ do
        ty <- t":" *> parseType
        t":=" *> parseTypedExpr (const ty) <@> Map.empty
      , do
        t":=" *> parseExpr <@> Map.empty <&> \case
          Insensitive expr -> expr
          TypeSensitive (Just suggested) getExpr -> getExpr suggested
          TypeSensitive Nothing _ -> error "Ambiguous top-level definition"
      ]
  ]
  where
  t :: Text -> PARSE ()
  t = void . P.token . S_

parseType :: PARSE STyp
parseType = parseTypeAtom & functioning
  where
  parseTypeAtom = asum
    [ SF64 <$ named "f64"
    , SF32 <$ named "f32"
    , SU64 <$ named "u64"
    , SU32 <$ named "u32"
    , SS64 <$ named "s64"
    , SS32 <$ named "s32"
    , STxt <$ named "txt"
    , S TBool <$ named "bool"
    , S TBitarray <$ named "bits"
    , S . TEnum <$> do named "enum" *> many tNAME
    , S . TArr <$> do named "array" *> parseTypeAtom
    , S <$> P.try do TStruct <$> (optional (named "struct") *> tNAME) <*> structFields
    , t"(" *> parseType <* t")"
    ]
  functioning atom = mkFun <$> do
    liftA2 (:) atom do many (t"->" *> atom)
  mkFun [o] = o
  mkFun ts = S $ TFun (List.init ts) (List.last ts)

  structFields = t"{" *> separated ((tNAME <* t":") /|\ parseType) <* t"}"

  t :: Text -> PARSE ()
  t = void . P.token . S_

data TypeSensitive t
  = Insensitive t
  | TypeSensitive
      (Maybe STyp) -- suggested type
      (STyp -> t) -- process with given type
  deriving (Functor)

homogeneous :: [TypeSensitive v] -> TypeSensitive [v]
homogeneous = go (Insensitive [])
  where
  go acc [] = reverse <$> acc
  go (Insensitive acc) (Insensitive v : vs) =
    go (Insensitive (v : acc)) vs
  go (Insensitive acc) (TypeSensitive mt v : vs) =
    go (TypeSensitive mt \t -> v t : acc) vs
  go (TypeSensitive mt acc) (Insensitive v : vs) =
    go (TypeSensitive mt \t -> v : acc t) vs
  go (TypeSensitive (Just t0) acc) (TypeSensitive (Just t1) v : vs) =
    if t0 /= t1 then error "Mismatched types" else
      go (TypeSensitive (Just t0) \t -> v t : acc t) vs
  go (TypeSensitive mt0 acc) (TypeSensitive mt1 v : vs) =
    go (TypeSensitive (mt0 <|> mt1) \t -> v t : acc t) vs

allMatch :: Eq v => [v] -> v
allMatch [] = error "No match"
allMatch vs = case List.group vs of
  [v : _] -> v
  _ -> error "Mismatched"

parseExpr, parseAtom :: PARSE (Map Name STyp -> TypeSensitive Expr)
parseAtom = (asum . fmap P.try)
  [ tNUMBER <&> \num _ ->
      TypeSensitive (Just SF64) \case
        SF64 -> EF64 $ read $ T.unpack num
        SF32 -> EF32 $ read $ T.unpack num
        SU64 -> EU64 $ read $ T.unpack num
        SU32 -> EU32 $ read $ T.unpack num
        SS64 -> ES64 $ read $ T.unpack num
        SS32 -> ES32 $ read $ T.unpack num
        S TBool -> if num == "0" then EFalse else ETrue
        _ -> error "Wrong type for a number"
  , tSTRING <&> \str _ ->
      Insensitive $ ETxt str
  , named "ERR" *> tSTRING <&> \err _ ->
      TypeSensitive Nothing \ty -> EError ty mempty err
  , t"(" *> parseExpr <* t")"
  , (t"[" *> separated parseExpr <* t"]") <&> \parsedItems ctx ->
      case homogeneous (sequence parsedItems ctx) of
        Insensitive [] -> TypeSensitive Nothing
          \arrT -> EArray arrT []
        Insensitive items -> TypeSensitive (Just (allMatch (infer <$> items)))
          \arrT -> EArray arrT items
        TypeSensitive mitemT getItems -> TypeSensitive (S . TArr <$> mitemT)
          \arrT -> EArray arrT $ getItems $ case isArrayType arrT of
            Just (TArr itemT) -> itemT
            Nothing -> error "Was not an array type"
  , liftA2 (,) (optional tNAME) (t"{" *> separated parseField <* t"}") <&> \(consNamed, parsedFields) ctx ->
      TypeSensitive Nothing \structT ->
        case isDataType structT of
          Nothing -> error "Constructor of non data type"
          Just constructors ->
            let
              (constructorName, constructor) = case consNamed of
                Just consName -> case Map.lookup consName constructors of
                  Just cons -> (consName, cons)
                  Nothing -> error $ "Type does not have constructor: " <> show consName
                Nothing | [oneCase] <- Map.toList constructors -> oneCase
                _ -> error "Ambiguous constructor"
              namedFields = Map.fromList $ parsedFields & mapMaybe \(l, r) -> (,r) <$> l
              unnamedFields = constructor & filter \(name, _) -> not $ Map.member name namedFields
              filledIn = namedFields <> Map.fromList do
                zipWith (\(name, _) (_, r) -> (name, r)) unnamedFields (filter ((== Nothing) . fst) parsedFields)
              fields = sequence filledIn ctx
            in ECons structT constructorName $ constructor <&>
              \(fieldName, fieldT) -> (fieldName, applyType fieldT $ fields ! fieldName)
  , tNAME <&> \name ctx ->
      case Map.lookup name ctx of
        Just ty -> Insensitive $ EVar ty name
        Nothing -> TypeSensitive Nothing \enumT ->
          case isDataType enumT of
            Just constructors
              | Just [] <- Map.lookup name constructors -> ECons enumT name []
            _ -> error $ "Missing name/nullary constructor: " <> show name
  , do
    t"\\"
    binds <- many binder
    t"."
    body <- parseExpr
    pure \ctx -> TypeSensitive Nothing \case
      funT@(SFun inputs output) ->
        let
          bodyT = case List.drop (length binds) inputs of
            [] -> output
            leftover -> SFun leftover output
        in EFun funT binds $
          applyType bodyT $ body $
            ctx <> gatherBinders id (zip inputs binds)
      _ -> error "Not a function type for the lambda"
  ]
  where
  t :: Text -> PARSE ()
  t = void . P.token . S_

  parseField = (,) <$> optional (tNAME <* t":=") <*> parseExpr
  binder = Bind <$> tNAME <|> Discard <$ t"_"

-- binops: `:`, `#`, `!`
parseExpr = asum
  [ do
    e1 <- parseAtom
    asum
      [ do
          typ <- t":" *> parseType
          pure \ctx -> Insensitive $ applyType typ (e1 ctx)
      , do
          t"#"
          e2 <- parseAtom
          pure \ctx ->
            let
              lenE = applyType SU32 (e1 ctx)
              fun2arr (SFun [SU32] itemT) = SArr itemT
              fun2arr _ = error "Bad array filler type"
              arr2fun arrT = case isArrayType arrT of
                Just (TArr itemT) -> SFun [SU32] itemT
                _ -> error "Bad array type"
            in case e2 ctx of
              Insensitive getterE ->
                TypeSensitive (Just $ fun2arr $ infer getterE) \arrT ->
                  EArrayFill arrT lenE getterE
              TypeSensitive mgetterT getter ->
                TypeSensitive (fun2arr <$> mgetterT) \arrT ->
                  EArrayFill arrT lenE (getter (arr2fun arrT))
      , do
          t"!"
          e2 <- parseAtom
          pure \ctx ->
            let
              idxE = applyType SU32 $ e2 ctx
              arr2item = isArrayType >>> \case
                Just (TArr itemT) -> itemT
                _ -> error "Was not array type"
            in case e1 ctx of
              Insensitive arrE ->
                Insensitive $ EIndex mempty arrE $ idxE
              TypeSensitive marrT getArray ->
                TypeSensitive (arr2item <$> marrT) \itemT ->
                  case marrT of
                    Just given | arr2item given == itemT ->
                      EIndex mempty (getArray given) idxE
                    _ -> EIndex mempty (getArray (SArr itemT)) idxE
      , pure e1
      ]
  ]
  where
  t :: Text -> PARSE ()
  t = void . P.token . S_

parseSomeExpr :: PARSE Expr
parseSomeExpr = parseExpr <@> Map.empty <&> \case
  Insensitive expr -> expr
  TypeSensitive (Just suggested) getExpr -> getExpr suggested
  TypeSensitive Nothing getExpr -> getExpr $ error "Unknown type"

parseTypedExpr :: (Map Name STyp -> STyp) -> PARSE (Map Name STyp -> Expr)
parseTypedExpr getType = parseExpr <&> \getExpr ctx ->
  applyType (getType ctx) (getExpr ctx)

parseTypedExpr' :: STyp -> PARSE Expr
parseTypedExpr' getType = parseTypedExpr (const getType) <@> Map.empty

applyType :: STyp -> TypeSensitive Expr -> Expr
applyType exprT = \case
  Insensitive expr
    | exprT == infer expr -> expr
    | otherwise -> error "Expected type mismatch"
  TypeSensitive _ getTyped -> getTyped exprT

tNUMBER :: PARSE Text
tNUMBER = P.token S_NUMBER & mapMaybe \case { T_NUMBER v -> Just v; _ -> Nothing }
tSTRING :: PARSE Text
tSTRING = P.token S_STRING & mapMaybe \case { T_STRING v -> Just v; _ -> Nothing }
tNAME :: PARSE Name
tNAME = P.token S_NAME & mapMaybe \case { T_NAME v -> Just v; _ -> Nothing }

named :: Name -> PARSE ()
named n = void $ P.try $ filter (== n) tNAME

separated :: forall r. PARSE r -> PARSE [r]
separated p = (liftA2 (:) p (many (comma *> p)) <|> pure []) <* optional comma
  where comma = P.token (S_",")
