module PureScript.ToSource where

import Prelude (Unit, Void, absurd, apply, bind, discard, join, map, pure, (#), ($), (<$>), (<<<), (<>))
import PureScript.CST.Types hiding (Type(..), Row(..))

import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import PureScript.CST.Types as CST
import Record as Record
import Tidy.Codegen as TC
import Tidy.Codegen.Class (class ToModuleName, class ToToken, toQualifiedName)
import Tidy.Codegen.Monad as TCG
import Tidy.Codegen.Types (Qualified(..))
import Type.Equality (class TypeEquals, proof)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type CG = TCG.Codegen Void
type CGF f = TCG.Codegen Void (f Void)
type CGE = TCG.Codegen Void (Expr Void)

class ToSource a where
  toSource :: Partial => a -> TCG.Codegen Void (Expr Void)

--------------------------------------------------------------------------------
-- Primitive types                                                            --
--------------------------------------------------------------------------------

instance toSourceVoid :: ToSource Void where
  toSource = absurd

instance toSourceUnit :: ToSource Unit where
  toSource _ = do
    TCG.importOpen "Prelude"
    pure $ TC.exprIdent "unit"

instance toSourceString :: ToSource String where
  toSource = pure <<< TC.exprString

instance toSourceInt :: ToSource Int where
  toSource = pure <<< TC.exprInt

instance toSourceNumber :: ToSource Number where
  toSource = pure <<< TC.exprNumber

instance toSourceBoolean :: ToSource Boolean where
  toSource = pure <<< TC.exprBool

instance toSourceChar :: ToSource Char where
  toSource = pure <<< TC.exprChar

instance toSourceArray :: ToSource a => ToSource (Array a) where
  toSource as = TC.exprArray <$> traverse toSource as

instance toSourceNonEmptyArray :: ToSource a => ToSource (NonEmptyArray a) where
  toSource =
    dataType "Data.Array.NonEmpty.Internal" "NonEmptyArray" case _ of
      NonEmptyArray as -> "NonEmptyArray" /\ [TC.exprArray <$> traverse toSource as]

--------------------------------------------------------------------------------
-- General types                                                              --
--------------------------------------------------------------------------------

instance toSourceMaybe :: ToSource a => ToSource (Maybe a) where
  toSource = dataType "Data.Maybe" "Maybe" case _ of
    Nothing -> "Nothing" /\ []
    Just a -> "Just" /\ [toSource a]

instance toSourceTuple :: (ToSource a, ToSource b) => ToSource (Tuple a b) where
  toSource = dataType "Data.Tuple" "Tuple" case _ of
    Tuple a b -> "Tuple" /\ [toSource a, toSource b]

instance toSourceEither :: (ToSource a, ToSource b) => ToSource (Either a b) where
  toSource = dataType "Data.Either" "Either" case _ of
    Left a -> "Left" /\ [toSource a]
    Right b -> "Right" /\ [toSource b]

--------------------------------------------------------------------------------
-- Row types                                                                  --
--------------------------------------------------------------------------------

instance toSourceVariant ::
  ( RL.RowToList r rl
  , ToSourceRowList rl r
  ) => ToSource (Variant r) where
    toSource v = do
      inj <- TCG.importFrom "Data.Variant" $ TCG.importValue "Variant.inj"
      prx <- TCG.importFrom "Type.Proxy" $ join TCG.importCtor "Variant.Proxy"
      cons /\ arg <- sequence $ toSourceVariantRL (Proxy :: Proxy rl) v
      let
        proxy = TC.exprTyped (TC.exprCtor prx) (TC.typeString cons)
      pure $ TC.exprApp (TC.exprIdent inj) [proxy, arg]

instance toSourceRecord ::
  ( RL.RowToList r rl
  , ToSourceRowList rl r
  ) => ToSource (Record r) where
    toSource v = do
      map TC.exprRecord $ traverse sequence $
        toSourceRecordRL (Proxy :: Proxy rl) v

class ToSourceRowList (rl :: RowList Type) (r :: Row Type) | rl -> r where
  toSourceRecordRL :: Partial => Proxy rl -> Record r -> Array (String /\ CGE)
  toSourceVariantRL :: Partial => Proxy rl -> Variant r -> String /\ CGE

instance toSourceRowNil :: TypeEquals r () => ToSourceRowList RL.Nil r where
  toSourceRecordRL _ _ = []
  toSourceVariantRL _ = Variant.case_ <<< proof

instance toSourceRowCons ::
  ( IsSymbol s
  , ToSource t
  , Row.Cons s t r' r
  , ToSourceRowList rl r'
  ) => ToSourceRowList (RL.Cons s t rl) r where
  toSourceRecordRL _ r = do
    [ reflectSymbol (Proxy :: Proxy s)
      /\ toSource (Record.get (Proxy :: Proxy s) r)
    ] <> toSourceRecordRL (Proxy :: Proxy rl) (unsafeCoerce r :: Record r')
  toSourceVariantRL _ =
    toSourceVariantRL (Proxy :: Proxy rl)
    # Variant.on (Proxy :: Proxy s) \v ->
        reflectSymbol (Proxy :: Proxy s) /\ toSource v

--------------------------------------------------------------------------------
-- CST types                                                                  --
--------------------------------------------------------------------------------

cstTypes = "PureScript.CST.Types" :: String

instance toSourceModuleName :: ToSource ModuleName where
  toSource = newType cstTypes "ModuleName"

instance toSourceComment :: ToSource a => ToSource (Comment a) where
  toSource = dataType cstTypes "Comment" case _ of
    Comment s -> "Comment" /\ [toSource s]
    Space i -> "Space" /\ [toSource i]
    Line l i -> "Line" /\ [toSource l, toSource i]
instance toSourceLineFeed :: ToSource LineFeed where
  toSource = dataType cstTypes "LineFeed" case _ of
    LF -> "LF" /\ []
    CRLF -> "CRLF" /\ []
instance toSourceSourceStyle :: ToSource SourceStyle where
  toSource = dataType cstTypes "SourceStyle" case _ of
    ASCII -> "ASCII" /\ []
    Unicode -> "Unicode" /\ []
instance toSourceIntValue :: ToSource IntValue where
  toSource = dataType cstTypes "IntValue" case _ of
    SmallInt i -> "SmallInt" /\ [toSource i]
    BigInt s -> "BigInt" /\ [toSource s]
    BigHex s -> "BigHex" /\ [toSource s]

instance toSourceToken :: ToSource Token where
  toSource = dataType cstTypes "Token" case _ of
    TokLeftParen -> "TokLeftParen" /\ []
    TokRightParen -> "TokRightParen" /\ []
    TokLeftBrace -> "TokLeftBrace" /\ []
    TokRightBrace -> "TokRightBrace" /\ []
    TokLeftSquare -> "TokLeftSquare" /\ []
    TokRightSquare -> "TokRightSquare" /\ []
    TokLeftArrow s -> "TokLeftArrow" /\ [toSource s]
    TokRightArrow s -> "TokRightArrow" /\ [toSource s]
    TokRightFatArrow s -> "TokRightFatArrow" /\ [toSource s]
    TokDoubleColon s -> "TokDoubleColon" /\ [toSource s]
    TokForall s -> "TokForall" /\ [toSource s]
    TokEquals -> "TokEquals" /\ []
    TokPipe -> "TokPipe" /\ []
    TokTick -> "TokTick" /\ []
    TokDot -> "TokDot" /\ []
    TokComma -> "TokComma" /\ []
    TokUnderscore -> "TokUnderscore" /\ []
    TokBackslash -> "TokBackslash" /\ []
    TokAt -> "TokAt" /\ []
    TokLowerName m s -> "TokLowerName" /\ [toSource m, toSource s]
    TokUpperName m s -> "TokUpperName" /\ [toSource m, toSource s]
    TokOperator m s -> "TokOperator" /\ [toSource m, toSource s]
    TokSymbolName m s -> "TokSymbolName" /\ [toSource m, toSource s]
    TokSymbolArrow s -> "TokSymbolArrow" /\ [toSource s]
    TokHole s -> "TokHole" /\ [toSource s]
    TokChar s v -> "TokChar" /\ [toSource s, toSource v]
    TokString s v -> "TokString" /\ [toSource s, toSource v]
    TokRawString s -> "TokRawString" /\ [toSource s]
    TokInt s v -> "TokInt" /\ [toSource s, toSource v]
    TokNumber s v -> "TokNumber" /\ [toSource s, toSource v]
    TokLayoutStart i -> "TokLayoutStart" /\ [toSource i]
    TokLayoutSep i -> "TokLayoutSep" /\ [toSource i]
    TokLayoutEnd i -> "TokLayoutEnd" /\ [toSource i]

instance toSourceIdent :: ToSource Ident where
  toSource = newType cstTypes "Ident"
instance toSourceProper :: ToSource Proper where
  toSource = newType cstTypes "Proper"
instance toSourceLabel :: ToSource Label where
  toSource = newType cstTypes "Label"
instance toSourceOperator :: ToSource Operator where
  toSource = newType cstTypes "Operator"

instance toSourceName :: ToSource a => ToSource (Name a) where
  toSource = newType cstTypes "Name"
instance toSourceQualifiedName :: ToSource a => ToSource (QualifiedName a) where
  toSource = newType cstTypes "QualifiedName"
instance toSourceWrapped :: ToSource a => ToSource (Wrapped a) where
  toSource = newType cstTypes "Wrapped"
instance toSourceSeparated :: ToSource a => ToSource (Separated a) where
  toSource = newType cstTypes "Separated"
instance toSourceLabeled :: (ToSource a, ToSource b) => ToSource (Labeled a b) where
  toSource = newType cstTypes "Labeled"
instance toSourcePrefixed :: ToSource a => ToSource (Prefixed a) where
  toSource = newType cstTypes "Prefixed"
instance toSourceOneOrDelimited :: ToSource a => ToSource (OneOrDelimited a) where
  toSource = dataType cstTypes "OneOrDelimited" case _ of
    One a -> "One" /\ [toSource a]
    Many as -> "Many" /\ [toSource as]

instance toSourceType :: ToSource e => ToSource (CST.Type e) where
  toSource = dataType cstTypes "Type" case _ of
    CST.TypeVar n -> "TypeVar" /\ [toSource n]
    CST.TypeConstructor n -> "TypeConstructor" /\ [toSource n]
    CST.TypeWildcard t -> "TypeWildcard" /\ [toSource t]
    CST.TypeHole n -> "TypeHole" /\ [toSource n]
    CST.TypeString t v -> "TypeString" /\ [toSource t, toSource v]
    CST.TypeInt m t v -> "TypeInt" /\ [toSource m, toSource t, toSource v]
    CST.TypeRow r -> "TypeRow" /\ [toSource r]
    CST.TypeRecord r -> "TypeRecord" /\ [toSource r]
    CST.TypeForall f b d t -> "TypeForall" /\ [toSource f, toSource b, toSource d, toSource t]
    CST.TypeKinded a b c -> "TypeKinded" /\ [toSource a, toSource b, toSource c]
    CST.TypeApp f a -> "TypeApp" /\ [toSource f, toSource a]
    CST.TypeOp a b -> "TypeOp" /\ [toSource a, toSource b]
    CST.TypeOpName n -> "TypeOpName" /\ [toSource n]
    CST.TypeArrow a t b -> "TypeArrow" /\ [toSource a, toSource t, toSource b]
    CST.TypeArrowName t -> "TypeArrowName" /\ [toSource t]
    CST.TypeConstrained c t a -> "TypeConstrained" /\ [toSource c, toSource t, toSource a]
    CST.TypeParens t -> "TypeParens" /\ [toSource t]
    CST.TypeError e -> "TypeError" /\ [toSource e]

instance toSourceTypeVarBinding :: (ToSource a, ToSource e) => ToSource (TypeVarBinding a e) where
  toSource = dataType cstTypes "TypeVarBinding" case _ of
    TypeVarKinded t -> "TypeVarKinded" /\ [toSource t]
    TypeVarName n -> "TypeVarName" /\ [toSource n]

instance toSourceRow :: ToSource e => ToSource (CST.Row e) where
  toSource = newType cstTypes "Row"
instance toSourceModule :: ToSource e => ToSource (Module e) where
  toSource = newType cstTypes "Module"
instance toSourceModuleHeader :: ToSource e => ToSource (ModuleHeader e) where
  toSource = newType cstTypes "ModuleHeader"
instance toSourceModuleBody :: ToSource e => ToSource (ModuleBody e) where
  toSource = newType cstTypes "ModuleBody"

instance toSourceExport :: ToSource e => ToSource (Export e) where
  toSource = dataType cstTypes "Export" case _ of
    ExportValue n -> "ExportValue" /\ [toSource n]
    ExportOp n -> "ExportOp" /\ [toSource n]
    ExportType n m -> "ExportType" /\ [toSource n, toSource m]
    ExportTypeOp t n -> "ExportTypeOp" /\ [toSource t, toSource n]
    ExportClass t n -> "ExportClass" /\ [toSource t, toSource n]
    ExportModule t n -> "ExportModule" /\ [toSource t, toSource n]
    ExportError e -> "ExportError" /\ [toSource e]

instance toSourceDataMembers :: ToSource DataMembers where
  toSource = dataType cstTypes "DataMembers" case _ of
    DataAll t -> "DataAll" /\ [toSource t]
    DataEnumerated n -> "DataEnumerated" /\ [toSource n]

instance toSourceDeclaration :: ToSource e => ToSource (Declaration e) where
  toSource = dataType cstTypes "Declaration" case _ of
    DeclData h c -> "DeclData" /\ [toSource h, toSource c]
    DeclType h s t -> "DeclType" /\ [toSource h, toSource s, toSource t]
    DeclNewtype h s n t -> "DeclNewtype" /\ [toSource h, toSource s, toSource n, toSource t]
    DeclClass h c -> "DeclClass" /\ [toSource h, toSource c]
    DeclInstanceChain i -> "DeclInstanceChain" /\ [toSource i]
    DeclDerive d q h -> "DeclDerive" /\ [toSource d, toSource q, toSource h]
    DeclKindSignature s n -> "DeclKindSignature" /\ [toSource s, toSource n]
    DeclSignature n -> "DeclSignature" /\ [toSource n]
    DeclValue f -> "DeclValue" /\ [toSource f]
    DeclFixity f -> "DeclFixity" /\ [toSource f]
    DeclForeign s t f -> "DeclForeign" /\ [toSource s, toSource t, toSource f]
    DeclRole s t n r -> "DeclRole" /\ [toSource s, toSource t, toSource n, toSource r]
    DeclError e -> "DeclError" /\ [toSource e]

instance toSourceInstance :: ToSource e => ToSource (Instance e) where
  toSource = newType cstTypes "Instance"

instance toSourceInstanceBinding :: ToSource e => ToSource (InstanceBinding e) where
  toSource = dataType cstTypes "InstanceBinding" case _ of
    InstanceBindingSignature n -> "InstanceBindingSignature" /\ [toSource n]
    InstanceBindingName f -> "InstanceBindingName" /\ [toSource f]

instance toSourceImportDecl :: ToSource e => ToSource (ImportDecl e) where
  toSource = newType cstTypes "ImportDecl"

instance toSourceImport :: ToSource e => ToSource (Import e) where
  toSource = dataType cstTypes "Import" case _ of
    ImportValue n -> "ImportValue" /\ [toSource n]
    ImportOp n -> "ImportOp" /\ [toSource n]
    ImportType n m -> "ImportType" /\ [toSource n, toSource m]
    ImportTypeOp s n -> "ImportTypeOp" /\ [toSource s, toSource n]
    ImportClass s n -> "ImportClass" /\ [toSource s, toSource n]
    ImportError e -> "ImportError" /\ [toSource e]

instance toSourceDataCtor :: ToSource e => ToSource (DataCtor e) where
  toSource = newType cstTypes "DataCtor"

instance toSourceClassFundep :: ToSource ClassFundep where
  toSource = dataType cstTypes "ClassFundep" case _ of
    FundepDetermined s i -> "FundepDetermined" /\ [toSource s, toSource i]
    FundepDetermines i s j -> "FundepDetermines" /\ [toSource i, toSource s, toSource j]

instance toSourceFixity :: ToSource Fixity where
  toSource = dataType cstTypes "Fixity" case _ of
    Infix -> "Infix" /\ []
    Infixl -> "Infixl" /\ []
    Infixr -> "Infixr" /\ []

instance toSourceFixityOp :: ToSource FixityOp where
  toSource = dataType cstTypes "FixityOp" case _ of
    FixityValue n s o -> "FixityValue" /\ [toSource n, toSource s, toSource o]
    FixityType s n t o -> "FixityType" /\ [toSource s, toSource n, toSource t, toSource o]

instance toSourceGuarded :: ToSource e => ToSource (Guarded e) where
  toSource = dataType cstTypes "Guarded" case _ of
    Unconditional s w -> "Unconditional" /\ [toSource s, toSource w]
    Guarded g -> "Guarded" /\ [toSource g]

instance toSourceGuardedExpr :: ToSource e => ToSource (GuardedExpr e) where
  toSource = newType cstTypes "GuardedExpr"

instance toSourcePatternGuard :: ToSource e => ToSource (PatternGuard e) where
  toSource = newType cstTypes "PatternGuard"

instance toSourceForeign :: ToSource e => ToSource (Foreign e) where
  toSource = dataType cstTypes "Foreign" case _ of
    ForeignValue n -> "ForeignValue" /\ [toSource n]
    ForeignData s n -> "ForeignData" /\ [toSource s, toSource n]
    ForeignKind s n -> "ForeignKind" /\ [toSource s, toSource n]

instance toSourceRole :: ToSource Role where
  toSource = dataType cstTypes "Role" case _ of
    Nominal -> "Nominal" /\ []
    Representational -> "Representational" /\ []
    Phantom -> "Phantom" /\ []

instance toSourceExpr :: ToSource e => ToSource (Expr e) where
  toSource = dataType cstTypes "Expr" case _ of
    ExprHole n -> "ExprHole" /\ [toSource n]
    ExprSection s -> "ExprSection" /\ [toSource s]
    ExprIdent n -> "ExprIdent" /\ [toSource n]
    ExprConstructor n -> "ExprConstructor" /\ [toSource n]
    ExprBoolean s v -> "ExprBoolean" /\ [toSource s, toSource v]
    ExprChar s v -> "ExprChar" /\ [toSource s, toSource v]
    ExprString s v -> "ExprString" /\ [toSource s, toSource v]
    ExprInt s v -> "ExprInt" /\ [toSource s, toSource v]
    ExprNumber s v -> "ExprNumber" /\ [toSource s, toSource v]
    ExprArray e -> "ExprArray" /\ [toSource e]
    ExprRecord e -> "ExprRecord" /\ [toSource e]
    ExprParens e -> "ExprParens" /\ [toSource e]
    ExprTyped e s t -> "ExprTyped" /\ [toSource e, toSource s, toSource t]
    ExprInfix e s -> "ExprInfix" /\ [toSource e, toSource s]
    ExprOp e s -> "ExprOp" /\ [toSource e, toSource s]
    ExprOpName n -> "ExprOpName" /\ [toSource n]
    ExprNegate s e -> "ExprNegate" /\ [toSource s, toSource e]
    ExprRecordAccessor e -> "ExprRecordAccessor" /\ [toSource e]
    ExprRecordUpdate e s -> "ExprRecordUpdate" /\ [toSource e, toSource s]
    ExprApp e s -> "ExprApp" /\ [toSource e, toSource s]
    ExprLambda e -> "ExprLambda" /\ [toSource e]
    ExprIf e -> "ExprIf" /\ [toSource e]
    ExprCase e -> "ExprCase" /\ [toSource e]
    ExprLet e -> "ExprLet" /\ [toSource e]
    ExprDo e -> "ExprDo" /\ [toSource e]
    ExprAdo e -> "ExprAdo" /\ [toSource e]
    ExprError e -> "ExprError" /\ [toSource e]

instance toSourceAppSpine :: (ToSource (f e), ToSource e) => ToSource (AppSpine f e) where
  toSource = dataType cstTypes "AppSpine" case _ of
    AppType s t -> "AppType" /\ [toSource s, toSource t]
    AppTerm f -> "AppTerm" /\ [toSource f]

instance toSourceRecordLabeled :: ToSource a => ToSource (RecordLabeled a) where
  toSource = dataType cstTypes "RecordLabeled" case _ of
    RecordPun n -> "RecordPun" /\ [toSource n]
    RecordField n s a -> "RecordField" /\ [toSource n, toSource s, toSource a]

instance toSourceRecordUpdate :: ToSource e => ToSource (RecordUpdate e) where
  toSource = dataType cstTypes "RecordUpdate" case _ of
    RecordUpdateLeaf n s e -> "RecordUpdateLeaf" /\ [toSource n, toSource s, toSource e]
    RecordUpdateBranch n u -> "RecordUpdateBranch" /\ [toSource n, toSource u]

instance toSourceWhere :: ToSource e => ToSource (Where e) where
  toSource = newType cstTypes "Where"

instance toSourceLetBinding :: ToSource e => ToSource (LetBinding e) where
  toSource = dataType cstTypes "LetBinding" case _ of
    LetBindingSignature n -> "LetBindingSignature" /\ [toSource n]
    LetBindingName f -> "LetBindingName" /\ [toSource f]
    LetBindingPattern b s w  -> "LetBindingPattern" /\ [toSource b, toSource s, toSource w]
    LetBindingError e -> "LetBindingError" /\ [toSource e]

instance toSourceDoStatement :: ToSource e => ToSource (DoStatement e) where
  toSource = dataType cstTypes "DoStatement" case _ of
    DoLet s e -> "DoLet" /\ [toSource s, toSource e]
    DoDiscard e -> "DoDiscard" /\ [toSource e]
    DoBind b s e -> "DoBind" /\ [toSource b, toSource s, toSource e]
    DoError e -> "DoError" /\ [toSource e]

instance toSourceBinder :: ToSource e => ToSource (Binder e) where
  toSource = dataType cstTypes "Binder" case _ of
    BinderWildcard s -> "BinderWildcard" /\ [toSource s]
    BinderVar n -> "BinderVar" /\ [toSource n]
    BinderNamed n s b -> "BinderNamed" /\ [toSource n, toSource s, toSource b]
    BinderConstructor n b -> "BinderConstructor" /\ [toSource n, toSource b]
    BinderBoolean s v -> "BinderBoolean" /\ [toSource s, toSource v]
    BinderChar s v -> "BinderChar" /\ [toSource s, toSource v]
    BinderString s v -> "BinderString" /\ [toSource s, toSource v]
    BinderInt m s v -> "BinderInt" /\ [toSource m, toSource s, toSource v]
    BinderNumber m s v -> "BinderNumber" /\ [toSource m, toSource s, toSource v]
    BinderArray b -> "BinderArray" /\ [toSource b]
    BinderRecord b -> "BinderRecord" /\ [toSource b]
    BinderParens b -> "BinderParens" /\ [toSource b]
    BinderTyped b s t -> "BinderTyped" /\ [toSource b, toSource s, toSource t]
    BinderOp b s -> "BinderOp" /\ [toSource b, toSource s]
    BinderError e -> "BinderError" /\ [toSource e]

--------------------------------------------------------------------------------
-- Helpers                                                                    --
--------------------------------------------------------------------------------

dataType ::
  forall adt mod name proper.
    ToModuleName mod =>
    ToToken name (Qualified Proper) =>
    ToToken proper Proper =>
    Partial =>
  mod -> name -> (adt -> proper /\ Array CGE) -> adt -> CGE
dataType mod tyName ctors value = ado
  QualifiedName { module: mod' } <- TCG.importFrom mod $ TCG.importTypeAll tyName
  ctor /\ args <- traverse sequence (ctors value)
  in TC.exprApp (TC.exprCtor (toQualifiedName (Qualified mod' ctor) :: QualifiedName Proper)) args

newType' ::
  forall adt wrapped mod name proper.
    ToModuleName mod =>
    ToToken name (Qualified Proper) =>
    ToToken proper Proper =>
    Newtype adt wrapped =>
    ToSource wrapped =>
    Partial =>
  mod -> name -> proper -> adt -> CGE
newType' mod tyName ctor = dataType mod tyName do
  wrapped <- unwrap
  pure $ ctor /\ [toSource wrapped]

newType ::
  forall adt wrapped mod proper.
    ToModuleName mod =>
    ToToken proper (Qualified Proper) =>
    ToToken proper Proper =>
    Newtype adt wrapped =>
    ToSource wrapped =>
    Partial =>
  mod -> proper -> adt -> CGE
newType mod ctor = newType' mod ctor ctor
