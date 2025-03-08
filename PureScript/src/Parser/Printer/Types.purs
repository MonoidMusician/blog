module Parser.Printer.Types where

import Prelude

import Ansi.Codes (GraphicsParam)
import Control.Alternative (class Alt, class Plus)
import Control.Apply (lift3)
import Control.Comonad (extract)
import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Compactable (compact, separateDefault)
import Data.Either (Either(..), either, isLeft)
import Data.Filterable (class Compactable, class Filterable, filter, filterMap, partitionDefaultFilter, partitionMapDefault)
import Data.Foldable (fold, foldMap)
import Data.Functor.Compose (Compose(..))
import Data.Lazy (Lazy)
import Data.Lens as Q
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo)
import Data.Newtype (class Newtype, over, un, unwrap)
import Data.Profunctor (class Profunctor, lcmap)
import Data.Rational (Rational)
import Data.Set (Set)
import Data.These (These(..), these)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith)
import Dodo as O
import Parser.Comb as Comb
import Parser.Comb.Comber (Comber, UserError, lift2)
import Parser.Comb.Comber as Comber
import Parser.Comb.Types (Associativity(..))
import Parser.Printer.Juxt (class Subjuxt, class Conjuxt, class Disjuxt, class GuideFlow, CaseTree(..), _Array, _NEA, casesSplit, cleaveCases, summarizeCaseTree, (!!!), (!>), (/!\), (<!), (\!/))
import Parser.Printer.Prec (Prec(..))
import Parser.Selective (casingOn, cmapCaseTree, hoistCaseTree', secondCaseTree)
import Riverdragon.Dragon (Dragon)
import Safe.Coerce (coerce)
import Whitespace (class FromWSF, WS, WSDoc, WithBoundaryF(..), _bddF, _wsDoc, docWS, noBoundaryF, pureWSF, withBoundaryF, wsF)
import Whitespace as WS

-- TODO: standard syntactic categories
newtype Ann = Ann
  { classes :: Array String
  , ansi :: Array GraphicsParam
  , dragon :: Endo (->) Dragon
  }
derive instance newtypeAnn :: Newtype Ann _
derive newtype instance semigroupAnn :: Semigroup Ann
derive newtype instance monoidAnn :: Monoid Ann

newAnn ::
  ( { ansi :: Array GraphicsParam
    , classes :: Array String
    , dragon :: Endo (->) Dragon
    } ->
    { ansi :: Array GraphicsParam
    , classes :: Array String
    , dragon :: Endo (->) Dragon
    }
  ) ->
  Ann
newAnn f = Ann (f mempty)

type Opts = Set String
type RatPrec = Prec (Tuple Rational Associativity)

newtype PrinterParser i o = PrinterParser
  { allOpts :: Opts
  , printer :: PrinterMade i
  , prec :: RatPrec
  , parser :: ParserMade o
  }

type PrinterMade i =
  (O.Doc Ann -> O.Doc Ann) ->
  i ->
  Maybe (Tuple Rational Associativity) ->
  Opts ->
  WSDoc Ann
type ParserMade o = WithBoundaryF InnerParser o
newtype Parsed o = Parsed
  { usedOpts :: Lazy Opts
  , cst :: Opts -> O.Doc Ann
  , ast :: Lazy o
  }
derive instance functorParsed :: Functor Parsed
derive instance newtypeParsed :: Newtype (Parsed o) _
instance applyParsed :: Apply Parsed where
  apply (Parsed r1) (Parsed r2) = Parsed
    { usedOpts: r1.usedOpts <> r2.usedOpts
    , cst: r1.cst <> r2.cst
    , ast: r1.ast <*> r2.ast
    }
instance applicativeParsed :: Applicative Parsed where
  pure a = Parsed { usedOpts: mempty, cst: mempty, ast: pure a }

newtype InnerParser o = InnerParser (Compose Comber Parsed o)
derive instance newtypeInnerParser :: Newtype (InnerParser o) _
derive newtype instance Functor InnerParser
derive newtype instance Apply InnerParser
derive newtype instance Applicative InnerParser
derive newtype instance Alt InnerParser
derive newtype instance Plus InnerParser
instance Compactable InnerParser where
  separate x = separateDefault x
  compact = over InnerParser $ over Compose $ filterMap \(Parsed r) ->
    Parsed <<< r { ast = _ } <<< pure <$> extract r.ast
instance Filterable InnerParser where
  partition f x = partitionDefaultFilter f x
  partitionMap f x = partitionMapDefault f x
  filter f = over InnerParser $ over Compose $ filter \(Parsed r) ->
    f (extract r.ast)
  filterMap f = over InnerParser $ over Compose $ filterMap \(Parsed r) ->
    Parsed <<< r { ast = _ } <<< pure <$> f (extract r.ast)
instance fromWSFInnerParser :: FromWSF InnerParser where
  infixWSF l h r =
    (/\) <$> l <* pureWSF h <*> r
  circumfixWSF h1 a h2 =
    pureWSF h1 *> a <* pureWSF h2
  pureWSF h = InnerParser $ Compose $ Comber.Comber $ Comb.tokensSourceOf (Comb.space (WS.wsProps h)) <#>
    spyWith "ws tokens" (map (either identity identity)) >>> \toks -> Parsed { ast: pure unit, cst: \_ -> O.text (extractWS toks), usedOpts: mempty }
    where
    extractWS :: Array (Either String _) -> String
    extractWS toks =
      let
        surrounding = Array.takeWhile isLeft toks <> Array.reverse (Array.takeWhile isLeft (Array.reverse toks))
      in surrounding # foldMap (either identity mempty)
  neverWSF = empty

applyBoundary :: forall o. ParserMade o -> Compose Comber Parsed o
applyBoundary = coerce (withBoundaryF :: _ -> InnerParser o)

unCompose :: forall o. WithBoundaryF InnerParser o -> WithBoundaryF Comber (Parsed o)
unCompose (WSF ws proof _) = WSF ws (coerce proof <<< map (pure @Parsed)) void
unCompose (EmptyF _) = EmptyF (map absurd)
unCompose (BddF r) = BddF r { wrapped = coerce r.wrapped }

reCompose :: forall o. WithBoundaryF Comber (Parsed o) -> WithBoundaryF InnerParser o
reCompose (WSF ws proof _) = WSF ws (coerce <<< proof <<< void <<< unwrap <<< unwrap) void
reCompose (EmptyF _) = EmptyF (map absurd)
reCompose (BddF r) = BddF r { wrapped = coerce r.wrapped }


type PP io = PrinterParser io io
-- I use `H`/`h` for separators since it is a silent letter and has no other meaning
type H = PP Unit

-- printPrec :: forall i. Maybe (Tuple Rational Associativity) -> PrinterMade i -> PrinterMade i
-- printPrec Nothing = identity
-- printPrec (Just precInner) = \printer parens i precOuter opts ->
--   if precOuter < precInner

setParensPrinter :: forall i. (O.Doc Ann -> O.Doc Ann) -> PrinterMade i -> PrinterMade i
setParensPrinter parens printer _parens i prec opts = printer parens i prec opts

_binn :: forall i1 i2 i3 o1 o2 o3.
  { printer :: PrinterMade i1 -> PrinterMade i2 -> PrinterMade i3
  , prec :: RatPrec -> RatPrec -> RatPrec
  , parser :: ParserMade o1 -> ParserMade o2 -> ParserMade o3
  } ->
  (PrinterParser i1 o1 -> PrinterParser i2 o2 -> PrinterParser i3 o3)
_binn fs (PrinterParser pp1) (PrinterParser pp2) = PrinterParser
  { allOpts: pp1.allOpts <> pp2.allOpts
  , printer: fs.printer pp1.printer pp2.printer
  , parser: fs.parser pp1.parser pp2.parser
  , prec: fs.prec pp1.prec pp2.prec
  }

_binnn :: forall i1 i2 i3 o1 o2 o3.
  { printer :: PrinterMade i1 -> PrinterMade i2 -> PrinterMade i3
  , prec :: RatPrec -> RatPrec -> RatPrec
  , ast :: o1 -> o2 -> o3
  , cst :: O.Doc Ann -> O.Doc Ann -> O.Doc Ann
  } ->
  (PrinterParser i1 o1 -> PrinterParser i2 o2 -> PrinterParser i3 o3)
_binnn fs = _binn
  { printer: fs.printer
  , prec: fs.prec
  , parser: overParser \(Parsed r1) (Parsed r2) -> Parsed
      { usedOpts: r1.usedOpts <> r2.usedOpts
      , ast: lift2 fs.ast r1.ast r2.ast
      , cst: lift2 fs.cst r1.cst r2.cst
      }
  }
  where
  overParser ::
    ( Parsed o1 ->
      Parsed o2 ->
      Parsed o3
    ) ->
    ParserMade o1 -> ParserMade o2 -> ParserMade o3
  overParser f p q = reCompose $ f <$> unCompose p <*> unCompose q

derive instance newtypePrinterParser :: Newtype (PrinterParser i o) _
derive instance profunctorPrinterParser :: Profunctor PrinterParser
derive instance functorPrinterParser :: Functor (PrinterParser i)
instance semigroupPrinterParser :: Semigroup o => Semigroup (PrinterParser i o) where
  append = lift2 append
instance monoidPrinterParser :: Monoid o => Monoid (PrinterParser i o) where
  mempty = pure mempty

-- Constructors and optics

printerParser :: forall i o. (i -> O.Doc Ann) -> Comber o -> PrinterParser i o
printerParser printer parser = PrinterParser
  { allOpts: mempty
  , printer: \_ i _ _ -> docWS $ printer i
  , prec: NoPrec
  , parser: noBoundaryF $ carbonCopy parser
  }

printerParser' :: forall t. (t -> O.Doc Ann) -> Comber t -> PrinterParser t t
printerParser' printer parser = PrinterParser
  { allOpts: mempty
  , printer: \_ i _ _ -> docWS $ printer i
  , prec: NoPrec
  , parser: noBoundaryF $ InnerParser $ Compose $ parser <#> \t -> Parsed
    { usedOpts: mempty
    , cst: \_ -> printer t
    , ast: pure t
    }
  }

_doc :: forall i o. Q.Setter' (PrinterParser i o) (O.Doc Ann)
_doc f = over PrinterParser \pp -> pp
    { printer = \w i p o -> _wsDoc f $ pp.printer w i p o
    , parser = pp.parser # do
        _bddF $ over InnerParser $ over Compose $ map $ over Parsed \r -> r { cst = map f r.cst }
    }

-- Interesting instances

instance conjuxtPrinterParser :: Conjuxt PrinterParser where
  conjuxt0 = mempty
  conjuxt2 = _binnn
    { printer: lift2 \p1 p2 -> lcmap fst p1 <> lcmap snd p2
    , prec: (*)
    , cst: append
    , ast: (/\)
    }

instance disjuxtPrinterParser :: Disjuxt PrinterParser where
  disjuxt0 = PrinterParser
    { allOpts: mempty
    , printer: pure absurd
    , prec: zero
    , parser: empty
    }
  disjuxt2 = _binn
    { printer: lift2 either
    , prec: (+)
    , parser: \p1 p2 -> Left <$> p1 <|> Right <$> p2
    }

instance subjuxtPrinterParser :: Subjuxt PrinterParser where
  subjuxt2 = _binn
    { printer: lift2 \p1 p2 -> these p1 p2 (\u v -> p1 u <> p2 v)
    , prec: \p1 p2 -> p1 * p2 + p1 + p2
    , parser: \p1 p2 -> Both <$> p1 <*> p2 <|> This <$> p1 <|> That <$> p2
    }
  subjuxtSep2 (PrinterParser ux) (PrinterParser sep) (PrinterParser vy) = PrinterParser
    { allOpts: ux.allOpts <> sep.allOpts <> vy.allOpts
    , printer: lift3 (\p1 p2 p3 -> these p1 p3 (\u v -> p1 u <> p2 unit <> p3 v)) ux.printer sep.printer vy.printer
    , prec: ux.prec * sep.prec * vy.prec + ux.prec + vy.prec
    , parser: Both <$> ux.parser <* sep.parser <*> vy.parser <|> This <$> ux.parser <|> That <$> vy.parser
    }

instance guideFlowPrinterParser :: GuideFlow PrinterParser where
  branchCases (PrinterParser pij) orig = PrinterParser
    { allOpts: pij.allOpts
    , printer: map ((>>>) wi) pij.printer
    , prec: pij.prec * caseTreePrec
    , parser: reCompose $ uncurry (*>) <$> casingOn (splitParsed <$> unCompose pij.parser) (secondCaseTree caseTree)
    }
    where
    caseTreePrec = un Additive $ TwoCases (casesSplit orig)
      # summarizeCaseTree \(PrinterParser p) -> Additive p.prec
    Tuple wi caseTree = cleaveCases (TwoCases (casesSplit orig))
      <#> hoistCaseTree' (\(PrinterParser pp) -> unCompose pp.parser)
      <#> cmapCaseTree (extract :: forall j. Lazy j -> j)
    splitParsed :: forall j. Parsed j -> Tuple (Parsed Unit) (Lazy j)
    splitParsed (Parsed p) = Tuple (Parsed (p { ast = pure unit })) (p.ast)

instance applyPrinterParser :: Apply (PrinterParser i) where
  apply = _binnn
    { printer: append
    , prec: (+)
    , cst: append
    , ast: ($)
    }
instance applicativePrinterParser :: Applicative (PrinterParser i) where
  pure a = PrinterParser
    { allOpts: mempty
    , printer: mempty
    , prec: NoPrec
    , parser: pure a
    }
-- | Left-biased!
instance altPrinterParser :: Alt (PrinterParser i) where
  alt = _binn
    { printer: const
    , prec: (+)
    , parser: (<|>)
    }

instance compactablePrinterParser :: Compactable (PrinterParser i) where
  separate x = separateDefault x
  compact = over PrinterParser \r -> r { parser = _ } $
    WS._bddF compact r.parser
instance filterablePrinterParser :: Filterable (PrinterParser i) where
  partition f x = partitionDefaultFilter f x
  partitionMap f x = partitionMapDefault f x
  filterMap f = over PrinterParser \pp -> pp { parser = _ } $
    WS._bddF (filterMap f) pp.parser
  filter f = over PrinterParser \pp -> pp { parser = _ } $
    WS._bddF (filter f) pp.parser

mapEither :: forall i a b. (a -> Either (Array UserError) b) -> PrinterParser i a -> PrinterParser i b
mapEither f = over PrinterParser \pp -> pp { parser = _ } $
  pp.parser # WS._bddF do
    over InnerParser $ over Compose $ Comber.mapEither \(Parsed r) ->
      Parsed <<< r { ast = _ } <<< pure <$> f (extract r.ast)

mapEither_ :: forall i a b. (a -> Either (Array UserError) b) -> PrinterParser i a -> PrinterParser i b
mapEither_ f = over PrinterParser \pp -> pp { parser = _ } $
  pp.parser # WS._bddF do
    over InnerParser $ over Compose $ Comber.mapEither_ \(Parsed r) ->
      Parsed <<< r { ast = _ } <<< pure <$> f (extract r.ast)

guardError :: forall i o. PrinterParser i o -> Array UserError -> (o -> Boolean) -> PrinterParser i o
guardError pp err f = pp # mapEither \v -> if not f v then Left err else Right v

checkError :: forall i o. PrinterParser i o -> Array UserError -> (o -> Boolean) -> PrinterParser i o
checkError pp err f = pp # mapEither_ \v -> if not f v then Left err else Right v

-- | Reproduce the exact source string in the output
-- TODO: parse newlines for O printer ... what about indentation, idk
carbonCopy :: forall o. Comber o -> InnerParser o
carbonCopy = InnerParser <<< Compose <<< Comber.withSourceOf >>> map \(Tuple src o) -> Parsed
  { usedOpts: mempty
  , cst: pure $ O.text src
  , ast: pure o
  }


namedRec :: forall i o. String -> (PrinterParser i o -> PrinterParser i o) -> PrinterParser i o
namedRec name mk = PrinterParser
  let
    pO /\ qO /\ opts /\ prec = Comb.namedRec' name \parserI ->
      let
        made = unwrap $ mk $ PrinterParser
          { allOpts: mempty
          , printer: \w i p o -> made.printer w i p o
          , prec: NoPrec -- TODO: check?
          , parser: noBoundaryF (InnerParser (Compose (Comber.Comber parserI)))
          }
        Compose (Comber.Comber parserO) = applyBoundary made.parser
        printerO = made.printer
      in parserO /\ printerO /\ made.allOpts /\ made.prec
  in { allOpts: opts, prec, printer: qO, parser: noBoundaryF (InnerParser (Compose (Comber.Comber pO))) }

named :: forall i o. String -> PrinterParser i o -> PrinterParser i o
named name pp = namedRec name (const pp)

infixr 2 named as #:
infixr 8 namedRec as #->

token :: String -> PP Unit
token tok = printerParser'
  (const $ O.text tok)
  (void $ Comber.token tok)

flexGroup :: forall i o. PrinterParser i o -> PrinterParser i o
flexGroup = _doc O.flexGroup

indent :: forall i o. PrinterParser i o -> PrinterParser i o
indent = _doc O.indent

flexIndent :: forall i o. PrinterParser i o -> PrinterParser i o
flexIndent = _doc (O.flexGroup <<< O.indent)

rulePrec :: forall i o. Rational -> PrinterParser i o -> PrinterParser i o
rulePrec prec p = p <* PrinterParser
  { allOpts: mempty
  , printer: mempty
  , prec: Prec (Tuple prec NoAssoc)
  , parser: noBoundaryF $ InnerParser $ Compose $ Comber.rulePrec prec (pure (pure unit))
  }

manySepBy :: forall i o. String -> PrinterParser Unit Unit -> PrinterParser i o -> PrinterParser (Array i) (Array o)
manySepBy name s p =
  _Array
  !!! pure unit
  \!/ p /!\ many name (s !> p)

many :: forall i o. String -> PrinterParser i o -> PrinterParser (Array i) (Array o)
many name p = name #-> \more ->
  _Array
  !!! pure unit
  \!/ p /!\ more

many1 :: forall i o. String -> PrinterParser i o -> PrinterParser (NonEmptyArray i) (NonEmptyArray o)
many1 name p =
  _NEA
  !!! p
  /!\ many name p

many_ :: forall i o. SepBy H -> String -> PrinterParser i o -> PrinterParser (Array i) (Array o)
many_ seps name p =
  let { sep, base, before, after } = applySepBy seps in
  _Array
  !!! base
  \!/ before !> p /!\ many name (sep !> p) <! after

many1_ :: forall i o. SepBy1 H -> String -> PrinterParser i o -> PrinterParser (NonEmptyArray i) (NonEmptyArray o)
many1_ NoSep1 name p = many1 name p
many1_ (SepBy1 h before after) name p =
  _NEA
  !!! applySepPref h before
   !> p
  /!\ many name (h !> p)
  <!  applySepPref h after


wsOf :: WS -> PrinterParser Unit Unit
wsOf h = PrinterParser
  { allOpts: mempty
  , prec: NoPrec
  , printer: \_ _ _ _ -> WS.JustWS $ WS.wsRender h
  , parser: wsF h
      -- Compose $ Parsed mempty <$ Comber.wsOf (WS.wsProps h)
      -- carbonCopy $ Comber.wsOf (WS.wsProps h)
  }



data SepBy h
  = NoSep
  | SepBy h SepPref SepPref SepPref
  | BaseSepBy h h SepPref SepPref

derive instance functorSepBy :: Functor SepBy

data SepBy1 h
  = NoSep1
  | SepBy1 h SepPref SepPref

derive instance functorSepBy1 :: Functor SepBy1

data SepPref
  = Require
  | Prefer
  | Allow
  | None

applySepPref :: forall f. Alt f => Applicative f => f Unit -> SepPref -> f Unit
applySepPref s Require = s
applySepPref s Prefer = s <|> pure unit
applySepPref s Allow = pure unit <|> s
applySepPref _ None = pure unit

applySepBy ::
  forall f.
    Applicative f =>
    Alt f =>
  SepBy (f Unit) ->
  { after :: f Unit
  , base :: f Unit
  , before :: f Unit
  , sep :: f Unit
  }
applySepBy NoSep =
  { sep: pure unit
  , base: pure unit
  , before: pure unit
  , after: pure unit
  }
applySepBy (SepBy h base before after) =
  { sep: h
  , base: applySepPref h base
  , before: applySepPref h before
  , after: applySepPref h after
  }
applySepBy (BaseSepBy h base before after) =
  { sep: h
  , base: base
  , before: applySepPref h before
  , after: applySepPref h after
  }
