module Parser.Lexing where

import Prelude

import Control.Alternative (guard, (<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Plus (empty)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bifoldMap, ltraverse)
import Data.Either (Either(..), either, hush, isLeft, isRight, note)
import Data.Either.Nested (type (\/))
import Data.Filterable (partitionMap)
import Data.Foldable (class Foldable, foldMap, foldr, oneOfMap, or, product, sum)
import Data.HeytingAlgebra (ff)
import Data.Lazy (force)
import Data.Lens (traverseOf)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over2, unwrap)
import Data.Semigroup.Foldable (maximum)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.String.CodeUnits as SCU
import Data.String.Regex (Regex)
import Data.String.Regex as Re
import Data.String.Regex as Regex
import Data.String.Regex.Flags (dotAll, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Parser.Algorithms (indexStates)
import Parser.Comb.Types (Associativity(..), LogicParts(..), Option, Options(..), PartialResult(..), applyLogic)
import Parser.Printer.JSON (class AutoJSON)
import Parser.Printer.JSON as C
import Parser.Proto (Stack(..), topOf)
import Parser.Types (Fragment, ICST(..), OrEOF(..), Part(..), ShiftReduce(..), State(..), StateInfo, States(..), Zipper(..), decide, decisionUnique, filterSR, filterSR', isInterTerminal, unInterTerminal)
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Whitespace (ParseWS, mkParseWS, unParseWS)

data Scanning i = Scanning Int i
derive instance functorScanning :: Functor Scanning
derive instance foldableScanning :: Foldable Scanning
derive instance traversableScanning :: Traversable Scanning

newtype Similar a b = Similar (Either a b)
infixr 9 type Similar as ~
derive newtype instance eqSimilar :: (Eq a, Eq b) => Eq (Similar a b)
derive newtype instance ordSimilar :: (Ord a, Ord b) => Ord (Similar a b)
derive newtype instance showSimilar :: (Show a, Show b) => Show (Similar a b)
derive instance newtypeSimilar :: Newtype (Similar a b) _
instance (AutoJSON a reg, AutoJSON b reg) => AutoJSON (Similar a b) reg where autoJ = C._N C.autoJ

-- | Typeclass for generic length of things
-- |
-- | `len mempty == 0`
class Len i where
  len :: i -> Int

instance lenString :: Len String where
  len = SCU.length -- This should be safe? since we will not be comparing heteregeneous strings?

instance lenArray :: Len (Array i) where
  len = Array.length

instance lenList :: Len (List i) where
  len = List.length

instance lenScanning :: Len i => Len (Scanning i) where
  len (Scanning idx i) = len i - idx

instance lenTuple :: (Len i1, Len i2) => Len (Tuple i1 i2) where
  len (Tuple i1 i2) = len i1 + len i2

instance lenOrEOF :: Len i => Len (OrEOF i) where
  len EOF = 0
  len (Continue i) = len i + 1

-- | Typeclass for recognizing tokens based on categories.
-- |
-- | Law: if you recognized it, you must unrecognize it, and vice versa.
class (Token cat o, Len i) <= Tokenize cat i o | cat -> o where
  recognize :: cat -> i -> Maybe (o /\ i)
  unrecognize :: cat -> o -> Maybe (i -> i)

class Token cat o | cat -> o where
  rerecognize :: cat -> o -> Boolean

class SquintToken cat o | cat -> o where
  squintAt :: o -> Maybe cat

instance tokenizeString :: Tokenize String String String where
  recognize prefix current =
    String.stripPrefix (String.Pattern prefix) current
      <#> Tuple prefix
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (recognized <> _)

instance tokenString :: Token String String where
  rerecognize = eq

instance tokenizeChar :: Tokenize Char String Char where
  recognize c current = case CU.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (CU.singleton recognized <> _)

instance tokenChar :: Token Char Char where
  rerecognize = eq

instance tokenizeCodePoint :: Tokenize CodePoint String CodePoint where
  recognize c current = case String.uncons current of
    Just { head, tail } | head == c -> pure (Tuple head tail)
    _ -> empty
  unrecognize prefix recognized =
    guard (prefix == recognized) $> (String.singleton recognized <> _)

instance tokenCodePoint :: Token CodePoint CodePoint where
  rerecognize = eq

wsRawr :: ParseWS -> Maybe Rawr
wsRawr = unParseWS >>> case _ of
  { allowed_newline: false, allowed_space: false, required: true } ->
    Nothing
  { allowed_newline: true, allowed_space: true, required: true } ->
    Just $ rawrs.rA
  { allowed_newline: true, allowed_space: false, required: true } ->
    Just $ rawrs.rN
  { allowed_newline: false, allowed_space: true, required: true } ->
    Just $ rawrs.rS
  { allowed_newline: true, allowed_space: true, required: false } ->
    Just $ rawrs.ra
  { allowed_newline: true, allowed_space: false, required: false } ->
    Just $ rawrs.rn
  { allowed_newline: false, allowed_space: true, required: false } ->
    Just $ rawrs.rs
  { allowed_newline: false, allowed_space: false, required: false } ->
    Just $ rawrs.none
  where
  rawrs =
    { rA: rawr "[\\s]+"
    , rN: rawr "[\\n]+"
    , rS: rawr "[ ]+"
    , ra: rawr "[\\s]*"
    , rn: rawr "[\\n]*"
    , rs: rawr "[ ]*"
    , none: rawr ""
    }

instance tokenizeWS :: Tokenize ParseWS String String where
  recognize ws rest = do
    r <- wsRawr ws
    recognize r rest
  unrecognize ws recognized =
    guard (rerecognize ws recognized) $> \rest -> recognized <> rest
instance tokenizeWSOrEOF :: Tokenize ParseWS (OrEOF String) String where
  recognize _ EOF = Nothing
  recognize ws (Continue rest) = do
    r <- wsRawr ws
    recognize r rest <#> map Continue
  unrecognize ws recognized =
    guard (rerecognize ws recognized) $> case _ of
      EOF -> Continue recognized
      Continue rest -> Continue (recognized <> rest)
instance tokenWS :: Token ParseWS String where
  rerecognize ws matched = or do
    r <- wsRawr ws
    pure (rerecognize r matched)
instance squintTokenWS :: SquintToken ParseWS String where
  squintAt "" = Just $ mkParseWS ff
  squintAt s = oneOfMap squintAs
    [ { allowed_newline: true, allowed_space: false, required: true }
    , { allowed_newline: false, allowed_space: true, required: true }
    , { allowed_newline: true, allowed_space: true, required: true }
    ]
    where
    squintAs v = mkParseWS v <$ guard (rerecognize (mkParseWS v) s)

class ToString s where
  toString :: s -> String

instance toStringChar :: ToString Char where
  toString = CU.singleton

instance toStringCodePoint :: ToString CodePoint where
  toString = String.singleton

instance toStringString :: ToString String where
  toString = identity

class FromString s where
  fromString :: String -> s

instance fromStringString :: FromString String where
  fromString = identity


data Rawr = Rawr Regex String
instance showRawr :: Show Rawr where
  show (Rawr _ r) = r
instance eqRaw :: Eq Rawr where
  eq (Rawr _ r1) (Rawr _ r2) = r1 == r2
instance ordRaw :: Ord Rawr where
  compare (Rawr _ r1) (Rawr _ r2) = r1 `compare` r2
rawr :: String -> Rawr
rawr source =
  let r = unsafeRegex ("^(?:" <> source <> ")") $ unicode <> dotAll
  in Rawr r (show r)
unRawr :: Rawr -> String
unRawr (Rawr re _) = fromMaybe (Re.source re) do
  x <- String.stripPrefix (String.Pattern "^(?:") (Re.source re)
  y <- String.stripSuffix (String.Pattern ")") x
  pure y

instance C.TryCached Rawr reg => AutoJSON Rawr reg where
  autoJ = C.tryCached $ C.string # C.prismatic "Rawr" unRawr
    \source -> hush ado
      r <- Re.regex ("^(?:" <> source <> ")") $ unicode <> dotAll
      in Rawr r (show r)

-- TODO: this isn't right:
-- - doesn't handle lookaround since we dropped the start of the string
-- - I think the behavior of flags on the regex will do weird things
-- - fucking terrible API
-- - why does every high-level regex API suck ;.;
instance tokenizeRawr :: Tokenize Rawr String String where
  recognize (Rawr re _) current = do
    groups <- Regex.match re current
    matched <- NEA.head groups
    pure (Tuple matched (CU.drop (CU.length matched) current))
  unrecognize re recognized =
    guard (recognize re recognized == Just (Tuple recognized mempty)) $>
      (recognized <> _)

instance tokenRawr :: Token Rawr String where
  rerecognize re recognized =
    recognize re recognized == Just (Tuple recognized mempty)

instance tokenizeOrEOF :: (Monoid i, Tokenize cat i o) => Tokenize (OrEOF cat) (OrEOF i) (OrEOF o) where
  recognize EOF (Continue i) | len i == 0 = Just (EOF /\ EOF)
  recognize (Continue cat) (Continue i) = bimap Continue Continue <$> recognize cat i
  recognize _ _ = Nothing
  unrecognize EOF EOF = Just (const (Continue mempty))
  unrecognize (Continue l) (Continue r) =
    unrecognize l r <#> case _, _ of
      p, Continue i -> Continue (p i)
      _, i -> i
  unrecognize _ _ = Nothing

instance tokenOrEOF :: (Token cat o) => Token (OrEOF cat) (OrEOF o) where
  rerecognize EOF EOF = true
  rerecognize (Continue l) (Continue r) = rerecognize l r
  rerecognize _ _ = false

-- else instance tokenizeOrEOFPeek :: (Monoid i, Tokenize cat i o) => Tokenize (OrEOF cat) i (OrEOF o) where
--   recognize EOF i | len i == 0 = Just (EOF /\ i)
--   recognize (Continue cat) i = lmap Continue <$> recognize cat i
--   recognize _ _ = Nothing
--   unrecognize EOF EOF = Just (const mempty)
--   unrecognize (Continue l) (Continue r) = unrecognize l r
--   unrecognize _ _ = Nothing

instance tokenizeEither ::
  ( Tokenize cat1 i o1
  , Tokenize cat2 i o2
  ) => Tokenize (cat1 \/ cat2) i (o1 \/ o2) where
    recognize (Left prefix) current =
      lmap Left <$> recognize prefix current
    recognize (Right prefix) current =
      lmap Right <$> recognize prefix current
    unrecognize (Left l) (Left r) = unrecognize l r
    unrecognize (Right l) (Right r) = unrecognize l r
    unrecognize _ _ = Nothing

instance tokenEither ::
  ( Token cat1 o1
  , Token cat2 o2
   ) => Token (cat1 \/ cat2) (o1 \/ o2) where
    rerecognize (Left l) (Left r) = rerecognize l r
    rerecognize (Right l) (Right r) = rerecognize l r
    rerecognize _ _ = false

instance tokenizeSimilar ::
  ( Tokenize cat1 i o
  , Tokenize cat2 i o
  ) => Tokenize (Similar cat1 cat2) i o where
    recognize (Similar e) = either recognize recognize e
    unrecognize (Similar e) = either unrecognize unrecognize e

instance tokenSimilar ::
  ( Token cat1 o
  , Token cat2 o
  ) => Token (Similar cat1 cat2) o where
    rerecognize (Similar e) = either rerecognize rerecognize e

instance tokenizeTuple ::
  ( Tokenize cat1 i1 o1
  , Tokenize cat2 i2 o2
  ) => Tokenize (cat1 /\ cat2) (i1 /\ i2) (o1 /\ o2) where
    recognize (cat1 /\ cat2) (i1 /\ i2) = ado
      o1 /\ j1 <- recognize cat1 i1
      o2 /\ j2 <- recognize cat2 i2
      in (o1 /\ o2) /\ (j1 /\ j2)
    unrecognize (l1 /\ l2) (r1 /\ r2) = ado
      p1 <- unrecognize l1 r1
      p2 <- unrecognize l2 r2
      in \(i1 /\ i2) -> p1 i1 /\ p2 i2

instance tokenTuple ::
  ( Token cat1 o1
  , Token cat2 o2
  ) => Token (cat1 /\ cat2) (o1 /\ o2) where
    rerecognize (l1 /\ l2) (r1 /\ r2) = rerecognize l1 r1 && rerecognize l2 r2


noteR :: forall e a. Maybe a -> e -> Either e a
noteR = flip note

infixr 9 noteR as ??

lmapR :: forall e e' a. Either e a -> (e -> e') -> Either e' a
lmapR x f = x # lmap f

infixr 9 lmapR as ?

-- | Prioritize matches.
type Best err s r cat i o =
  forall space air.
  NonEmptyArray (ShiftReduce s r /\ space /\ air /\ cat /\ o /\ i) ->
  Either (Array err) (Either s r /\ space /\ air /\ cat /\ o /\ i)

prioritize :: forall a score. Ord score => (a -> score) -> NonEmptyArray a -> NonEmptyArray a
prioritize f as =
  let
    scored = (Tuple <*> f) <$> as
    scores = snd <$> scored
    winner = maximum scores
    winners = fst <$> NEA.filter (snd >>> eq winner) scored
  in fromMaybe as $ NEA.fromArray winners

guessBest :: forall err s r cat i o. Best err s r cat i o
guessBest options = note [] case NEA.toArray options of
  [ sr /\ d ] -> do
    guard (decisionUnique sr) *> decide sr <#> (_ /\ d)
  _ -> Nothing

longest :: forall err s r cat i o. Len i => Best err s r cat i o
longest = guessBest <<< prioritize \(_ /\ _ /\ _ /\ _ /\ _ /\ i) -> negate (len i)

bestRegexOrString :: forall err s r. Best err s r (OrEOF (Similar String Rawr)) (OrEOF String) (OrEOF String)
bestRegexOrString options = options # guessBest <<< prioritize case _ of
  (_ /\ _ /\ _ /\ (Continue (Similar cat)) /\ _ /\ Continue i) -> Just $ Tuple (isLeft cat) (negate (len i))
  _ -> Nothing

longestRegexOrString :: forall err s r. Best err s r (OrEOF (Similar String Rawr)) (OrEOF String) (OrEOF String)
longestRegexOrString options = options # guessBest <<< prioritize case _ of
  (_ /\ _ /\ _ /\ (Continue (Similar cat)) /\ _ /\ Continue i) -> Just $ Tuple (negate (len i)) (isLeft cat)
  _ -> Nothing

lazyBest :: forall err s r. Best err s r (OrEOF (Similar String Rawr)) (OrEOF String) (OrEOF String)
lazyBest = note [] <<< ltraverse decide <<< NEA.head <<< prioritize case _ of
  (_ /\ _ /\ _ /\ (Continue (Similar cat)) /\ _ /\ Continue i) -> Just $ Tuple (isLeft cat) (negate (len i))
  _ -> Nothing


-- | Precedence, like Happy in Haskell
newtype ResolvePrec m tok r = ResolvePrec
  ( forall s' r'.
    tok ->
    -- You are only allowed to project information out of the contents, in order
    -- to filter them out. You may not change their values in the result.
    (r' -> r) ->
    ShiftReduce s' r' ->
    m (ShiftReduce s' r')
  )

screenPrecedence ::
  forall m s r space air cat i o.
    Applicative m =>
  ResolvePrec m cat r ->
  NonEmptyArray (ShiftReduce s r /\ space /\ air /\ cat /\ o /\ i) ->
  m (NonEmptyArray (ShiftReduce s r /\ space /\ air /\ cat /\ o /\ i))
screenPrecedence (ResolvePrec f) = traverse \(sr /\ space /\ air /\ cat /\ oi) ->
  f cat identity sr <#> \sr' -> sr' /\ space /\ air /\ cat /\ oi

applyPrecedence ::
  forall m s space nt r tok.
    Applicative m =>
  ResolvePrec m tok (Fragment space nt tok /\ nt /\ r) ->
  States s space nt r tok ->
  m (States s space nt r tok)
applyPrecedence (ResolvePrec prec) =
  traverseOf _Newtype $ traverse $
    traverseOf (prop (Proxy :: Proxy "advance")) $
      traverseWithIndex \tok ->
        traverse $ prec tok identity

-- -- | Resolve precedence like Happy does
happyPrecedence' ::
  forall m measure space nt r tok.
    Monad m =>
  (measure -> Associativity) ->
  -- Disqualifies the right option when returns `Just GT`.
  -- Or depending on associativity if `Just EQ`.
  (measure -> measure -> Maybe Ordering) ->
  (tok -> Maybe measure) ->
  (nt /\ r -> Maybe measure) ->
  ResolvePrec m tok (Fragment space nt tok /\ nt /\ r)
happyPrecedence' assoc cmp measureTok measureRule =
  ResolvePrec \tok getR sr' -> pure
    let
      cmpMaybe (Just l) (Just r) = cmp l r
      cmpMaybe _ _ = Nothing

      lastToken = Array.reverse >>> Array.findMap case _ of
        Terminal t -> Just t
        NonTerminal _ -> Nothing
        InterTerminal _ -> Nothing

      measureS = measureTok tok
      measureR = getR >>> \(fragment /\ ruleName) ->
        measureRule ruleName <|>
          (lastToken fragment >>= measureTok)

      measuredSR =
        bimap
          (\s' -> (true /\ measureS) /\ s')
          (\r' -> (false /\ measureR r') /\ r')
          sr'

      measured = measuredSR # bifoldMap
        (fst >>> Array.singleton)
        (fst >>> Array.singleton)

      disqualify (a /\ m) = not $ measured # Array.any
        \(a' /\ m') -> case cmpMaybe m' m of
          Just GT -> true
          Just EQ -> case assoc <$> m, assoc <$> m', a, a' of
            -- If the token is left-associative, then reduce
            -- If the token is right-associative, then shift
            Just AssocL, _, true, false -> true
            _, Just AssocR, false, true -> true
            _, _, _, _ -> false
          _ -> false

      filtered = measuredSR # filterSR
        (fst >>> disqualify)
        (fst >>> disqualify)
    in bimap snd snd (fromMaybe measuredSR filtered)

happyPrecedenceOrd ::
  forall m measure space nt r tok.
    Monad m =>
    Ord measure =>
  (tok -> Maybe (measure /\ Associativity)) ->
  (nt /\ r -> Maybe measure) ->
  ResolvePrec m tok (Fragment space nt tok /\ nt /\ r)
happyPrecedenceOrd measureTok measureRule =
  happyPrecedence' snd
    do \a b -> Just (compare (fst a) (fst b))
    do \tok -> measureTok tok
    do \rule -> measureRule rule <#> (_ /\ NoAssoc)

addSpine :: forall air r tok. Array (Tuple (Array (ICST air r tok)) r) -> Array (ICST air r tok) -> Array (ICST air r tok)
addSpine = flip $ foldr (\(Tuple prev r) children -> prev <> [IBranch r children])

stillAccept ::
  forall rec err space air nt r cat o.
  rec ->
  Option rec err space air nt r cat o ->
  ICST air (nt /\ r) o ->
  Either (Array err) (Option rec err space air nt r cat o)
stillAccept rec opt@{ logicParts: LogicParts { necessary } } shift =
  let advanced = opt.advanced <> [shift] in
  map (opt { advanced = advanced, logicParts = _ } <<< LogicParts <<< { necessary: _ }) $
    sequence $ necessary # Array.mapMaybe \part -> sequence case unit of
      _ | Array.length advanced < part.start -> Right (Just part)
      _ | Failed e <- applyLogic part.logic rec ((addSpine part.parents (Array.drop part.start advanced))) -> Left e
      _ | part.start + part.length >= Array.length advanced -> Right Nothing
      _ -> Right (Just part)

advanceAccepting ::
  forall rec err space air nt r cat o.
    Semiring space =>
    Eq space =>
    Eq cat =>
    Eq nt =>
    Eq r =>
    Token space air =>
  rec ->
  Options rec err space air nt r cat o ->
  Part Void (air /\ ICST air (nt /\ r) o) (space /\ air /\ cat /\ o) ->
  Either (Array err) (Options rec err space air nt r cat o)
advanceAccepting rec (Options options) shift = bimap (foldMap snd) Options $ options # someSucceed' advanceAccepting'
  where
  advanceAccepting' option@{ advanced } =
    case option.rule !! Array.length advanced of
      Just head ->
        case head, shift of
          Terminal cat', Terminal (space /\ air /\ cat /\ o) | cat == cat' ->
            stillAccept rec option (ILeaf o)
          NonTerminal (_ /\ nt'), NonTerminal (_ /\ b@(IBranch (nt /\ _) _)) | nt' == nt ->
            stillAccept rec option b
          InterTerminal expected, Terminal (space /\ air /\ _ /\ _o)
            | rerecognize expected air ->
            advanceAccepting' option { advanced = Array.snoc advanced (IAir air) } -- FIXME
          InterTerminal expected, NonTerminal (air /\ _)
            | rerecognize expected air ->
            advanceAccepting' option { advanced = Array.snoc advanced (IAir air) } -- FIXME???
          _, InterTerminal vd -> absurd vd
          _, _ -> Left [] -- this is totally okay, just means some other rule matched
      Nothing | Array.length advanced == Array.length option.rule -> -- ??
        Right option
      _ -> Left [] -- this is bad, means we advanced too much

closeA1 ::
  forall rec err space air nt r cat o.
    Eq air =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Eq o =>
  Options rec err space air nt r cat o -> Options rec err space air nt r cat o
closeA1 (Options items) = acceptMore (Options items) $ items # foldMap closeAItem

pushLogicParts ::
  forall rec err space air nt r cat o.
  Option rec err space air nt r cat o ->
  Option rec err space air nt r cat o ->
  LogicParts err air (nt /\ r) o rec
pushLogicParts parent@{ logicParts: LogicParts { necessary } } child =
  LogicParts
    { necessary: necessary <#> \n ->
      { start: 0
      , length: Array.length child.rule
      , logic: n.logic
      , parents: n.parents <> [Tuple parent.advanced (Tuple child.pName child.rName)]
      }
    }

closeAItem ::
  forall rec err space air nt r cat o.
    Ord nt =>
    Eq r =>
    Ord cat =>
    Eq o =>
  Option rec err space air nt r cat o ->
  Options rec err space air nt r cat o
closeAItem parent =
  case parent.rule !! Array.length parent.advanced of
    Just (NonTerminal (opts /\ _)) ->
      Options $ unwrap (force opts) <#> \opt -> opt
        { logicParts = opt.logicParts <> pushLogicParts parent opt
        }
    Just (InterTerminal _) ->
      case parent.rule # Array.drop (Array.length parent.advanced + 1) # Array.dropWhile isInterTerminal # Array.head of
        Just (NonTerminal (opts /\ _)) ->
          Options $ unwrap (force opts) <#> \opt -> opt
            { logicParts = opt.logicParts <> pushLogicParts parent opt
            }
        _ -> mempty
    _ -> mempty

closeA ::
  forall rec err space air nt r cat o.
    Eq air =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Eq o =>
  Options rec err space air nt r cat o -> Options rec err space air nt r cat o
closeA items =
  let items' = closeA1 items
  in if Array.length (unwrap items') == Array.length (unwrap items) then items else closeA items'

acceptMore ::
  forall rec err space air nt r cat o.
    Eq air =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Eq o =>
  Options rec err space air nt r cat o -> Options rec err space air nt r cat o -> Options rec err space air nt r cat o
acceptMore = over2 Options $
  let
    comp a b =
      a.pName == b.pName && a.rName == b.rName &&
        a.logicParts == b.logicParts
  in \as bs -> Array.nubByEq comp (as <> bs)
  -- in \as bs -> as <> do
  --   bs # Array.filter \b ->
  --     isNothing $ as # Array.find \a -> comp a b

subsume :: forall space. Semiring space => Eq space => space -> space -> Boolean
subsume lesser greater = lesser + greater == greater

type ContextStack s rec err space air nt r cat o =
  Stack (Options rec err space air nt r cat o /\ s) (ICST air (nt /\ r) o)

type Poss s space air nt r cat i o = ShiftReduce s (Fragment space nt cat /\ nt /\ r) /\ space /\ air /\ cat /\ o /\ i

nPossibilities :: forall s space air nt r cat i o. NonEmptyArray (Poss s space air nt r cat i o) -> Int
nPossibilities = sum <<< map case _ of
  Shift _ /\ _ -> 1
  Reduces rs /\ _ -> NEA.length rs
  ShiftReduces _ rs /\ _ -> 1 + NEA.length rs

data FailedStack err s rec space air nt r cat i o
  = FailedStack
    { states :: States s space nt r cat
    , lookupState :: s -> Maybe (StateInfo s space nt r cat)
    , initialInput :: i
    , failedStack :: ContextStack s rec err space air nt r cat o
    , failedState :: Maybe (StateInfo s space nt r cat)
    , currentInput :: i
    , failReason :: FailReason err s space air nt r cat i o
    }
  | CrashedStack String
data FailReason err s space air nt r cat i o
  = StackInvariantFailed
  | UnknownState
    { state :: s
    }
  | Uhhhh
    { token :: cat
    }
  | UnknownReduction
    { reduction :: nt /\ r
    }
  | ExternalError String
  | UserRejection
    { userErr :: Array err
    }
  | NoTokenMatches
    { advance :: Map cat (space /\ ShiftReduce s (Fragment space nt cat /\ nt /\ r))
    }
  | UnexpectedToken
    { advance :: Map cat (space /\ ShiftReduce s (Fragment space nt cat /\ nt /\ r))
    , matched :: NonEmptyArray (space /\ air /\ cat /\ o /\ i)
    }
  | AllActionsRejected
    { possibilities :: NonEmptyArray (Poss s space air nt r cat i o /\ Array (FailReason err s space air nt r cat i o))
    }
  | Ambiguity
    { allPossibilities :: NonEmptyArray (Poss s space air nt r cat i o)
    , filtered :: NonEmptyArray (Poss s space air nt r cat i o)
    , userErr :: Array err
    }

justErrorName :: forall err s rec space air nt r cat i o. Len i => FailedStack err s rec space air nt r cat i o -> String
justErrorName (FailedStack { failReason, initialInput, currentInput }) =
  -- FIXME: String length in CodePoints
  errorName failReason <> " at " <> show (1 + len initialInput - len currentInput)
justErrorName (CrashedStack crash) = crash

errorName :: forall err s space air nt r cat i o. FailReason err s space air nt r cat i o -> String
errorName StackInvariantFailed = "Internal parser error: Stack invariant failed"
errorName (UnknownState _) = "Internal parser error: Unknown state"
errorName (Uhhhh _) = "Internal parser error: Uhhhh"
errorName (UnknownReduction _) = "Internal parser error: UnknownReduction"
errorName (ExternalError msg) = "Internal parser error: " <> msg
errorName (UserRejection _) = "Parse error: User rejection"
errorName (NoTokenMatches _) = "Parse error: No token matches"
errorName (UnexpectedToken _) = "Parse error: Unexpected token(s) match"
errorName (AllActionsRejected _) = "Parse error: All actions rejected"
errorName (Ambiguity _) = "Parse error: LR(1) ambiguity"

userErrors :: forall err s space air nt r cat i o. FailReason err s space air nt r cat i o -> Array err
userErrors (UserRejection { userErr }) = userErr
userErrors (AllActionsRejected { possibilities }) = foldMap (foldMap userErrors <<< snd) possibilities
userErrors (Ambiguity { userErr }) = userErr
userErrors _ = []

someSucceed :: forall e a b. (a -> Either e b) -> NonEmptyArray a -> (NonEmptyArray (a /\ e) \/ NonEmptyArray b)
someSucceed f as = case partitionMap f' (NEA.toArray as) of
  { right } | Just r <- NEA.fromArray right -> Right r
  { left } | Just l <- NEA.fromArray left -> Left l
  _ -> unsafeCrashWith "Elements disappeared during partitionMap"
  where
  f' a = case f a of
    Left e -> Left (a /\ e)
    Right b -> Right b

someSucceed' :: forall e a b. (a -> Either e b) -> Array a -> (Array (a /\ e) \/ Array b)
someSucceed' f as = case partitionMap f' as of
  { right } | not Array.null right -> Right right
  { left } -> Left left
  where
  f' a = case f a of
    Left e -> Left (a /\ e)
    Right b -> Right b

contextLexingParse ::
  forall s rec err space air nt r cat i o moreConfig.
    Ord s =>
    Semiring space =>
    Ord space =>
    Eq air =>
    Ord nt =>
    Eq r =>
    Ord cat =>
    Tokenize cat i o =>
    Tokenize space i air =>
    Monoid air =>
    Eq o =>
  { best :: Best err s (Fragment space nt cat /\ nt /\ r) cat i o
  | moreConfig
  } ->
  s /\ States s space nt r cat ->
  Options rec err space air nt r cat o ->
  rec ->
  i ->
  FailedStack err s rec space air nt r cat i o \/ ContextStack s rec err space air nt r cat o
contextLexingParse { best } (initialState /\ States states) acceptings rec initialInput =
  tailRecM go (Zero (initialMeta /\ initialState) /\ Left initialInput)
  where
  tabulated = indexStates (States states)

  reachable :: Set.Set s -> Set.Set s -> Set.Set s
  reachable s delta =
    let
      more = foldMap $ lookupState >>> foldMap \{ advance, receive } ->
        foldMap (bifoldMap Set.singleton mempty <<< snd) advance
          <> foldMap Set.singleton receive
      s' = s <> more delta
    in if s' == s then s else reachable s' (Set.difference s' s)
  allCatsFrom :: s -> Array cat
  allCatsFrom i = Array.nub do
    { advance: SemigroupMap advance } <-
      Array.filter (_.sName >>> Set.member <@> join reachable (Set.singleton i)) states
    Set.toUnfoldable $ Map.keys advance

  lookupState :: s -> Maybe (StateInfo s space nt r cat)
  lookupState s = tabulated s >>= Array.index states

  lookupAction ::
    ContextStack s rec err space air nt r cat o -> StateInfo s space nt r cat -> Either i (space /\ air /\ cat /\ o /\ i) ->
    FailReason err s space air nt r cat i o \/ _
  lookupAction stack { advance: SemigroupMap advance } = case _ of
    Left input -> do
      let actions = NEA.fromArray $ Array.mapMaybe (matchCat input) $ Map.toUnfoldable advance
      possibilities <- (actions?? NoTokenMatches { advance: coerce advance })? \e ->
        case NEA.fromArray $ Array.mapMaybe (matchCatFallback input) $ allCatsFrom initialState of
          Nothing -> e
          Just matched -> UnexpectedToken { advance: coerce advance, matched }
      checkBest stack possibilities
    Right (space /\ air /\ cat /\ o /\ i) -> do
      Additive _expected /\ sr <- Map.lookup cat advance?? Uhhhh { token: cat }
      checkBest stack (NEA.singleton (sr /\ space /\ air /\ cat /\ o /\ i))
  matchCat input (cat /\ (Additive space /\ sr)) = do
    air /\ input' <- recognize space input
    o /\ input'' <- recognize cat input'
    pure $ sr /\ space /\ air /\ cat /\ o /\ input''
  matchCatFallback input cat = do
    air /\ input' <- recognize (one :: space) input
    o /\ input'' <- recognize cat input'
    pure $ (one :: space) /\ air /\ cat /\ o /\ input''

  -- test ::
  --   _ ->
  --   ShiftReduce (s /\ space /\ air /\ cat /\ o) ((Fragment space nt cat /\ nt /\ r) /\ space /\ air /\ cat /\ o) ->
  --   Either (Array (FailReason err s space air nt r cat i o)) (ShiftReduce (s /\ space /\ air /\ cat /\ o) ((Fragment space nt cat /\ nt /\ r) /\ space /\ air /\ cat /\ o))
  test stack = filterSR' (testShift stack) (testReduce stack)
  testShift stack (s /\ space /\ air /\ cat /\ o) = lmap Array.singleton do
    (s /\ cat /\ o) <$ advanceMeta (topOf stack) (Terminal (space /\ air /\ cat /\ o))
  testReduce stack ((_rule /\ r) /\ space /\ air /\ cat /\ o) = lmap Array.singleton do
    let si = topOf stack # snd
    s <- lookupState si?? UnknownState { state: si }
    reduction <- lookupReduction r (space /\ air) s?? UnknownReduction { reduction: r }
    reduced <- takeStack r (space /\ air) stack reduction
    -- traceM "testReduce"
    -- pure ((_rule /\ r) /\ space /\ air /\ cat /\ o)
    ((_rule /\ r) /\ space /\ air /\ cat /\ o) <$ drain reduced (space /\ air /\ cat /\ o)

  drain :: ContextStack s rec err space air nt r cat o -> space /\ air /\ cat /\ o -> Either _ (ContextStack s rec err space air nt r cat o)
  drain stack _ = pure stack
  -- drain stack (space /\ air /\ cat /\ o) = do
  --   state@{ advance: SemigroupMap m } <-
  --     lookupState (snd (topOf stack))??
  --       UnknownState { state: snd (topOf stack) }
  --   Additive space /\ sr <- Map.lookup cat m?? Uhhhh { token: cat }
  --   case sr of
  --     Shift s -> do
  --       Snoc stack (ILeaf o) <$> (advanceMeta (topOf stack) (Terminal (space /\ air /\ cat /\ o)) <#> (_ /\ s))
  --     Reduces rs | [r] <- snd <$> NEA.toArray rs -> do
  --       reduction <- lookupReduction r (space /\ air) state?? UnknownReduction { reduction: r }
  --       stacked <- takeStack r (space /\ air) stack reduction
  --       -- traceM "drain"
  --       drain stacked (space /\ air /\ cat /\ o)
  --     _ -> do
  --       -- traceM "drain stopped"
  --       -- Left StackInvariantFailed
  --       pure stack
  chosenAction = case _ of
    Left s /\ space /\ air /\ cat /\ o /\ _i -> Shift (s /\ space /\ air /\ cat /\ o)
    Right r /\ space /\ air /\ cat /\ o /\ _i -> Reduces $ pure $ r /\ space /\ air /\ cat /\ o
  choosePossibility stack (sr /\ space /\ air /\ cat /\ o /\ i) =
    filterSR'
      (\s' -> testShift stack (s' /\ space /\ air /\ cat /\ o))
      (\r' -> testReduce stack (r' /\ space /\ air /\ cat /\ o)) sr
      <#> \sr' -> bimap fst fst sr' /\ space /\ air /\ cat /\ o /\ i
  checkBest stack allPossibilities = do
    case best allPossibilities of
      Right chosen | act <- chosenAction chosen, isRight $ test stack act ->
        Right chosen
      _ -> do
        filtered <- someSucceed (choosePossibility stack) allPossibilities?
          \possibilities -> AllActionsRejected
            { possibilities
            }
        best filtered? \userErr -> Ambiguity
          { allPossibilities
          , filtered
          , userErr
          }



  go ::
    (ContextStack s rec err space air nt r cat o /\ Either i (space /\ air /\ cat /\ o /\ i)) ->
    FailedStack err s rec space air nt r cat i o \/ Step
      (ContextStack s rec err space air nt r cat o /\ Either i (space /\ air /\ cat /\ o /\ i))
      (ContextStack s rec err space air nt r cat o)
  go (stack /\ Left input) | len input == 0 = pure (Done stack)
  go (stack /\ input) = do
    let
      contextualize failedState failReason =
        FailedStack
          { states: States states
          , lookupState
          , initialInput
          , failedStack: stack
          , failedState
          , currentInput: either identity (snd <<< snd <<< snd <<< snd) input
          , failReason
          }
    state <- lookupState (snd (topOf stack))?? contextualize Nothing (UnknownState { state: snd (topOf stack) })
    -- traceM input
    act <- lookupAction stack state input? contextualize (Just state)
    case act of
      Left s /\ space /\ air /\ cat /\ o /\ i -> do
        case advanceMeta (topOf stack) (Terminal (space /\ air /\ cat /\ o)) of
          Right x ->
            pure $ Loop $ Snoc (Snoc stack (IAir air) (topOf stack)) (ILeaf o) (x /\ s) /\ (Left i)
          Left e -> Left (contextualize (Just state) e)
      Right (_rule /\ r) /\ space /\ air /\ cat /\ o /\ i -> do
        reduction <- lookupReduction r (space /\ air) state?? contextualize (Just state) (UnknownReduction { reduction: r })
        stacked <- takeStack r (space /\ air) stack reduction? contextualize (Just state)
        pure $ Loop (stacked /\ Right (space /\ air /\ cat /\ o /\ i))
  lookupReduction (p /\ r) (space /\ air) { items: State items } = items # oneOfMap case _ of
    { rule: Zipper parsed finishing, pName, rName }
      | pName == p && rName == r
      , Just spaceAhead <- product <$> traverse unInterTerminal finishing
      , subsume space spaceAhead ->
        Just parsed
    _ -> Nothing
  takeStack ::
    nt /\ r ->
    space /\ air ->
    ContextStack s rec err space air nt r cat o ->
    Array _ ->
    Either _ (ContextStack s rec err space air nt r cat o)
  takeStack r spaceAirAhead stack0 parsed =
    let
      take1 (isFirst /\ taken /\ stack) = case _ of
        Terminal _ -> case stack of
          Snoc (Snoc stack' v@(ILeaf _) _) u@(IAir _) _ ->
            Right $ false /\ ([ v, u ] <> taken) /\ stack'
          Snoc stack' v@(ILeaf _) _ ->
            Right $ false /\ ([ v ] <> taken) /\ stack'
          _ -> Left StackInvariantFailed
        NonTerminal nt -> case stack of
          Snoc (Snoc stack' v@(IBranch (p /\ _) _) _) u@(IAir _) _ | p == nt ->
            Right $ false /\ ([ v, u ] <> taken) /\ stack'
          Snoc stack' v@(IBranch (p /\ _) _) _ | p == nt ->
            Right $ false /\ ([ v ] <> taken) /\ stack'
          _ -> Left StackInvariantFailed
        InterTerminal expected -> case stack of
          Snoc _ (IAir air) _ | rerecognize expected air -> -- FIXME?
            Right $ false /\ taken /\ stack
          _ | isFirst, rerecognize expected (snd spaceAirAhead) ->
            Right $ false /\ taken /\ stack
          _ | isFirst -> Left $ UserRejection { userErr: [] } -- ????
          _ -> Left StackInvariantFailed
    in
      Array.foldM take1 (true /\ [] /\ stack0) (Array.reverse parsed) >>= \(_wasEmpty /\ taken /\ stack) ->
        let
          air = case stack of
            Snoc _ (IAir _air) _ -> _air
            -- If it is only space
            Zero _ | _wasEmpty -> snd spaceAirAhead
            -- _ | _ <- spy "stack" stack -> snd $ spy "FIXME spaceAirAhead" spaceAirAhead
            _ -> snd spaceAirAhead
          stack' = case taken of
            [] -> Snoc stack (IAir mempty) (topOf stack)
            _ -> stack
        in Snoc stack' (IBranch r taken) <$> goto r air taken (topOf stack)
  goto :: nt /\ r -> air -> Array _ -> (Options rec err space air nt r cat o /\ s) -> Either _ (Options rec err space air nt r cat o /\ s)
  goto shifting@(p /\ _) (air) taken s = do
      st <- lookupState (snd s)?? UnknownState { state: snd s }
      r <- Map.lookup p st.receive?? UnknownState { state: snd s } -- UnknownReduction { reduction: shifting }
      advanceMeta s (NonTerminal (air /\ IBranch shifting taken)) <#> (_ /\ r)

  initialMeta :: Options rec err space air nt r cat o
  initialMeta = closeA acceptings
  advanceMeta ::
    Options rec err space air nt r cat o /\ s ->
    Part Void (air /\ ICST air (nt /\ r) o) (space /\ air /\ cat /\ o) ->
    Either _ (Options rec err space air nt r cat o)
  advanceMeta (accept /\ _s) advance =
    closeA <$> advanceAccepting rec accept advance?
      \userErr -> UserRejection { userErr }
