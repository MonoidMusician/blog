module Parser.Main where

import Prelude

import Bolson.Core (Child(..))
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Apply (lift2, lift3)
import Control.Bind (bindFlipped)
import Control.Monad.Gen as Gen
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.ST.Class (class MonadST)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Plus (empty)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (bimap, lmap)
import Data.Codec (BasicCodec, decode, encode)
import Data.Codec.Argonaut as CA
import Data.Compactable (compact)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either, hush, note)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, any, foldMap, oneOf, oneOfMap, sequence_, traverse_)
import Data.Foldable (length) as Foldable
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, maybe)
import Data.Maybe.Last (Last(..))
import Data.Newtype (unwrap)
import Data.Number (e, pi)
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (mapAccumL, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (class Attr, Attribute, cb, (:=))
import Bolson.Core (dyn, envy, fixed)
import Deku.Control (switcher, text, text_)
import Deku.Core (bus, bussed, vbussed)
import Deku.Core (class Korok, Domable, Nut)
import Deku.Core as DC
import Deku.DOM as D
import Deku.Listeners (click, slider)
import Effect (Effect)
import Effect.Class.Console as Log
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Behavior (step)
import FRP.Deku (withValue, (!:=), (<:=>), (?:=))
import FRP.Event (AnEvent, Event, bang, filterMap, fromEvent, keepLatest, makeEvent, mapAccum, memoize, sampleOn, subscribe, sweep, toEvent)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Class (biSampleOn)
import FRP.Event.Time (withTime)
import FRP.Event.VBus (V)
import FRP.Helpers (dedup, dedupOn, spotlight, spotlightChange, toggle)
import FRP.Memoize (memoBeh, memoBehFold)
import FRP.NotFrom (notFrom)
import FRP.Rate (Beats(..), RateInfo, timeFromRate)
import FRP.SampleJIT (readersT, sampleJITE)
import FRP.SelfDestruct (selfDestruct)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Algorithms (addEOF', calculateStates, fromSeed', fromString', gatherNonTerminals', gatherNonTerminals_, gatherTokens', gatherTokens_, getResultC, indexStates, longestFirst, numberStates, parseDefinition, parseIntoGrammar, toAdvanceTo, toTable, withProducible)
import Parser.Codecs (grammarCodec, intStringCodec, listCodec, mappy, maybeCodec, nonEmptyStringCodec, parseStepsCodec, producibleCodec, setCodec, stateInfoCodec, statesCodec)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps, topOf)
import Parser.Random (genNT, sampleS)
import Parser.Samples (defaultEOF, defaultTopName, defaultTopRName)
import Parser.Types (AST(..), CST(..), Grammar(..), Part(..), SAST, SAugmented, SCParseSteps, SCST, SCStack, SFragment, SGrammar, SProducible, SState, SStateIndex, SStateInfo, SStateItem, SStates, SZipper, ShiftReduce(..), State(..), States(..), Zipper(..), prune, unNonTerminal, unSPart, unTerminal)
import Partial.Unsafe (unsafePartial)
import Random.LCG as LCG
import Test.QuickCheck.Gen as QC
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Widget (Interface, Widget, adaptInterface)
import Widget.Types (SafeNut(..))

data StepAction = Initial | Toggle | Slider | Play

type Nuts =
  forall s m lock payload
   . DC.Korok s m
  => Array (DC.Domable m lock payload)

type Nutss =
  forall s m lock payload
   . DC.Korok s m
  => Array (Array (DC.Domable m lock payload))

inputC :: String -> String -> String -> (String -> Effect Unit) -> Nut
inputC label placeholder initialValue onInput =
  D.label (D.Class !:= "text")
    [ D.span_ [ text_ label ]
    , D.input
        ( oneOf
            [ D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
            , D.Value <:=> if initialValue == "" then empty else bang initialValue
            , D.OnInput !:= withValue onInput
            ]
        )
        []
    ]

inputValidated
  :: forall s m lock payload
   . Korok s m
  => String
  -> String
  -> String
  -> String
  -> AnEvent m String
  -> (String -> Effect Unit)
  -> Domable m lock payload
inputValidated cls label placeholder initialValue valid onInput =
  D.label (D.Class <:=> (append "text" <<< (eq "" >>> if _ then "" else " invalid")) <$> (bang "" <|> valid))
    [ D.span_ [ text_ label ]
    , D.input
        ( oneOf
            [ D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
            , D.Value <:=> if initialValue == "" then empty else bang initialValue
            , D.OnInput !:= withValue onInput
            , D.Class !:= cls
            ]
        )
        []
    , D.span (D.Class !:= "error") [ text valid ]
    ]

inputC' :: forall s m lock payload. Korok s m => String -> String -> AnEvent m String -> (String -> Effect Unit) -> Domable m lock payload
inputC' label placeholder initialValue onInput =
  D.label (D.Class !:= "text")
    [ D.span_ [ text_ label ]
    , D.input
        ( oneOf
            [ D.Placeholder <:=> if placeholder == "" then empty else bang placeholder
            , D.Value <:=> initialValue
            , D.OnInput !:= withValue onInput
            ]
        )
        []
    ]

type Header nt tok = Array tok /\ Array nt

getHeader :: forall s nt r tok. Ord nt => Ord tok => States s nt r tok -> Header nt tok
getHeader (States states) = bimap Array.nub Array.nub $
  states # foldMap \{ items: State items } -> items # foldMap \item ->
    ([] /\ [ item.pName ]) <> foldZipper fromPart item.rule
  where
  foldZipper f (Zipper l r) = foldMap f l <> foldMap f r
  fromPart (NonTerminal nt) = [] /\ [ nt ]
  fromPart (Terminal tok) = [ tok ] /\ []

col :: forall a m e. Eq a => Applicative m => Attr e D.Class String => a -> a -> AnEvent m (Attribute e)
col j i =
  if i == j then D.Class !:= "first" else empty

renderParseTable
  :: forall s m lock payload r
   . Korok s m
  => { getCurrentState :: Int -> AnEvent m Boolean | r }
  -> SGrammar
  -> SStates
  -> Domable m lock payload
renderParseTable info (MkGrammar grammar) (States states) =
  bussed \push event ->
    let
      stateHighlighted = bang Nothing <|> event
      terminals /\ nonTerminals = getHeader (States states)
      renderTerminals x = renderTok mempty x
      gatherRules nt = grammar # filterMap \r ->
        if r.pName == nt then Just r.rName else Nothing
      renderNonTerminals x =
        D.div (D.Class !:= "pileup") $
          (map (\y -> renderRule mempty y) (gatherRules x)) <>
            [ renderNT mempty x ]
      renderStHere s =
        D.span
          ( oneOf
              [ D.Class !:= "state hoverable"
              , D.OnMouseenter !:= push (Just s)
              , D.OnMouseleave !:= push Nothing
              ]
          )
          [ text_ (show s) ]
      renderShiftReduce Nothing = fixed []
      renderShiftReduce (Just (Shift s)) = D.span_ [ renderCmd mempty "s", renderStHere s ]
      renderShiftReduce (Just (Reduces rs)) =
        D.span (if NEA.length rs > 1 then D.Class !:= "conflict" else empty) $
          rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ]
      renderShiftReduce (Just (ShiftReduces s rs)) =
        D.span (D.Class !:= "conflict") $
          [ renderCmd mempty "s", renderStHere s ] <> (rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ])
      renderGoto Nothing = []
      renderGoto (Just s) = [ renderCmd mempty "g", renderStHere s ]
      cols state =
        let
          forTerminal tok = map snd <$> Map.lookup tok (unwrap state.advance)
          forNonTerminal nt = Map.lookup nt state.receive
        in
          map (pure <<< renderShiftReduce <<< forTerminal) terminals <> map (renderGoto <<< forNonTerminal) nonTerminals

      header = D.tr_ $ mapWithIndex (\i -> D.th (col (Array.length terminals + 1) i) <<< pure) $
        [ text_ "" ] <> map renderTerminals terminals <> map renderNonTerminals nonTerminals
      clsFor s =
        biSampleOn
          ((if _ then " active " else "") <$> info.getCurrentState s)
          $ stateHighlighted <#> \s' -> append $
              if s' == Just s then " hover " else ""
      rows = states <#> \state -> D.tr (D.Class <:=> clsFor state.sName)
        $ Array.cons (D.th_ [ renderStHere state.sName ])
        $
          mapWithIndex (\i -> D.td (col (Array.length terminals) i)) (cols state)
    in
      D.table (D.Class !:= "parse-table")
        [ D.thead_ [ header ]
        , D.tbody_ rows
        ]

type StartingTick = Boolean

type ParsedUIAction = V
  ( toggleLeft :: Unit
  , toggleRight :: Unit
  , slider :: Number
  , rate :: Number
  , startState :: Maybe (Effect Unit)
  , animationTick :: StartingTick /\ RateInfo
  )

data TodoAction = Prioritize | Delete

showStack :: SCStack -> Nut
showStack i = D.span (D.Class !:= "full stack") (go i)
  where
  go (Zero state) = [ D.sub_ [ renderSt mempty state ] ]
  go (Snoc stack tok state) = go stack
    <> [ renderCSTTree tok ]
    <> [ D.sub_ [ renderSt mempty state ] ]

renderStackItem :: Either CodePoint SAST -> Nut
renderStackItem (Left x) = renderTok mempty x
renderStackItem (Right x) = renderASTTree x

renderAST :: SAST -> Nut
renderAST (Layer (_ /\ r) []) = D.span (D.Class !:= "layer") [ renderRule mempty r ]
renderAST (Layer (_ /\ r) cs) =
  D.span (D.Class !:= "layer")
    [ renderMeta mempty "("
    , renderRule mempty r
    , fixed $ cs # foldMap \c -> [ text_ " ", renderAST c ]
    , renderMeta mempty ")"
    ]

renderASTTree :: SAST -> Nut
renderASTTree ast =
  D.ol (D.Class !:= "AST")
    [ D.li_ (renderASTChild ast) ]

renderASTChild :: SAST -> Nuts
renderASTChild (Layer (_ /\ r) []) =
  [ D.span (D.Class !:= "leaf node")
      [ renderRule mempty r ]
  ]
renderASTChild (Layer (_ /\ r) cs) =
  [ D.span (D.Class !:= "node")
      [ renderRule mempty r ]
  , D.ol (D.Class !:= "layer") $
      cs <#> \c -> D.li_ (renderASTChild c)
  ]

renderCSTTree :: SCST -> Nut
renderCSTTree ast =
  D.ol (D.Class !:= "AST CST")
    [ D.li_ (renderCSTChild ast) ]

renderCSTChild :: SCST -> Nuts
renderCSTChild (Leaf tok) =
  [ D.span (D.Class !:= "leaf node")
      [ renderTok mempty tok ]
  ]
renderCSTChild (Branch (_ /\ r) cs) =
  [ D.span (D.Class !:= "node")
      [ renderRule mempty r ]
  , D.ol (D.Class !:= "layer") $
      cs <#> \c -> D.li_ (renderCSTChild c)
  ]

showMaybeStack :: Maybe SCStack -> Nut
showMaybeStack Nothing = text_ "Parse error"
showMaybeStack (Just stack) = showStack stack

showMaybeParseSteps :: forall s m lock payload. Korok s m => Maybe SCParseSteps -> SuperStack m (Domable m lock payload)
showMaybeParseSteps Nothing = pure (pure (text_ "Parse error"))
showMaybeParseSteps (Just stack) = showParseSteps stack

getVisibilityAndIncrement
  :: forall m s element
   . MonadST s m
  => Attr element D.Class String
  => SuperStack m (Int /\ AnEvent m (Attribute element))
getVisibilityAndIncrement = getVisibilityAndIncrement' ""

getVisibilityAndIncrement'
  :: forall m s element
   . MonadST s m
  => Attr element D.Class String
  => String
  -> SuperStack m (Int /\ AnEvent m (Attribute element))
getVisibilityAndIncrement' s = do
  n <- get
  put (n + 1)
  pure
    ( \f -> n /\
        ( f n <#> \v ->
            D.Class := (s <> if v then "" else " hidden")
        )
    )

getVisibility
  :: forall m s element
   . MonadST s m
  => Attr element D.Class String
  => SuperStack m (Int /\ AnEvent m (Attribute element))
getVisibility = do
  n <- get
  pure
    ( \f -> n /\
        ( f n <#> \v ->
            D.Class := (if v then "" else " hidden")
        )
    )

showParseStep
  :: forall r s m lock payload
   . Korok s m
  => Either (Maybe SCStack)
       { inputs :: List CodePoint
       , stack :: SCStack
       | r
       }
  -> SuperStack m (Domable m lock payload)
showParseStep (Left Nothing) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    D.div vi [ (text_ $ ("Step " <> show n <> ": ") <> "Parse error") ]
showParseStep (Left (Just v)) = do
  getVisibilityAndIncrement <#> map \(_ /\ vi) ->
    case getResultC v of
      Just r | Right p <- map snd <$> prune r ->
        D.div vi [ text_ $ ("Step the last: "), renderCSTTree r ]
      _ ->
        D.div vi [ text_ $ ("Step the last: ") <> "Something went wrong" ]
showParseStep (Right { stack, inputs }) = do
  getVisibilityAndIncrement' "flex justify-between" <#> map \(n /\ vi) ->
    D.div vi [ D.div_ [ text_ ("Step " <> show n <> ": "), showStack stack ], D.div_ (foldMap (\x -> [ renderTok mempty x ]) inputs) ]

showParseTransition
  :: forall s m lock payload
   . Korok s m
  => Int /\ Either CodePoint (NonEmptyString /\ String)
  -> SuperStack m (Domable m lock payload)
showParseTransition (s /\ Left tok) = do
  getVisibility <#> map \(_ /\ vi) ->
    D.span vi [ {- renderTok mempty tok, text_ " ", -} renderCmd mempty "s", renderSt mempty s ]
showParseTransition (s /\ Right (_ /\ rule)) = do
  getVisibility <#> map \(_ /\ vi) ->
    D.span vi [ renderCmd mempty "r", renderRule mempty rule, renderMeta mempty " —> ", renderCmd mempty "g", renderSt mempty s ]

type SuperStack m a = StateT Int Trampoline ((Int -> AnEvent m Boolean) -> a)

showParseSteps
  :: forall s m lock payload
   . Korok s m
  => SCParseSteps
  -> SuperStack m (Domable m lock payload)
showParseSteps i = map fixed <$> (go i)
  where
  go =
    let
      s v = showParseStep v
      t v = showParseTransition v
    in
      case _ of
        Error prev -> do
          lift2 (\o u -> [ o, u ]) <$> s (Right prev) <*> s (Left Nothing)
        Complete prev v -> do
          lift2 (\o u -> [ o, u ]) <$> s (Right prev) <*> s (Left (Just v))
        Step prev action more -> do
          lift3 (\o v r -> [ o, v ] <> r) <$> s (Right prev) <*> t (firstState more /\ action) <*> go more

renderStateTable :: forall s m lock payload r. Korok s m => { getCurrentState :: Int -> AnEvent m Boolean | r } -> SStates -> Domable m lock payload
renderStateTable info (States states) = do
  let
    mkTH n 0 0 = D.th (D.Rowspan !:= show n)
    mkTH _ _ 0 = const (fixed [])
    mkTH _ _ _ = D.td_
    stateClass sName = (if _ then "active" else "") <$> info.getCurrentState sName
    renderStateHere items =
      let
        n = Array.length items
      in
        items # mapWithIndex \j -> D.tr_ <<< mapWithIndex (\i -> mkTH n j i <<< pure)
  D.table (D.Class !:= "state-table")
    $ states <#>
        \s@{ sName, items } ->
          D.tbody (D.Class <:=> stateClass sName)
            $ renderStateHere
            $ renderState s items

renderState :: SStateInfo -> SState -> Nutss
renderState s (State items) = (\j v -> renderItem s j v) `mapWithIndex` items

renderItem :: SStateInfo -> Int -> SStateItem -> Nuts
renderItem s j { pName, rName, rule: rule@(Zipper _ after), lookahead } =
  [ if j == 0 then renderSt mempty s.sName else text_ ""
  , renderNT mempty pName
  , renderMeta mempty ": "
  , renderZipper rule
  , renderLookahead (if Array.null after then " reducible" else "") lookahead
  , fixed [ renderMeta mempty " #", renderRule mempty rName ]
  , case toAdvanceTo s rule of
      Nothing -> fixed []
      Just s' -> fixed [ renderMeta mempty " —> ", renderSt mempty s' ]
  ]

renderZipper :: SZipper -> Nut
renderZipper (Zipper before after) =
  D.span (D.Class !:= ("zipper" <> if Array.null after then " reducible" else ""))
    [ D.span (D.Class !:= "parsed") $ before <#> \x -> renderPart mempty x
    , if Array.null after then fixed empty
      else
        D.span empty $ after <#> \x -> renderPart mempty x
    ]

renderLookahead :: String -> Array CodePoint -> Nut
renderLookahead moreClass items = D.span (D.Class !:= append "lookahead" moreClass) $
  [ renderMeta mempty "{ " ]
    <> Array.intercalate [ renderMeta mempty ", " ] (items <#> \x -> [ renderTok mempty x ])
    <> [ renderMeta mempty " }" ]

--------------------------------------------------------------------------------

debug :: forall m a. Show a => String -> AnEvent m a -> AnEvent m a
debug tag = map \a -> unsafePerformEffect (a <$ (Log.info (tag <> show a)))

debugJson :: forall m. String -> AnEvent m Json.Json -> AnEvent m Json.Json
debugJson tag = map \a -> unsafePerformEffect (a <$ (Log.info (tag <> Json.stringify a)))

unsafeDebug :: forall m a. String -> AnEvent m a -> AnEvent m a
unsafeDebug tag = map \a -> unsafePerformEffect (a <$ (Log.info tag <* Log.info (unsafeCoerce a)))

silentDebug :: forall m a. String -> AnEvent m a -> AnEvent m a
silentDebug tag = map \a -> unsafePerformEffect (a <$ Log.info tag)

renderAs :: String -> String -> Nut
renderAs c t = D.span (D.Class !:= c) [ text_ t ]

renderTok :: Maybe (Effect Unit) -> CodePoint -> Nut
renderTok c t = D.span (D.OnClick ?:= c <|> D.Class !:= "terminal" <> if isJust c then " clickable" else "") [ text_ (String.singleton t) ]

renderTok' :: forall s m lock payload. Korok s m => AnEvent m String -> AnEvent m (Maybe (Effect Unit)) -> CodePoint -> Domable m lock payload
renderTok' cls c t = D.span (D.OnClick <:=> filterMap identity c <|> D.Class <:=> (bang "terminal" <|> (append "terminal " <$> cls))) [ text_ (String.singleton t) ]

renderNT :: Maybe (Effect Unit) -> NonEmptyString -> Nut
renderNT c nt = D.span (D.OnClick ?:= c <|> D.Class !:= "non-terminal" <> if isJust c then " clickable" else "") [ text_ (NES.toString nt) ]

renderNT' :: forall s m lock payload. Korok s m => AnEvent m String -> AnEvent m (Maybe (Effect Unit)) -> NonEmptyString -> Domable m lock payload
renderNT' cls c nt = D.span (D.OnClick <:=> filterMap identity c <|> D.Class <:=> (bang "non-terminal" <|> (append "non-terminal " <$> cls))) [ text_ (NES.toString nt) ]

renderRule :: Maybe (Effect Unit) -> String -> Nut
renderRule c r = D.span (D.OnClick ?:= c <|> D.Class !:= "rule" <> if isJust c then " clickable" else "") [ text_ r ]

renderMeta :: Maybe (Effect Unit) -> String -> Nut
renderMeta c x = D.span (D.OnClick ?:= c <|> D.Class !:= "meta" <> if isJust c then " clickable" else "") [ text_ x ]

renderSt :: Maybe (Effect Unit) -> Int -> Nut
renderSt c x = D.span (D.OnClick ?:= c <|> D.Class !:= "state" <> if isJust c then " clickable" else "") [ text_ (show x) ]

renderSt' :: forall s m lock payload. Korok s m => AnEvent m String -> Maybe (Effect Unit) -> Int -> Domable m lock payload
renderSt' cls c x = D.span (D.OnClick ?:= c <|> D.Class <:=> (bang "state" <|> (append "state " <$> cls))) [ text_ (show x) ]

renderPart :: Maybe (Effect Unit) -> Part NonEmptyString CodePoint -> Nut
renderPart c (NonTerminal nt) = renderNT c nt
renderPart c (Terminal t) = renderTok c t

renderCmd :: Maybe (Effect Unit) -> String -> Nut
renderCmd c x = D.span (D.OnClick ?:= c <|> D.Class !:= "cmd" <> if isJust c then " clickable" else "") [ text_ x ]

type GrammarInputs =
  ( pName :: V (value :: String, error :: String)
  , rule :: V (value :: String, error :: String)
  , rName :: V (value :: String, error :: String)
  , top :: V (value :: String, error :: String)
  , entry :: V (value :: String, error :: String)
  , eof :: V (value :: String, error :: String)
  , topName :: V (value :: String, error :: String)
  )

type GrammarAction =
  ( errorMessage :: Maybe ParseGrammarError
  , addRule :: Int /\ { pName :: NonEmptyString, rule :: String, rName :: String }
  , removeRule :: Int
  , troubleRules :: Array String
  )

only :: forall a. Array a -> Maybe a
only [ a ] = Just a
only _ = Nothing

findNext :: Set String -> String -> Int -> String
findNext avoid pre n =
  let
    pren = pre <> show n
  in
    if pren `Set.member` avoid then findNext avoid pre (n + 1) else pren

parseGrammar
  :: { top :: NonEmptyString
     , entry :: Maybe NonEmptyString
     , eof :: CodePoint
     , topName :: String
     }
  -> Array { pName :: NonEmptyString, rule :: String, rName :: String }
  -> Either ParseGrammarError SAugmented
parseGrammar top rules = do
  firstRule <- note NoRules $ Array.head rules
  let
    entry = fromMaybe firstRule.pName top.entry
    nonTerminals = longestFirst $ [ top.top ] <> (rules <#> _.pName)
    parse = parseDefinition nonTerminals
    rules' = _.value $ rules # flip mapAccumL Set.empty
      \ruleNames r ->
        let
          rName =
            if r.rName /= "" then r.rName
            else
              findNext ruleNames (NES.toString r.pName) 1
        in
          { value: r { rule = parse r.rule, rName = rName }, accum: Set.insert rName ruleNames }
    topRule = { pName: top.top, rName: top.topName, rule: [ NonTerminal entry, Terminal top.eof ] }
    start =
      { pName: topRule.pName
      , rName: topRule.rName
      , rule: Zipper [] topRule.rule
      , lookahead: []
      }

    tokenRefs = gatherTokens_ (MkGrammar rules')
    ntRefs = gatherNonTerminals_ (MkGrammar rules')
    eofNT = NES.singleton top.eof

    duplicatedRules =
      filterMap (\rs -> if NEA.length rs > 1 then Just (NEA.head rs) else Nothing)
        (Array.groupAll ((rules' <#> _.rName) <> [ top.topName ]))
  if top.top == entry then Left TopClash
  else pure unit
  case NEA.fromArray duplicatedRules of
    Just d -> Left $ RuleNamesUnique d
    Nothing -> pure unit
  if not isJust (Array.find (eq entry <<< _.pName) rules) then Left (MissingEntry entry)
  else pure unit
  case Map.lookup top.top ntRefs of
    Just refs -> Left (ReferencesTop refs)
    Nothing -> pure unit
  case NEA.fromArray $ filterMap (\r -> if r.pName == eofNT then Just r.rName else Nothing) rules' of
    Just refs -> Left (EOFNonTerminal refs)
    Nothing -> pure unit
  case Map.lookup top.eof tokenRefs <> Map.lookup eofNT ntRefs of
    Just refs -> Left (ReferencesEOF refs)
    Nothing -> pure unit
  pure $ { augmented: MkGrammar ([ topRule ] <> rules'), start, eof: top.eof, entry }

data ParseGrammarError
  = ReferencesEOF (NonEmptyArray String)
  | EOFNonTerminal (NonEmptyArray String)
  | ReferencesTop (NonEmptyArray String)
  | MissingEntry NonEmptyString
  | TopClash
  | RuleNamesUnique (NonEmptyArray String)
  | NoRules

pl :: forall f a b. Foldable f => f a -> b -> b -> b
pl as _ many | Foldable.length as > 1 = many
pl _ single _ = single

list
  :: forall f s m lock payload
   . Korok s m
  => Foldable f
  => f (Domable m lock payload)
  -> Array (Domable m lock payload)
list = Array.fromFoldable >>> case _ of
  [ a ] -> [ a ]
  [ a, b ] -> [ a, text_ " & ", b ]
  as -> case Array.unsnoc as of
    Nothing -> []
    Just { init: as', last: b } ->
      Array.intercalate [ text_ ", " ] (map pure as') <> [ text_ " & ", b ]

rulesAtFault :: ParseGrammarError -> Array String
rulesAtFault = case _ of
  NoRules -> []
  ReferencesEOF rules -> NEA.toArray rules
  EOFNonTerminal rules -> NEA.toArray rules
  ReferencesTop rules -> NEA.toArray rules
  MissingEntry _ -> []
  TopClash -> []
  RuleNamesUnique rules -> NEA.toArray rules

parseGrammarError :: ParseGrammarError -> Nut
parseGrammarError NoRules = text_ "Need at least one rule in the grammar."
parseGrammarError (ReferencesEOF rules) = fixed $
  [ text_ "The final token must only be referenced in the top rule, but it was also referenced in " ]
    <> list (map (\x -> fixed [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ text_ "." ]
parseGrammarError (EOFNonTerminal rules) = fixed $
  [ text_ "The final token must not be a nonterminal, but it appeared in " ]
    <> list (map (\x -> fixed [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ text_ "." ]
parseGrammarError (ReferencesTop rules) = fixed $
  [ text_ "The top nonterminal must not appear in the rest of the grammar, but it was also referenced in " ]
    <> list (map (\x -> fixed [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ text_ "." ]
parseGrammarError (MissingEntry nt) = fixed
  [ text_ "Entry point ", renderNT mempty nt, text_ " is not present as a nonterminal in the grammar." ]
parseGrammarError TopClash = text_ "Entry point must be different from the top rule."
parseGrammarError (RuleNamesUnique rules) = fixed $
  [ text_ "Rule names must be unique, but " ] <> list (map (\x -> fixed [ renderMeta mempty "#", renderRule mempty x ]) rules) <> [ text_ " ", text_ (pl rules "was" "were"), text_ " duplicated" ]

grammarComponent
  :: forall s m lock payload
   . Korok s m
  => String
  -> SAugmented
  -> AnEvent Effect SAugmented
  -> (SAugmented -> Effect Unit)
  -> Domable m lock payload
grammarComponent buttonText reallyInitialGrammar forceGrammar sendGrammar =
  (bang reallyInitialGrammar <|> fromEvent forceGrammar) `flip switcher` \initialGrammar ->
    vbussed (Proxy :: Proxy (V GrammarInputs)) \putInput inputs ->
      vbussed (Proxy :: Proxy (V GrammarAction)) \pushState changeState ->
        let
          changeRule =
            ( \change rules ->
                case change of
                  Left new -> Array.snoc rules new
                  Right remove -> Array.filter (fst >>> not eq remove) rules
            )
          ruleChanges = (Left <$> changeState.addRule <|> Right <$> changeState.removeRule)
          initialRules = unwrap initialGrammar.augmented # Array.drop 1 # mapWithIndex \i rule ->
            i /\ rule { rule = rule.rule # foldMap unSPart }
          initialTop =
            case Array.head (unwrap initialGrammar.augmented) of
              Just { pName, rName, rule: [ NonTerminal entry, Terminal eof ] } ->
                { top: NES.toString pName
                , entry: NES.toString entry
                , eof: String.singleton eof
                , topName: rName
                }
              _ ->
                { top: NES.toString defaultTopName
                , entry: ""
                , eof: String.singleton defaultEOF
                , topName: defaultTopRName
                }
          currentText =
            biSampleOn (bang "" <|> inputs.pName.value)
              $ biSampleOn (bang "" <|> inputs.rule.value)
              $
                (bang "" <|> inputs.rName.value) <#> \rName rule pName ->
                  { rName, rule, pName }
          currentTop =
            biSampleOn (bang initialTop.top <|> inputs.top.value)
              $ biSampleOn (bang initialTop.entry <|> inputs.entry.value)
              $ biSampleOn (bang initialTop.eof <|> inputs.eof.value)
              $ biSampleOn (bang initialTop.topName <|> inputs.topName.value)
              $ bang { topName: _, eof: _, entry: _, top: _ }
          counted = add (Array.length initialRules) <$>
            (bang 0 <|> (add 1 <$> fst <$> changeState.addRule))
        in
          envy $ memoBeh currentTop initialTop \currentTop -> do
          envy $ memoBehFold changeRule ruleChanges initialRules \currentRules -> do
            let
              currentNTs = dedup $ map longestFirst $
                biSampleOn
                  (map (_.pName <<< snd) <$> currentRules)
                  (append <<< pure <<< fromMaybe defaultTopName <<< NES.fromString <<< _.top <$> currentTop)
              currentTopParsed = biSampleOn currentRules $ currentTop <#> \r rules ->
                { top: fromMaybe defaultTopName $ NES.fromString r.top
                , entry: NES.fromString r.entry <|> (Array.head rules <#> snd >>> _.pName)
                , eof: fromMaybe defaultEOF $ only $ String.toCodePointArray r.eof
                , topName: r.topName
                }
              currentGrammar = biSampleOn (map snd <$> currentRules) (currentTopParsed <#> parseGrammar)

              rulesInTrouble = bang [] <|> changeState.troubleRules

              putEofError v = putInput.eof.error $
                if String.length v <= 1 then ""
                else
                  "Must be a single character (Unicode code point)"
            D.div_
              [ D.div_
                  [ join (inputValidated "non-terminal" "Top name") initialTop.top inputs.top.error (const (putInput.top.error "") <> putInput.top.value)
                  , inputValidated "non-terminal" "Entry point" "" initialTop.entry inputs.entry.error (const (putInput.entry.error "") <> putInput.entry.value)
                  , join (inputValidated "terminal" "Final token") initialTop.eof inputs.eof.error (putEofError <> putInput.eof.value)
                  , join (inputValidated "rule" "Top rule name") initialTop.topName inputs.topName.error (const (putInput.topName.error "") <> putInput.topName.value)
                  ]
              , D.table (D.Class !:= "grammar")
                  [ D.tr_
                      [ D.td_ [ switcher (\x -> renderNT mempty x) (currentTopParsed <#> _.top) ]
                      , D.td_ [ renderMeta mempty " : " ]
                      , D.td_
                          [ D.span_ [ switcher (maybe (text_ "—") (\x -> renderNT mempty x)) (currentTopParsed <#> _.entry) ]
                          , D.span_ [ switcher (\x -> renderTok mempty x) (currentTopParsed <#> _.eof) ]
                          ]
                      , D.td_
                          [ renderMeta mempty " #"
                          , D.span_ [ switcher (\x -> renderRule mempty x) (currentTopParsed <#> _.topName) ]
                          ]
                      ]
                  , D.tbody_ $ pure $ dyn $ map
                      ( \(i /\ txt) -> keepLatest $ bus \p' e' ->
                          ( bang $ Insert $ D.tr (D.Class <:=> (rulesInTrouble <#> \rit -> if Array.elem txt.rName rit then "trouble" else "")) $ map (D.td_ <<< pure) $
                              [ renderNT mempty txt.pName
                              , renderMeta mempty " : "
                              , D.span_
                                  [ switcher
                                      ( \nts ->
                                          fixed $ map (\x -> renderPart mempty x) (parseDefinition nts txt.rule)
                                      )
                                      currentNTs
                                  ]
                              , D.span_
                                  [ renderMeta mempty " #"
                                  , renderRule mempty txt.rName
                                  ]
                              , D.span_
                                  [ text_ " "
                                  , D.button
                                      ( oneOf
                                          [ D.Class !:= "delete"
                                          , D.OnClick !:= (p' Remove *> pushState.removeRule i *> pushState.troubleRules [])
                                          ]
                                      )
                                      [ text_ "Delete" ]
                                  ]
                              ]
                          ) <|> e'
                      )
                      (oneOfMap bang initialRules <|> changeState.addRule)
                  ]
              , D.div_
                  [ inputValidated "non-terminal" "Nonterminal name" "" "" inputs.pName.error (const (putInput.pName.error "") <> putInput.pName.value)
                  , renderMeta mempty " : "
                  , inputValidated "terminal" "Value" "" "" inputs.rule.error (const (putInput.rule.error "") <> putInput.rule.value)
                  , renderMeta mempty " #"
                  , inputValidated "rule" "Rule name" "" "" inputs.rName.error (const (putInput.rName.error "") <> putInput.rName.value)
                  , D.button
                      ( oneOf
                          [ D.Class !:= "big add"
                          , D.OnClick <:=> do
                              sampleJITE currentText $ sampleJITE counted
                                $ map readersT
                                $ bang \i text -> do
                                    case NES.fromString text.pName of
                                      Nothing -> do
                                        putInput.pName.error "Name for non-terminal must be non-empty."
                                      -- TODO: findNext ruleNames text.pName 1
                                      Just pName -> do
                                        pushState.errorMessage Nothing
                                        putInput.pName.error ""
                                        putInput.rule.error ""
                                        putInput.rName.error ""
                                        pushState.troubleRules []
                                        pushState.addRule (i /\ text { pName = pName })
                          ]
                      )
                      [ text_ "Add rule" ]
                  ]
              , D.br_ []
              , if buttonText == "" then fixed []
                else
                  D.div_ $ pure $ D.button
                    ( oneOf
                        [ D.Class !:= "big"
                        , currentGrammar <#> \g -> D.OnClick := do
                            pushState.errorMessage Nothing
                            putInput.pName.error ""
                            putInput.rule.error ""
                            putInput.rName.error ""
                            putInput.top.error ""
                            putInput.entry.error ""
                            putInput.eof.error ""
                            putInput.topName.error ""
                            pushState.troubleRules []
                            case g of
                              Left err -> do
                                pushState.errorMessage (Just err)
                                pushState.troubleRules (rulesAtFault err)
                                case err of
                                  NoRules ->
                                    pure unit
                                  ReferencesEOF _ ->
                                    putInput.eof.error "Must not appear in the rest of the grammar"
                                  EOFNonTerminal _ ->
                                    putInput.eof.error "Cannot appear as nonterminal"
                                  ReferencesTop _ ->
                                    putInput.top.error "Must not appear in the rest of the grammar"
                                  MissingEntry _ ->
                                    putInput.entry.error "Must name a nonterminal in the grammar"
                                  TopClash ->
                                    putInput.entry.error "Cannot be the top rule itself"
                                  RuleNamesUnique _ ->
                                    pure unit
                              Right g' -> do
                                sendGrammar g'
                        ]
                    )
                    [ text_ buttonText ]
              , D.div_
                  [ changeState.errorMessage # switcher \et -> case et of
                      Nothing -> envy empty
                      Just e -> fixed
                        [ D.br_ []
                        , D.div (D.Class !:= "Error") [ parseGrammarError e ]
                        ]
                  ]
              ]

type TopLevelUIAction = V
  ( changeText :: Boolean /\ String
  , errorMessage :: Maybe String
  , grammar :: SAugmented
  , focusMode :: Unit
  )

sampleGrammar :: SAugmented
sampleGrammar = fromSeed' defaultTopName defaultTopRName defaultEOF
  ( parseIntoGrammar
      [ { pName: "Additive", rName: "A:Add", rule: "Additive+Multiplicative" }
      , { pName: "Additive", rName: "A<-M", rule: "Multiplicative" }
      , { pName: "Multiplicative", rName: "M:Mul", rule: "Multiplicative*Unit" }
      , { pName: "Multiplicative", rName: "M<-U", rule: "Unit" }
      , { pName: "Unit", rName: "U:Val", rule: "Number" }
      , { pName: "Unit", rName: "U<-A", rule: "(Additive)" }
      , { pName: "Number", rName: "N<-D", rule: "Digit" }
      , { pName: "Digit", rName: "0", rule: "0" }
      , { pName: "Digit", rName: "1", rule: "1" }
      , { pName: "Digit", rName: "2", rule: "2" }
      {-
      , { pName: "Digit", rName: "3", rule: "3" }
      , { pName: "Digit", rName: "4", rule: "4" }
      , { pName: "Digit", rName: "5", rule: "5" }
      , { pName: "Digit", rName: "6", rule: "6" }
      , { pName: "Digit", rName: "7", rule: "7" }
      , { pName: "Digit", rName: "8", rule: "8" }
      , { pName: "Digit", rName: "9", rule: "9" }
      -}
      ]
  )
  (unsafePartial (fromJust (NES.fromString "Additive")))

lastState :: SCParseSteps -> Int
lastState (Error x) = topOf x.stack
lastState (Complete _ x) = topOf x
lastState (Step _ _ s) = lastState s

firstState :: SCParseSteps -> Int
firstState (Error x) = topOf x.stack
firstState (Complete x _) = topOf x.stack
firstState (Step x _ _) = topOf x.stack

type ExplorerAction =
  ( focus :: Maybe (Int /\ NonEmptyString)
  , select :: SFragment
  )

explorerComponent
  :: forall s m lock payload
   . Korok s m
  => SProducible
  -> (Array CodePoint -> Effect Unit)
  -> Domable m lock payload
explorerComponent { produced: producedRules, grammar: { augmented: MkGrammar rules, start: { pName: entry } } } sendUp =
  vbussed (Proxy :: Proxy (V ExplorerAction)) \push event -> do
    envy $ memoBeh event.select [ NonTerminal entry ] \currentParts -> do
      let
        firstNonTerminal = Array.head <<< foldMapWithIndex
          \i v -> maybe [] (\r -> [ i /\ r ]) (unNonTerminal v)
      envy $ memoBeh (event.focus <|> map firstNonTerminal currentParts) (Just (0 /\ entry)) \currentFocused -> do
        let
          activity here = here <#> if _ then "active" else "inactive"
          renderPartHere i (NonTerminal nt) =
            D.span
              (D.Class <:=> (currentFocused <#> any (fst >>> eq i) >>> if _ then "selected" else ""))
              [ renderNT (Just (push.focus (Just (i /\ nt)))) nt ]
          renderPartHere _ (Terminal tok) = renderTok mempty tok
          send = currentParts <#> \parts ->
            case traverse unTerminal parts of
              Nothing -> Nothing
              Just toks -> Just (sendUp toks)
        D.div_
          [ D.span_ [ switcher (fixed <<< mapWithIndex renderPartHere) currentParts ]
          , D.button
              ( D.Class <:=> maybe "disabled" mempty <$> send
                  <|> D.OnClick <:=> sequence_ <$> send
              )
              [ text_ "Send" ]
          , D.button
              ( D.Class !:= "delete"
                  <|> D.OnClick !:= push.select [ NonTerminal entry ]
              )
              [ text_ "Reset" ]
          , D.table (D.Class !:= "explorer-table")
              [ D.tbody_ $ rules <#> \rule -> do
                  let
                    focusHere = currentFocused # map (any (snd >>> eq rule.pName))
                    replacement = sampleOn currentParts $ currentFocused <#> \mfoc parts -> do
                      focused /\ nt <- mfoc
                      guard $ nt == rule.pName
                      guard $ focused <= Array.length parts
                      pure $ Array.take focused parts <> rule.rule <> Array.drop (focused + 1) parts
                  D.tr (D.Class <:=> activity focusHere) $ map (D.td_ <<< pure) $
                    [ renderNT mempty rule.pName
                    , renderMeta mempty " : "
                    , fixed $ map (\x -> renderPart mempty x) rule.rule
                    , D.span_
                        [ renderMeta mempty " #"
                        , renderRule mempty rule.rName
                        ]
                    , D.span_
                        [ text_ " "
                        , D.button
                            ( oneOf
                                [ D.Class <:=> (append "select")
                                    <$> (if _ then mempty else " disabled")
                                    <$> focusHere
                                , D.OnClick <:=> traverse_ push.select <$> replacement
                                ]
                            )
                            [ text_ "Choose" ]
                        ]
                    , case Array.find (_.production >>> eq rule) producedRules of
                        Nothing -> text_ "Unproducible"
                        Just { produced } ->
                          D.span_
                            [ D.em_ [ text_ "e.g. " ]
                            , fixed $ map (\x -> renderTok mempty x) produced
                            ]
                    ]
              ]
          ]

type RandomAction =
  ( size :: Int
  , amt :: Int
  , randomMany :: Array (Array CodePoint)
  )

randomComponent
  :: forall s m lock payload
   . Korok s m
  => SProducible
  -> (Array CodePoint -> Effect Unit)
  -> Domable m lock payload
randomComponent { produced: producedRules, grammar: { start: { pName: entry } } } sendUp =
  vbussed (Proxy :: Proxy (V RandomAction)) \push event -> do
    let
      initialSize = 50
      initialAmt = 15
      randomProductions nt sz amt =
        genNT producedRules nt # traverse (Gen.resize (const sz) >>> QC.randomSample' amt)
      initial = genNT producedRules entry # maybe [] (QC.sample (LCG.mkSeed 1234) initialAmt)
    D.div_
      [ D.div_
          [ D.label (D.Class !:= "range")
              [ D.span_ [ text_ "Simple" ]
              , D.input
                  ( oneOf
                      [ D.Xtype !:= "range"
                      , D.Min !:= "0"
                      , D.Max !:= "100"
                      , slider $ bang $ push.size <<< Int.round
                      ]
                  )
                  []
              , D.span_ [ text_ "Complex" ]
              ]
          , D.label (D.Class !:= "range")
              [ D.span_ [ text_ "Few" ]
              , D.input
                  ( oneOf
                      [ D.Xtype !:= "range"
                      , D.Min !:= "1"
                      , D.Max !:= "30"
                      , D.Value !:= "15"
                      , slider $ bang $ push.amt <<< Int.round
                      ]
                  )
                  []
              , D.span_ [ text_ "Many" ]
              ]
          , D.button
              ( D.Class !:= ""
                  <|> D.OnClick <:=>
                    ( biSampleOn (bang initialAmt <|> event.amt) $
                        ( (bang initialSize <|> event.size) <#> \sz amt ->
                            (traverse_ (push.randomMany) =<< randomProductions entry sz amt)
                        )
                    )
              )
              [ text_ "Random" ]
          ]
      , D.ul (D.Class !:= "compact") $ pure $ switcher (fixed <<< map (\xs -> D.li (D.Class !:= "clickable" <|> D.OnClick !:= sendUp xs) <<< map (\x -> renderTok mempty x) $ xs))
          (bang initial <|> event.randomMany)
      ]

peas :: Array String -> Nut
peas x = fixed (map (D.p_ <<< pure <<< text_) x)

computeGrammar :: SAugmented ->
  { producible :: SProducible
  , states :: SStates
  , stateIndex :: SStateIndex
  , allTokens :: Array CodePoint
  , allNTs :: Array NonEmptyString
  }
computeGrammar grammar =
  let
    _producible = withProducible grammar
    _states = either (const (States [])) identity $ numberStates (add 1) grammar.augmented (calculateStates grammar.augmented grammar.start)
    _stateIndex = indexStates _states
    _allTokens = gatherTokens' grammar.augmented
    _allNTs = gatherNonTerminals' grammar.augmented
  in
    { producible: _producible
    , states: _states
    , stateIndex: _stateIndex
    , allTokens: _allTokens
    , allNTs: _allNTs
    }

computeInput :: forall r.
  { grammar :: SAugmented
  , states :: SStates
  , stateIndex :: SStateIndex
  | r
  } ->
  String ->
  { tokens :: Maybe (List CodePoint)
  , tokens' :: Maybe (List CodePoint)
  , parseSteps :: Maybe SCParseSteps
  , stateId :: Int
  , state :: Maybe SStateInfo
  , validTokens :: Set CodePoint
  , validNTs :: Set NonEmptyString
  }
computeInput { grammar, states, stateIndex } =
  let
    _tokenize' = fromString' grammar
    table = toTable states stateIndex
  in \value ->
    let
      _tokens' = _tokenize' value
      _tokens = _tokens' <#> addEOF' grammar
      _parseSteps = parseSteps table <$> _tokens <@> 1
      _parseSteps' = parseSteps table <$> _tokens' <@> 1
      _stateId = maybe 0 lastState _parseSteps'
      _state = unwrap states # Array.find (_.sName >>> eq _stateId)
      _validTokens = _state # maybe Map.empty (_.advance >>> unwrap) # Map.keys
      _validNTs = _state # maybe Map.empty _.receive # Map.keys
    in
      { tokens: _tokens
      , tokens': _tokens'
      , parseSteps: _parseSteps
      , stateId: _stateId
      , state: _state
      , validTokens: _validTokens
      , validNTs: _validNTs
      }

widgetGrammar :: Widget
widgetGrammar { interface, attrs } = do
  let
    io =
      { grammar: adaptInterface grammarCodec (interface "grammar")
      , producible: adaptInterface producibleCodec (interface "producible")
      , states: adaptInterface statesCodec (interface "states")
      , stateIndex: adaptInterface (mappy intStringCodec CA.int) (interface "stateIndex")
      , allTokens: adaptInterface (CA.array CA.codePoint) (interface "allTokens")
      , allNTs: adaptInterface (CA.array nonEmptyStringCodec) (interface "allNTs")
      }
    sendOthers grammar = do
      let computed = computeGrammar grammar
      io.producible.send computed.producible
      io.states.send computed.states
      io.stateIndex.send computed.stateIndex
      io.allTokens.send computed.allTokens
      io.allNTs.send computed.allNTs
    upstream =
      { send: \grammar -> do
          sendOthers grammar
          io.grammar.send grammar
      , receive: io.grammar.receive
      , current: io.grammar.current
      }
  initialGrammar <- fromMaybe sampleGrammar <$> upstream.current
  -- For all of the other data, it might not be updated (e.g. because we only
  -- received a grammar from the query)
  sendOthers initialGrammar
  buttonName <- fromMaybe "Use grammar" <<< Json.toString <$> attrs "action"
  pure $ SafeNut do
    grammarComponent buttonName initialGrammar upstream.receive upstream.send

sideKick :: forall a. (a -> Effect Unit) -> Event a -> Effect (Event a)
sideKick eff e = do
  current <- Ref.new 0
  pure $ makeEvent \k -> do
    this <- add 1 <$> Ref.read current
    u <- subscribe e \v -> do
      active <- Ref.read current
      when (active == this) (eff v)
      k v
    Ref.modify_ (add 1) current
    pure u

widgetInput :: Widget
widgetInput { interface, attrs } = do
  let
    io =
      { grammar: adaptInterface grammarCodec (interface "grammar")
      , producible: adaptInterface producibleCodec (interface "producible")
      , states: adaptInterface statesCodec (interface "states")
      , stateIndex: adaptInterface (mappy intStringCodec CA.int) (interface "stateIndex")
      , allTokens: adaptInterface (CA.array CA.codePoint) (interface "allTokens")
      , allNTs: adaptInterface (CA.array nonEmptyStringCodec) (interface "allNTs")

      , input: adaptInterface CA.string (interface "input")
      , tokens: adaptInterface (maybeCodec (listCodec CA.codePoint)) (interface "tokens")
      , tokens': adaptInterface (maybeCodec (listCodec CA.codePoint)) (interface "tokens'")
      , parseSteps: adaptInterface (maybeCodec parseStepsCodec) (interface "parseSteps")
      , stateId: adaptInterface CA.int (interface "stateId")
      , state: adaptInterface (maybeCodec stateInfoCodec) (interface "state")
      , validTokens: adaptInterface (setCodec CA.codePoint) (interface "validTokens")
      , validNTs: adaptInterface (setCodec nonEmptyStringCodec) (interface "validNTs")
      }
    replace = defer \_ ->
      let
        grammar = sampleGrammar
        c = computeGrammar grammar
      in
        { grammar, states: c.states, stateIndex: c.stateIndex }
    sendOthers input = do
      others <- fromMaybe' (\_ -> force replace) <$> unwrap do
        { grammar: _, states: _, stateIndex: _ }
          <$> Compose io.grammar.current
          <*> Compose io.states.current
          <*> Compose io.stateIndex.current
      let
        c = computeInput others input
      io.tokens.send c.tokens
      io.tokens'.send c.tokens'
      io.parseSteps.send c.parseSteps
      io.stateId.send c.stateId
      io.state.send c.state
      io.validTokens.send c.validTokens
      io.validNTs.send c.validNTs
  receiver <- sideKick sendOthers io.input.receive
  initialGrammar <- fromMaybe sampleGrammar <$> io.grammar.current
  fallbackInput <- fromMaybe' (\_ -> sampleS (withProducible initialGrammar)) <<< Json.toString <$> attrs "input"
  initialInput <- fromMaybe fallbackInput <$> io.input.current
  grammarStream <- sideKick (\_ -> io.input.current >>= fromMaybe initialInput >>> sendOthers) io.grammar.receive
  let
    sendInput = sendOthers <> io.input.send
    upstream =
      { send: sendInput
      -- it turns out that inputComponent never actually subscribes to
      -- grammar, so we need to force `sideKick` to happen
      , receive: sampleOn grammarStream (const <$> receiver)
      , current: io.input.current
      }
  -- For all of the other data, it might not be updated (e.g. because we only
  -- received a grammar from the query)
  sendOthers initialInput
  pure $ SafeNut do
    inputComponent initialInput upstream.receive upstream.send
      { grammar: fromEvent grammarStream
      , states: fromEvent io.states.receive
      , tokens: fromEvent io.tokens.loopback
      , tokens': fromEvent io.tokens'.loopback
      , parseSteps: fromEvent io.parseSteps.loopback
      , stateId: fromEvent io.stateId.loopback
      , state: fromEvent io.stateId.loopback
      , allTokens: fromEvent io.allTokens.receive
      , allNTs: fromEvent io.allNTs.receive
      , validTokens: fromEvent io.validTokens.loopback
      , validNTs: fromEvent io.validNTs.loopback
      , producible: fromEvent io.producible.receive
      }

widgets :: Object Widget
widgets = Object.fromFoldable $ map (lmap (append "Parser."))
  [ "Grammar" /\ widgetGrammar
  , "Input" /\ widgetInput
  , "Random" /\ withProducibleSendTokens randomComponent
  , "Explorer" /\ withProducibleSendTokens explorerComponent
  , "StateTable" /\ widgetStateTable
  , "ParseTable" /\ widgetParseTable
  ]

withGrammar :: (forall s m lock payload. Korok s m => SAugmented -> Domable m lock payload) -> Widget
withGrammar component { interface } = do
  let
    grammarEvent = filterMap (hush <<< decode grammarCodec) (interface "grammar").receive
  pure $ SafeNut (switcher component (bang sampleGrammar <|> fromEvent grammarEvent))

withProducibleSendTokens :: (forall s m lock payload. Korok s m => SProducible -> (Array CodePoint -> Effect Unit) -> Domable m lock payload) -> Widget
withProducibleSendTokens component { interface } = do
  let
    grammarEvent = filterMap (hush <<< decode producibleCodec) (interface "producible").receive
    sendTokens = (interface "input").send <<< Json.fromString <<< String.fromCodePointArray
  pure $ SafeNut (switcher (flip component sendTokens) (bang (withProducible sampleGrammar) <|> fromEvent grammarEvent))

spotlightBeh :: forall s m a b. MonadST s m => Ord a => AnEvent m a -> ((a -> AnEvent m Boolean) -> b) -> AnEvent m b
spotlightBeh e f = sweep (spotlightChange e) \g -> f \a ->
  toggle false $ keepLatest $
    -- query the behavioral event for its current value
    -- before only listening to sweeps
    bang (unit <$ filter (eq a) e) <|> bang (g a)

widgetStateTable :: Widget
widgetStateTable { interface } = do
  let
    stateIdI = adaptInterface CA.int (interface "stateId")
    currentGetCurrentState = spotlightBeh stateIdI.receive identity
    currentStates = filterMap (hush <<< decode statesCodec) (interface "states").receive
    currentStatesAndGetState = (sampleOn currentGetCurrentState (map (/\) currentStates))
  pure $ SafeNut (switcher (\(x /\ getCurrentState) -> renderStateTable { getCurrentState: fromEvent <<< getCurrentState } x) (fromEvent currentStatesAndGetState))

widgetParseTable :: Widget
widgetParseTable { interface } = do
  let
    stateIdI = adaptInterface CA.int (interface "stateId")
    currentGetCurrentState = spotlightBeh stateIdI.receive identity
    currentStates = filterMap (hush <<< decode statesCodec) (interface "states").receive
    currentStatesAndGetState = (sampleOn currentGetCurrentState (map (/\) currentStates))
    currentGrammar = filterMap (hush <<< decode grammarCodec) (interface "grammar").receive
  initialGrammar <- fromMaybe sampleGrammar <<< join <<< map (hush <<< decode grammarCodec) <$> (interface "grammar").current
  pure $ SafeNut (switcher (\(grammar /\ x /\ getCurrentState) -> renderParseTable { getCurrentState: fromEvent <$> getCurrentState } grammar x) (flip sampleOn ((/\) <<< _.augmented <$> (bang initialGrammar <|> fromEvent currentGrammar)) (fromEvent currentStatesAndGetState)))

inputComponent
  :: forall r s m lock payload
   . Korok s m
  => String
  -> AnEvent Effect String
  -> (String -> Effect Unit)
  ->
    { grammar :: AnEvent m SAugmented
    , states :: AnEvent m SStates
    , tokens :: AnEvent m (Maybe (List CodePoint))
    , tokens' :: AnEvent m (Maybe (List CodePoint))
    , parseSteps :: AnEvent m (Maybe SCParseSteps)
    , stateId :: AnEvent m Int
    , state :: AnEvent m Int
    , allTokens :: AnEvent m (Array CodePoint)
    , allNTs :: AnEvent m (Array NonEmptyString)
    , validTokens :: AnEvent m (Set CodePoint)
    , validNTs :: AnEvent m (Set NonEmptyString)
    , producible :: AnEvent m SProducible
    | r
    }
  -> Domable m lock payload
inputComponent initialInput inputStream sendInput current =
  bussed \pushInput localInput ->
    envy $ memoBeh (fromEvent inputStream <|> localInput) initialInput \currentValue -> do
      let
        currentValue' = notFrom localInput currentValue
        currentParseSteps = current.parseSteps
        currentGrammarTokens = current.allTokens
        currentGrammarNTs = current.allNTs
        currentValidTokens = current.validTokens
        currentValidNTs = current.validNTs
        currentProducible = current.producible

        renderTokenHere mtok = do
          let
            onClick = sampleJITE currentValue $ bang $ ReaderT \v ->
              pushInput <> sendInput $ case mtok of
                Just tok -> v <> String.singleton tok
                Nothing -> String.take (String.length v - 1) v
            valid = case mtok of
              Just tok -> currentValidTokens <#> Set.member tok
              Nothing -> currentValue <#> not String.null
            toktext = case mtok of
              Nothing -> codePointFromChar '⌫'
              Just tok -> tok
          renderTok' (map (append "clickable") $ valid <#> if _ then "" else " unusable") (Just <$> onClick) toktext
        renderNTHere nt = do
          let
            onClick = sampleJITE currentProducible $ sampleJITE currentValue $ bang $ ReaderT \v -> ReaderT \prod -> do
              genned <- map (maybe "" String.fromCodePointArray) $ sequence $
                QC.randomSampleOne <$> genNT prod.produced nt
              pushInput <> sendInput $ (v <> genned)
            valid = currentValidNTs <#> Set.member nt
          renderNT' (map (append "clickable") $ valid <#> if _ then "" else " unusable") (Just <$> onClick) nt
      D.div_
        [ D.div_
            [ D.span_ $ pure $
                switcher (\x -> fixed $ ([ Nothing ] <> map Just x) <#> renderTokenHere) currentGrammarTokens
            , D.span_ $ pure $
                switcher (\x -> fixed $ x <#> renderNTHere) currentGrammarNTs
            ]
        , D.div_
            [ D.span (D.Class !:= "terminal") [ inputC' "Source text" "" currentValue' (pushInput <> sendInput) ] ]
        , D.div_ $ pure $ currentParseSteps `flip switcher` \todaysSteps ->
            let
              contentAsMonad = showMaybeParseSteps $ todaysSteps
              -- Run the first layer of the monad, to get the number of items being rendered up-front
              contentAsMonad2 /\ nEntities = runTrampoline (runStateT contentAsMonad 0)
            in
              vbussed (Proxy :: _ ParsedUIAction) \pPush pEvent ->
                let
                  -- Maintain the current index, clamped between 0 and nEntitities
                  -- (Note: it is automatically reset, since `switcher` resubscribes,
                  -- creating new state for it)
                  startState = pEvent.startState <|> bang Nothing
                  rate = pEvent.rate <|> bang 1.0
                  animationTick = compact $ mapAccum
                    ( \i@(tf /\ { beats: Beats beats }) { target: Beats target' } ->
                        let
                          target = if tf then 0.0 else target'
                        in
                          if target > beats then ({ target: Beats target } /\ Nothing)
                          else ({ target: Beats (target + 1.0) } /\ Just i)
                    )
                    pEvent.animationTick
                    { target: Beats 0.0 }
                  currentIndex = dedupOn (eq `on` fst) $ bang ((nEntities - 1) /\ Initial) <|>
                    mapAccum
                      (\(f /\ a) x -> let fx = clamp 0 (nEntities - 1) (f x) in fx /\ fx /\ a)
                      ( oneOf
                          [ ((_ - 1) /\ Toggle) <$ pEvent.toggleLeft
                          , ((_ + 1) /\ Toggle) <$ pEvent.toggleRight
                          -- if we're starting and at the end of a play, loop back to the beginning
                          , (\(tf /\ _) -> if tf then ((\n -> if n == nEntities - 1 then 0 else n + 1) /\ Play) else ((_ + 1) /\ Play)) <$> animationTick
                          , (floor >>> const >>> (_ /\ Slider)) <$> pEvent.slider
                          ]
                      )
                      (nEntities - 1)
                in
                  -- Memoize it and run it through `spotlight` to toggle each
                  -- item individually
                  envy $ keepLatest $ memoize currentIndex \stackIndex ->
                    spotlight false (map fst stackIndex) \sweeper ->
                      let
                        content = contentAsMonad2 sweeper
                      in
                        D.div_
                          [ D.div_
                              [ D.button
                                  ( oneOf
                                      [ (biSampleOn rate ((/\) <$> startState)) <#> \(s /\ rt) -> D.OnClick := do
                                          case s of
                                            Just unsub -> do
                                              unsub
                                              pPush.startState Nothing
                                            Nothing -> do
                                              let toSeconds = unInstant >>> unwrap >>> (_ / 1000.0)
                                              t <- toSeconds <$> now
                                              sub <- subscribe
                                                ( selfDestruct (\((isStart /\ _) /\ ci) -> (fst ci == (nEntities - 1) && not isStart)) (pPush.startState Nothing)
                                                    ( sampleOn (toEvent currentIndex)
                                                        ( (/\) <$> mapAccum (\i tf -> false /\ tf /\ i)
                                                            ( timeFromRate (step rt $ toEvent pEvent.rate)
                                                                ( _.time
                                                                    >>> toSeconds
                                                                    >>> (_ - t)
                                                                    >>> Seconds <$> withTime (animationFrame)
                                                                )
                                                            )
                                                            true
                                                        )
                                                    )
                                                )
                                                \(info /\ _) -> pPush.animationTick info
                                              pPush.startState (Just sub)
                                      ]
                                  )
                                  [ text
                                      ( startState <#> case _ of
                                          Just _ -> "Pause"
                                          Nothing -> "Play"
                                      )
                                  ]
                              ]
                          , D.div_
                              [ text_ "Speed"
                              , D.span_ $ join $ map
                                  ( \(n /\ l) ->
                                      [ D.input
                                          ( oneOfMap bang
                                              [ D.Xtype := "radio"
                                              , D.Checked := show (l == "1x")
                                              , D.Name := "speed"
                                              , D.Value := show n
                                              , D.OnClick := cb \_ -> pPush.rate n
                                              ]
                                          )
                                          []
                                      , D.label_ [ text_ l ]
                                      ]
                                  )
                                  [ 1.0 /\ "1x", (1.0 / e) /\ "ex", (1.0 / pi) /\ "pix" ]
                              ]
                          , D.div_
                              [ D.input
                                  ( oneOf
                                      [ D.Xtype !:= "range"
                                      , D.Min !:= "0"
                                      , D.Max !:= show (nEntities - 1)
                                      , D.Value !:= show (nEntities - 1)
                                      , stackIndex
                                          # filterMap case _ of
                                              _ /\ Slider -> Nothing
                                              x /\ _ -> Just x
                                          <#> (\si -> D.Value := show si)
                                      , slider $ startState <#> case _ of
                                          Nothing -> pPush.slider
                                          Just unsub -> \n -> pPush.slider n
                                            *> unsub
                                            *> pPush.startState Nothing
                                      ]
                                  )
                                  []
                              ]
                          , let
                              clickF f = click $
                                startState <#>
                                  ( case _ of
                                      Nothing -> f unit
                                      Just unsub -> f unit
                                        *> unsub
                                        *> pPush.startState Nothing
                                  )
                            in
                              D.div_
                                [ D.button
                                    (oneOf [ clickF $ pPush.toggleLeft ])
                                    [ text_ "<" ]
                                , D.button (oneOf [ clickF $ pPush.toggleRight ])
                                    [ text_ ">" ]
                                ]
                          , D.div (D.Class !:= "parse-steps") [ content ]
                          ]
        ]
