module Parser.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Monad.Gen as Gen
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trampoline (Trampoline, runTrampoline)
import Control.Plus (empty)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (bimap, lmap)
import Data.Codec (decode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either, hush, note)
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, any, foldMap, oneOfMap, sequence_, traverse_)
import Data.Foldable (length) as Foldable
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor (mapFlipped)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor)
import Data.Int as Int
import Data.Lazy (defer, force)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, maybe)
import Data.Newtype (unwrap)
import Data.SequenceRecord (sequenceRecord)
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint, codePointFromChar)
import Data.String as String
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (mapAccumL, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Class.Console as Log
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parser.Algorithms (addEOF', calculateStates, fromSeed', fromString', gatherNonTerminals', gatherNonTerminals_, gatherTokens', gatherTokens_, getResultC, indexStates', longestFirst, numberStatesBy, parseDefinition, parseIntoGrammar, toAdvanceTo, toTable, withProducible)
import Parser.Codecs (grammarCodec, intStringCodec, listCodec, mappy, maybeCodec, nonEmptyStringCodec, parseStepsCodec, producibleCodec, setCodec, stateInfoCodec, statesCodec)
import Parser.Proto (ParseSteps(..), Stack(..), parseSteps, topOf)
import Parser.Random (genNT, sampleS)
import Parser.Samples (defaultEOF, defaultTopName, defaultTopRName)
import Parser.Types (AST(..), CST(..), Grammar(..), Part(..), SAST, SAugmented, SCParseSteps, SCST, SCStack, SFragment, SGrammar, SProducible, SState, SStateInfo, SStateItem, SStates, SZipper, ShiftReduce(..), State(..), States(..), Zipper(..), prune, unNonTerminal, unSPart, unTerminal)
import Partial.Unsafe (unsafePartial)
import Random.LCG as LCG
import Riverdragon.Dragon.Bones (AttrProp, Dragon, (>@), (@<))
import Riverdragon.Dragon.Bones as D
import Riverdragon.Dragon.Wings (eggy)
import Riverdragon.River (Lake, createRiver, createRiverStore, foldStream, sampleOnRight, selfGating, subscribe, (<**>))
import Riverdragon.River.Beyond (dedup, dedupOn, interval, animationLoop)
import Test.QuickCheck.Gen as QC
import Unsafe.Coerce (unsafeCoerce)
import Widget (Widget, adaptInterface)

data StepAction = Initial | Toggle | Slider | Play
derive instance Eq StepAction

type Dragons = Array Dragon

type Dragonss = Array (Array Dragon)

inputC :: String -> String -> String -> (String -> Effect Unit) -> Dragon
inputC label placeholder initialValue onInput =
  D.label' [ D.className "input-wrapper text" ] $ D.Fragment
    [ D.span $ D.text label
    , D.input'
        [ D.placeholder' if placeholder == "" then empty else pure placeholder
        , D.value' if initialValue == "" then empty else pure initialValue
        , D.onInputValue onInput
        ]
    ]

inputValidated ::
  String ->
  String ->
  String ->
  String ->
  Lake String ->
  (String -> Effect Unit) ->
  Dragon
inputValidated cls label placeholder initialValue valid onInput =
  D.label'
    [ D.className' $
        (append "input-wrapper text" <<< (eq "" >>> if _ then "" else " invalid"))
        <$> (pure "" <|> valid)
    ] $ D.Fragment
    [ D.span $ D.text label
    , D.input'
        [ D.placeholder' if placeholder == "" then empty else pure placeholder
        , D.value' if initialValue == "" then empty else pure initialValue
        , D.onInputValue onInput
        , D.className cls
        ]
    , D.span' [ D.className "error" ] (D.Text valid)
    ]

inputC' :: String -> String -> Lake String -> (String -> Effect Unit) -> Dragon
inputC' label placeholder initialValue onInput =
  D.label' [ D.className "input-wrapper text" ] $ D.Fragment
    [ D.span (D.text label)
    , D.input'
        [ D.placeholder' if placeholder == "" then empty else pure placeholder
        , D.value' initialValue
        , D.onInputValue onInput
        ]
    ]

type Header nt tok = Array tok /\ Array nt

getHeader :: forall s nt r tok. Ord nt => Ord tok => States s Unit nt r tok -> Header nt tok
getHeader (States states) = bimap Array.nub Array.nub $
  states # foldMap \{ items: State items } -> items # foldMap \item ->
    ([] /\ [ item.pName ]) <> foldZipper fromPart item.rule
  where
  foldZipper f (Zipper l r) = foldMap f l <> foldMap f r
  fromPart (NonTerminal nt) = [] /\ [ nt ]
  fromPart (Terminal tok) = [ tok ] /\ []
  fromPart (InterTerminal _) = [] /\ []

col :: forall a. Eq a => a -> a -> Lake AttrProp
col j i =
  if i == j then D.className "first" else empty

renderParseTable :: forall r.
  { getCurrentState :: Int -> Lake Boolean | r } ->
  SGrammar ->
  SStates ->
  Dragon
renderParseTable info (MkGrammar grammar) (States states) = eggy \_ -> do
  { stream: event, send: push } <- createRiverStore $ Just Nothing
  let
    stateHighlighted = pure Nothing <|> event
    terminals /\ nonTerminals = getHeader (States states)
    renderTerminals x = renderTok mempty x
    gatherRules nt = grammar # filterMap \r ->
      if r.pName == nt then Just r.rName else Nothing
    renderNonTerminals x =
      D.div' [ D.className "pileup" ] $ D.Fragment $
        (map (\y -> renderRule mempty y) (gatherRules x)) <>
          [ renderNT mempty x ]
    renderStHere s =
      D.span'
        [ D.className "state hoverable"
        , D.on_"mouseenter" $ const $ push (Just s)
        , D.on_"mouseleave" $ const $ push Nothing
        ]
        (D.text (show s))
    renderShiftReduce Nothing = D.span mempty
    renderShiftReduce (Just (Shift s)) = D.span $ D.Fragment [ renderCmd mempty "s", renderStHere s ]
    renderShiftReduce (Just (Reduces rs)) =
      D.span' [ if NEA.length rs > 1 then D.className "conflict" else empty ] $ D.Fragment $
        rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ]
    renderShiftReduce (Just (ShiftReduces s rs)) =
      D.span' [ D.className "conflict" ] $ D.Fragment $
        [ renderCmd mempty "s", renderStHere s ] <> (rs # foldMap \r -> [ renderCmd mempty "r", renderRule mempty r ])
    renderGoto Nothing = []
    renderGoto (Just s) = [ renderCmd mempty "g", renderStHere s ]
    cols state =
      let
        forTerminal tok = map (snd <<< snd) <<< snd <$> Map.lookup tok (unwrap state.advance)
        forNonTerminal nt = Map.lookup nt state.receive
      in
        map (pure <<< renderShiftReduce <<< forTerminal) terminals <> map (renderGoto <<< forNonTerminal) nonTerminals

    header = D.tr $ D.Fragment $ mapWithIndex (\i -> D.th' [ col (Array.length terminals + 1) i ]) $
      [ D.text "" ] <> map renderTerminals terminals <> map renderNonTerminals nonTerminals
    clsFor s =
      (<**>)
        ((if _ then " active " else "") <$> info.getCurrentState s)
        $ stateHighlighted <#> \s' -> append $
            if s' == Just s then " hover " else ""
    rows = states <#> \state ->
      D.tr' [ D.className' $ clsFor state.sName] $ D.Fragment $
        Array.cons (D.th $ renderStHere state.sName) $
          cols state # mapWithIndex
            \i -> D.td' [ col (Array.length terminals) i ] <<< D.Fragment
  pure $ D.table' [ D.className "data-table parse-table" ] $ D.Fragment
    [ D.thead header
    , D.tbody $ D.Fragment rows
    ]

type StartingTick = Boolean

data TodoAction = Prioritize | Delete

showStack :: SCStack -> Dragon
showStack i = D.span' [ D.className "full stack" ] (go i)
  where
  go (Zero state) = D.sub (renderSt mempty state)
  go (Snoc stack tok state) = go stack
    <> renderCSTTree tok
    <> D.sub (renderSt mempty state)

renderStackItem :: Either CodePoint SAST -> Dragon
renderStackItem (Left x) = renderTok mempty x
renderStackItem (Right x) = renderASTTree x

renderAST :: SAST -> Dragon
renderAST (Layer (_ /\ r) []) = D.span' [ D.className "layer" ] (renderRule mempty r)
renderAST (Layer (_ /\ r) cs) =
  D.span' [ D.className "layer" ] $ D.Fragment $ join
    [ pure $ renderMeta mempty "("
    , pure $ renderRule mempty r
    , cs # foldMap \c -> [ D.text " ", renderAST c ]
    , pure $ renderMeta mempty ")"
    ]

renderASTTree :: SAST -> Dragon
renderASTTree ast =
  D.ol' [ D.className "AST" ] $
    D.li (renderASTChild ast)

renderASTChild :: SAST -> Dragon
renderASTChild (Layer (_ /\ r) []) =
  D.span' [ D.className "leaf node" ]
      (renderRule mempty r)
renderASTChild (Layer (_ /\ r) cs) = D.Fragment
  [ D.span' [ D.className "node" ]
      (renderRule mempty r)
  , D.ol' [ D.className "layer" ] $ D.Fragment $
      cs <#> \c -> D.li (renderASTChild c)
  ]

renderCSTTree :: SCST -> Dragon
renderCSTTree ast =
  D.ol' [ D.className "AST CST" ]
    (D.li (renderCSTChild ast))

renderCSTChild :: SCST -> Dragon
renderCSTChild (Leaf tok) =
  D.span' [ D.className "leaf node" ]
    (renderTok mempty tok)
renderCSTChild (Branch (_ /\ r) cs) = D.Fragment
  [ D.span' [ D.className "node" ]
      (renderRule mempty r)
  , D.ol' [ D.className "layer" ] $ D.Fragment $
      cs <#> \c -> D.li (renderCSTChild c)
  ]

showMaybeStack :: Maybe SCStack -> Dragon
showMaybeStack Nothing = D.text "Parse error"
showMaybeStack (Just stack) = showStack stack

showMaybeParseSteps :: Maybe SCParseSteps -> SuperStack (Dragon)
showMaybeParseSteps Nothing = pure (pure (D.text "Parse error"))
showMaybeParseSteps (Just stack) = showParseSteps stack

getVisibilityAndIncrement :: SuperStack (Int /\ Lake AttrProp)
getVisibilityAndIncrement = getVisibilityAndIncrement' ""

getVisibilityAndIncrement' :: String -> SuperStack (Int /\ Lake AttrProp)
getVisibilityAndIncrement' s = do
  n <- get
  put (n + 1)
  pure
    ( \f -> n /\
        ( D.className' $ f n <#> \v -> (s <> if v then "" else " hidden")
        )
    )

getVisibility :: SuperStack (Int /\ Lake AttrProp)
getVisibility = do
  n <- get
  pure
    ( \f -> n /\
        ( D.className' $ f n <#> \v -> (if v then "" else " hidden")
        )
    )

showParseStep :: forall r.
  Either
    (Maybe SCStack)
    { inputs :: List CodePoint
    , stack :: SCStack
    | r
    } ->
  SuperStack Dragon
showParseStep (Left Nothing) = do
  getVisibilityAndIncrement <#> map \(n /\ vi) ->
    D.div' [ vi ] $ D.text $ ("Step " <> show n <> ": ") <> "Parse error"
showParseStep (Left (Just v)) = do
  getVisibilityAndIncrement <#> map \(_ /\ vi) ->
    case getResultC v of
      Just r | Right p <- map snd <$> prune r ->
        D.div' [ vi ] $ D.text "Step the last: " <> renderCSTTree r
      _ ->
        D.div' [ vi ] $ D.text $ "Step the last: " <> "Something went wrong"
showParseStep (Right { stack, inputs }) = do
  getVisibilityAndIncrement' "flex justify-between" <#> map \(n /\ vi) ->
    D.div' [ vi ] $ D.Fragment
      [ D.div $ D.Fragment
        [ D.text ("Step " <> show n <> ": ")
        , showStack stack
        ]
      , D.div (foldMap (renderTok mempty) inputs)
      ]

showParseTransition ::
  Int /\ Either CodePoint (NonEmptyString /\ String) ->
  SuperStack Dragon
showParseTransition (s /\ Left tok) = do
  getVisibility <#> map \(_ /\ vi) ->
    D.span' [ vi ] $ D.Fragment
      [ {- renderTok mempty tok, D.text " ", -} renderCmd mempty "s", renderSt mempty s ]
showParseTransition (s /\ Right (_ /\ rule)) = do
  getVisibility <#> map \(_ /\ vi) ->
    D.span' [ vi ] $ D.Fragment
      [ renderCmd mempty "r"
      , renderRule mempty rule
      , renderMeta mempty " —> "
      , renderCmd mempty "g"
      , renderSt mempty s
      ]

type SuperStack a = StateT Int Trampoline ((Int -> Lake Boolean) -> a)

showParseSteps :: SCParseSteps -> SuperStack Dragon
showParseSteps i = map (D.div' [ D.className "parse-steps" ]) <$> (go i)
  where
  go =
    let
      s v = showParseStep v
      t v = showParseTransition v
    in
      case _ of
        Error prev -> do
          s (Right prev) <> s (Left Nothing)
        Complete prev v -> do
          s (Right prev) <> s (Left (Just v))
        Step prev action more -> do
          s (Right prev) <> t (firstState more /\ action) <> go more

renderStateTable :: forall r. { getCurrentState :: Int -> Lake Boolean | r } -> SStates -> Dragon
renderStateTable info (States states) = do
  let
    mkTH n 0 0 = D.th' [ D.attr "rowspan" (show n) ]
    mkTH _ _ 0 = mempty
    mkTH _ _ _ = D.td
    stateClass sName = (if _ then "active" else "") <$> info.getCurrentState sName
    renderStateHere items =
      let
        n = Array.length items
      in
        items # mapWithIndex \j -> D.tr <<< D.Fragment <<< mapWithIndex (\i -> mkTH n j i)
  D.table' [ D.className "data-table state-table" ] $ D.Fragment $
    states <#> \s@{ sName, items } ->
      D.tbody' [ D.className' $ stateClass sName ] $ D.Fragment $
        renderStateHere $ renderState s items

renderState :: SStateInfo -> SState -> Dragonss
renderState s (State items) = (\j v -> renderItem s j v) `mapWithIndex` items

renderItem :: SStateInfo -> Int -> SStateItem -> Dragons
renderItem s j { pName, rName, rule: rule@(Zipper _ after), lookahead } =
  [ if j == 0 then renderSt mempty s.sName else D.text ""
  , renderNT mempty pName
  , renderMeta mempty ": "
  , renderZipper rule
  , renderLookahead (if Array.null after then " reducible" else "") (map snd lookahead)
  , D.span $ D.Fragment [ renderMeta mempty " #", renderRule mempty rName ]
  , D.span $ D.Fragment case toAdvanceTo s rule of
      Nothing -> []
      Just s' -> [ renderMeta mempty " —> ", renderSt mempty s' ]
  ]

renderZipper :: SZipper -> Dragon
renderZipper (Zipper before after) =
  D.span' [ D.className ("zipper" <> if Array.null after then " reducible" else "") ] $ D.Fragment
    [ D.span' [ D.className "parsed" ] $ D.Fragment $ before <#> \x -> renderPart mempty x
    , if Array.null after then D.text ""
      else
        D.span $ D.Fragment $ after <#> \x -> renderPart mempty x
    ]

renderLookahead :: String -> Array CodePoint -> Dragon
renderLookahead moreClass items =
  D.span' [ D.className $ append "lookahead" moreClass ] $
    D.Fragment $
      [ renderMeta mempty "{ " ]
      <> Array.intercalate [ renderMeta mempty ", " ] (items <#> \x -> [ renderTok mempty x ])
      <> [ renderMeta mempty " }" ]

--------------------------------------------------------------------------------

debug :: forall a. Show a => String -> Lake a -> Lake a
debug tag = map \a -> unsafePerformEffect (a <$ (Log.info (tag <> show a)))

debugJson :: String -> Lake Json.Json -> Lake Json.Json
debugJson tag = map \a -> unsafePerformEffect (a <$ (Log.info (tag <> Json.stringify a)))

unsafeDebug :: forall a. String -> Lake a -> Lake a
unsafeDebug tag = map \a -> unsafePerformEffect (a <$ (Log.info tag <* Log.info (unsafeCoerce a)))

silentDebug :: forall a. String -> Lake a -> Lake a
silentDebug tag = map \a -> unsafePerformEffect (a <$ Log.info tag)

renderAs :: String -> String -> Dragon
renderAs c t = D.span' [ D.className c ] (D.text t)

renderTok :: Maybe (Effect Unit) -> CodePoint -> Dragon
renderTok c t = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "terminal" <> if isJust c then " clickable" else ""
  ] (D.text (String.singleton t))

renderTok' :: Lake String -> Lake (Maybe (Effect Unit)) -> CodePoint -> Dragon
renderTok' cls c t = D.span'
  [ D.onClickE' (filterMap identity c)
  , D.className' (pure "terminal" <|> (append "terminal " <$> cls))
  ] (D.text (String.singleton t))

renderNT :: Maybe (Effect Unit) -> NonEmptyString -> Dragon
renderNT c nt = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "non-terminal" <> if isJust c then " clickable" else ""
  ] (D.text (NES.toString nt))

renderNT' :: Lake String -> Lake (Maybe (Effect Unit)) -> NonEmptyString -> Dragon
renderNT' cls c nt = D.span'
  [ D.onClickE' (filterMap identity c)
  , D.className' (pure "non-terminal" <|> (append "non-terminal " <$> cls))
  ] (D.text (NES.toString nt))

renderRule :: Maybe (Effect Unit) -> String -> Dragon
renderRule c r = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "rule" <> if isJust c then " clickable" else ""
  ] (D.text r)

renderMeta :: Maybe (Effect Unit) -> String -> Dragon
renderMeta c x = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "meta" <> if isJust c then " clickable" else ""
  ] (D.text x)

renderSt :: Maybe (Effect Unit) -> Int -> Dragon
renderSt c x = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "state" <> if isJust c then " clickable" else ""
  ] (D.text (show x))

renderSt' :: Lake String -> Maybe (Effect Unit) -> Int -> Dragon
renderSt' cls c x = D.span'
  [ maybe empty D.onClickE c
  , D.className' (pure "state" <|> (append "state " <$> cls))
  ] (D.text (show x))

renderPart :: Maybe (Effect Unit) -> Part Unit NonEmptyString CodePoint -> Dragon
renderPart c (NonTerminal nt) = renderNT c nt
renderPart c (Terminal t) = renderTok c t
renderPart _ (InterTerminal _) = mempty

renderCmd :: Maybe (Effect Unit) -> String -> Dragon
renderCmd c x = D.span'
  [ maybe empty D.onClickE c
  , D.className $ "cmd" <> if isJust c then " clickable" else ""
  ] (D.text x)

only :: forall a. Array a -> Maybe a
only [ a ] = Just a
only _ = Nothing

findNext :: Set String -> String -> Int -> String
findNext avoid pre n =
  let
    pren = pre <> show n
  in
    if pren `Set.member` avoid then findNext avoid pre (n + 1) else pren

parseGrammar ::
  { top :: NonEmptyString
  , entry :: Maybe NonEmptyString
  , eof :: CodePoint
  , topName :: String
  } ->
  Array { pName :: NonEmptyString, rule :: String, rName :: String } ->
  Either ParseGrammarError SAugmented
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
      , lookbehind: mempty
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

list ::
  forall f.
    Foldable f =>
  f (Dragon) -> Array (Dragon)
list = Array.fromFoldable >>> case _ of
  [ a ] -> [ a ]
  [ a, b ] -> [ a, D.text " & ", b ]
  as -> case Array.unsnoc as of
    Nothing -> []
    Just { init: as', last: b } ->
      Array.intercalate [ D.text ", " ] (map pure as') <> [ D.text " & ", b ]

rulesAtFault :: ParseGrammarError -> Array String
rulesAtFault = case _ of
  NoRules -> []
  ReferencesEOF rules -> NEA.toArray rules
  EOFNonTerminal rules -> NEA.toArray rules
  ReferencesTop rules -> NEA.toArray rules
  MissingEntry _ -> []
  TopClash -> []
  RuleNamesUnique rules -> NEA.toArray rules

parseGrammarError :: ParseGrammarError -> Dragons
parseGrammarError NoRules = [ D.text "Need at least one rule in the grammar." ]
parseGrammarError (ReferencesEOF rules) =
  [ D.text "The final token must only be referenced in the top rule, but it was also referenced in " ]
    <> list (map (\x -> D.Fragment [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ D.text "." ]
parseGrammarError (EOFNonTerminal rules) =
  [ D.text "The final token must not be a nonterminal, but it appeared in " ]
    <> list (map (\x -> D.Fragment [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ D.text "." ]
parseGrammarError (ReferencesTop rules) =
  [ D.text "The top nonterminal must not appear in the rest of the grammar, but it was also referenced in " ]
    <> list (map (\x -> D.Fragment [ renderMeta mempty "#", renderRule mempty x ]) rules)
    <>
      [ D.text "." ]
parseGrammarError (MissingEntry nt) =
  [ D.text "Entry point ", renderNT mempty nt, D.text " is not present as a nonterminal in the grammar." ]
parseGrammarError TopClash = [ D.text "Entry point must be different from the top rule." ]
parseGrammarError (RuleNamesUnique rules) =
  [ D.text "Rule names must be unique, but " ] <> list (map (\x -> D.Fragment [ renderMeta mempty "#", renderRule mempty x ]) rules) <> [ D.text " ", D.text (pl rules "was" "were"), D.text " duplicated" ]

grammarComponent ::
  String ->
  SAugmented ->
  Lake SAugmented ->
  (SAugmented -> Effect Unit) ->
  Dragon
grammarComponent buttonText reallyInitialGrammar forceGrammar sendGrammar =
  (pure reallyInitialGrammar <|> forceGrammar) >@ \initialGrammar -> eggy \shell -> do
    let
      createInput = sequenceRecord
        { value: createRiverStore $ Just ""
        , error: createRiverStore $ Just ""
        }
    inputs <- sequenceRecord
      { pName: createInput
      , rule: createInput
      , rName: createInput
      , top: createInput
      , entry: createInput
      , eof: createInput
      , topName: createInput
      }
    actions <- sequenceRecord
      { errorMessage: createRiver -- Maybe ParseGrammarError
      , addRule: createRiver -- Int /\ { pName :: NonEmptyString, rule :: String, rName :: String }
      , removeRule: createRiver -- Int
      , troubleRules: createRiverStore $ Just []
      }
    let
      changeRule =
        ( \rules ->
            case _ of
              Left new -> Array.snoc rules new
              Right remove -> Array.filter (fst >>> not eq remove) rules
        )
      ruleChanges = (Left <$> actions.addRule.stream <|> Right <$> actions.removeRule.stream)
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
        (<**>) (pure "" <|> inputs.pName.value.stream)
          $ (<**>) (pure "" <|> inputs.rule.value.stream)
          $
            (pure "" <|> inputs.rName.value.stream) <#> \rName rule pName ->
              { rName, rule, pName }
      currentTop =
        (<**>) (pure initialTop.top <|> inputs.top.value.stream)
          $ (<**>) (pure initialTop.entry <|> inputs.entry.value.stream)
          $ (<**>) (pure initialTop.eof <|> inputs.eof.value.stream)
          $ (<**>) (pure initialTop.topName <|> inputs.topName.value.stream)
          $ pure { topName: _, eof: _, entry: _, top: _ }
      counted = add (Array.length initialRules) <$>
        (pure 0 <|> (add 1 <$> fst <$> actions.addRule.stream))
    getCurrentText <- shell.storeLast mempty currentText
    getCounted <- shell.storeLast 12341 counted
    currentTop <- shell.instStore currentTop
    currentRules <- shell.inst $ foldStream initialRules ruleChanges changeRule
    let
      currentNTs = dedup $ map longestFirst $
        (<**>)
          (map (_.pName <<< snd) <$> currentRules)
          (append <<< pure <<< fromMaybe defaultTopName <<< NES.fromString <<< _.top <$> currentTop)
      currentTopParsed = (<**>) currentRules $ currentTop <#> \r rules ->
        { top: fromMaybe defaultTopName $ NES.fromString r.top
        , entry: NES.fromString r.entry <|> (Array.head rules <#> snd >>> _.pName)
        , eof: fromMaybe defaultEOF $ only $ String.toCodePointArray r.eof
        , topName: r.topName
        }
      currentGrammar = (<**>) (map snd <$> currentRules) (currentTopParsed <#> parseGrammar)

      putEofError v = inputs.eof.error.send
        if String.length v <= 1 then "" else
        "Must be a single character (Unicode code point)"
    pure $ D.div $ D.Fragment
      [ D.div $ D.Fragment
          [ join (inputValidated "non-terminal" "Top name") initialTop.top inputs.top.error.stream (const (inputs.top.error.send "") <> inputs.top.value.send)
          , inputValidated "non-terminal" "Entry point" "" initialTop.entry inputs.entry.error.stream (const (inputs.entry.error.send "") <> inputs.entry.value.send)
          , join (inputValidated "terminal" "Final token") initialTop.eof inputs.eof.error.stream (putEofError <> inputs.eof.value.send)
          , join (inputValidated "rule" "Top rule name") initialTop.topName inputs.topName.error.stream (const (inputs.topName.error.send "") <> inputs.topName.value.send)
          ]
      , D.table' [ D.className "data-table grammar" ] $ D.Fragment
          [ D.tr $ D.Fragment
              [ D.td $ currentTopParsed <#> _.top >@ renderNT mempty
              , D.td $ renderMeta mempty " : "
              , D.td $ D.Fragment
                  [ currentTopParsed <#> _.entry >@ (maybe (D.text "—") (renderNT mempty))
                  , currentTopParsed <#> _.eof >@ renderTok mempty
                  ]
              , D.td $ D.Fragment
                  [ renderMeta mempty " #"
                  , currentTopParsed <#> _.topName >@ renderRule mempty
                  ]
              ]
          , D.tbody $ D.Appending $ mapFlipped
              (oneOfMap pure initialRules <|> actions.addRule.stream)
              \(i /\ txt) -> eggy \_ -> do
                this <- createRiver
                pure $ D.Replacing $
                  ( pure $ D.tr'
                      [ D.className' (actions.troubleRules.stream <#> \rit -> if Array.elem txt.rName rit then "trouble" else "") ]
                      $ D.Fragment $ map D.td $
                      [ renderNT mempty txt.pName
                      , renderMeta mempty " : "
                      , currentNTs >@ \nts ->
                          D.span $ D.Fragment $
                            renderPart mempty <$> parseDefinition nts txt.rule
                      , D.span $ D.Fragment
                          [ renderMeta mempty " #"
                          , renderRule mempty txt.rName
                          ]
                      , D.span $ D.Fragment
                          [ D.text " "
                          , D.buttonN
                              "delete"
                              (this.send mempty *> actions.removeRule.send i *> actions.troubleRules.send [])
                              "Delete"
                          ]
                      ]
                  ) <|> this.stream
          ]
      , D.div $ D.Fragment
          [ inputValidated "non-terminal" "Nonterminal name" "" "" inputs.pName.error.stream (const (inputs.pName.error.send "") <> inputs.pName.value.send)
          , renderMeta mempty " : "
          , inputValidated "terminal" "Value" "" "" inputs.rule.error.stream (const (inputs.rule.error.send "") <> inputs.rule.value.send)
          , renderMeta mempty " #"
          , inputValidated "rule" "Rule name" "" "" inputs.rName.error.stream (const (inputs.rName.error.send "") <> inputs.rName.value.send)
          , D.button'
              [ D.className "big add"
              , D.onClickE do
                  text <- getCurrentText
                  i <- getCounted
                  case NES.fromString text.pName of
                    Nothing -> do
                      inputs.pName.error.send "Name for non-terminal must be non-empty."
                    -- TODO: findNext ruleNames text.pName 1
                    Just pName -> do
                      actions.errorMessage.send Nothing
                      inputs.pName.error.send ""
                      inputs.rule.error.send ""
                      inputs.rName.error.send ""
                      actions.troubleRules.send []
                      actions.addRule.send (i /\ text { pName = pName })
              ]
              (D.text "Add rule")
          ]
      , D.br
      , if buttonText == "" then D.div mempty
        else
          D.div $ D.button'
            [ D.className "big"
            , D.onClickE' $ currentGrammar <#> \g -> do
                actions.errorMessage.send Nothing
                inputs.pName.error.send ""
                inputs.rule.error.send ""
                inputs.rName.error.send ""
                inputs.top.error.send ""
                inputs.entry.error.send ""
                inputs.eof.error.send ""
                inputs.topName.error.send ""
                actions.troubleRules.send []
                case g of
                  Left err -> do
                    actions.errorMessage.send (Just err)
                    actions.troubleRules.send (rulesAtFault err)
                    case err of
                      NoRules ->
                        pure unit
                      ReferencesEOF _ ->
                        inputs.eof.error.send "Must not appear in the rest of the grammar"
                      EOFNonTerminal _ ->
                        inputs.eof.error.send "Cannot appear as nonterminal"
                      ReferencesTop _ ->
                        inputs.top.error.send "Must not appear in the rest of the grammar"
                      MissingEntry _ ->
                        inputs.entry.error.send "Must name a nonterminal in the grammar"
                      TopClash ->
                        inputs.entry.error.send "Cannot be the top rule itself"
                      RuleNamesUnique _ ->
                        pure unit
                  Right g' -> do
                    sendGrammar g'
            ]
            (D.text buttonText)
      , actions.errorMessage.stream >@ case _ of
          Nothing -> D.div mempty
          Just e -> D.Fragment
            [ D.br
            , D.div' [ D.className "Error" ] $ D.Fragment $ parseGrammarError e
            ]
      ]

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

explorerComponent ::
  SProducible ->
  (Array CodePoint -> Effect Unit) ->
  Dragon
explorerComponent grammar sendUp = eggy \shell -> do
  let { produced: producedRules, grammar: { augmented: MkGrammar rules, start: { pName: entry } } } = grammar
  actions <- sequenceRecord
    { focus: createRiver
    , select: createRiverStore $ Just [ NonTerminal entry ]
    }
  let
    currentParts = actions.select.stream
    firstNonTerminal = Array.head <<< foldMapWithIndex
      \i v -> maybe [] (\r -> [ i /\ r ]) (unNonTerminal v)
  currentFocused <- shell.instStore
    (pure (Just (0 /\ entry)) <|> actions.focus.stream <|> map firstNonTerminal currentParts)
  let
    activity here = here <#> if _ then "active" else "inactive"
    renderPartHere i (NonTerminal nt) =
      D.span'
        [ D.className' (currentFocused <#> any (fst >>> eq i) >>> if _ then "selected" else "") ]
        (renderNT (Just (actions.focus.send (Just (i /\ nt)))) nt)
    renderPartHere _ (Terminal tok) = renderTok mempty tok
    renderPartHere _ (InterTerminal _) = mempty
    send = currentParts <#> \parts ->
      case traverse unTerminal parts of
        Nothing -> Nothing
        Just toks -> Just (sendUp toks)
  pure $ D.div $ D.Fragment
    [ (D.Fragment <<< mapWithIndex renderPartHere) @< currentParts
    , D.button'
        [ D.className' $ maybe "disabled" mempty <$> send
        , D.onClickE' $ sequence_ <$> send
        ]
        (D.text "Send")
    , D.buttonN "delete" (actions.select.send [ NonTerminal entry ]) "Reset"
    , D.table' [ D.className "data-table explorer-table" ] $ D.Fragment
        [ D.tbody $ D.Fragment $ rules <#> \rule -> do
            let
              focusHere = currentFocused # map (any (snd >>> eq rule.pName))
              replacement = sampleOnRight currentParts $ currentFocused <#> \mfoc parts -> do
                focused /\ nt <- mfoc
                guard $ nt == rule.pName
                guard $ focused <= Array.length parts
                pure $ Array.take focused parts <> rule.rule <> Array.drop (focused + 1) parts
            D.tr' [ D.className' $ activity focusHere ] $ D.Fragment $ map D.td
              [ renderNT mempty rule.pName
              , renderMeta mempty " : "
              , D.Fragment $ map (\x -> renderPart mempty x) rule.rule
              , D.span $ D.Fragment
                  [ renderMeta mempty " #"
                  , renderRule mempty rule.rName
                  ]
              , D.span $ D.Fragment
                  [ D.text " "
                  , D.button'
                      [ D.className' $ (append "select")
                          <$> (if _ then mempty else " disabled")
                          <$> focusHere
                      , D.onClickE' $ traverse_ actions.select.send <$> replacement
                      ]
                      (D.text "Choose")
                  ]
              , case Array.find (_.production >>> eq rule) producedRules of
                  Nothing -> D.text "Unproducible"
                  Just { produced } ->
                    D.span $ D.Fragment $ join
                      [ pure $ D.em $ D.text "e.g. "
                      , map (\x -> renderTok mempty x) produced
                      ]
              ]
        ]
    ]

type RandomAction =
  ( size :: Int
  , amt :: Int
  , randomMany :: Array (Array CodePoint)
  )

randomComponent ::
  SProducible ->
  (Array CodePoint -> Effect Unit) ->
  Dragon
randomComponent grammar sendUp = eggy \_ -> do
  let { produced: producedRules, grammar: { start: { pName: entry } } } = grammar
  let
    initialSize = 50
    initialAmt = 15
    randomProductions nt sz amt =
      genNT producedRules nt # traverse (Gen.resize (const sz) >>> QC.randomSample' amt)
    initial = genNT producedRules entry # maybe [] (QC.sample (LCG.mkSeed 1234) initialAmt)
  actions <- sequenceRecord
    { size: createRiverStore $ Just initialSize
    , amt: createRiverStore $ Just initialAmt
    , randomMany: createRiverStore $ Just initial
    }
  pure $ D.div $ D.Fragment
      [ D.div $ D.Fragment
          [ D.label' [ D.className "input-wrapper range" ] $ D.Fragment
              [ D.span (D.text "Simple")
              , D.input'
                  [ D.attr "type" "range"
                  , D.attr "min" "0"
                  , D.attr "max" "100"
                  , D.onInputNumber $ actions.size.send <<< Int.round
                  ]
              , D.span (D.text "Complex")
              ]
          , D.label' [ D.className "input-wrapper range" ] $ D.Fragment
              [ D.span (D.text "Few")
              , D.input'
                  [ D.attr "type" "range"
                  , D.attr "min" "1"
                  , D.attr "max" "30"
                  , D.value "15"
                  , D.onInputNumber $ actions.amt.send <<< Int.round
                  ]
              , D.span (D.text "Many")
              ]
          , D.button'
              [ D.className ""
              , D.onClickE' $
                ( (<**>) (pure initialAmt <|> actions.amt.stream) $
                    ( (pure initialSize <|> actions.size.stream) <#> \sz amt ->
                        (traverse_ (actions.randomMany.send) =<< randomProductions entry sz amt)
                    )
                )
              ]
              (D.text "Random")
          ]
      , (pure initial <|> actions.randomMany.stream) >@
          (D.ul' [ D.className "compact" ] <<< D.Fragment
          <<< map (\xs -> D.li' [ D.className "clickable", D.onClickE (sendUp xs) ] <<< D.Fragment
          <<< map (\x -> renderTok mempty x) $ xs)
          )
      ]

peas :: Array String -> Dragon
peas = D.Fragment <<< map (D.p <<< D.text)

computeGrammar
  :: SAugmented
  -> { producible :: SProducible
     , states :: SStates
     , stateIndex :: Map Int Int
     , allTokens :: Array CodePoint
     , allNTs :: Array NonEmptyString
     }
computeGrammar grammar =
  let
    _producible = withProducible grammar
    _states = either (const (States [])) identity $ numberStatesBy (add 1) grammar.augmented (calculateStates grammar.augmented grammar.start)
    _stateIndex = indexStates' _states
    _allTokens = gatherTokens' grammar.augmented
    _allNTs = gatherNonTerminals' grammar.augmented
  in
    { producible: _producible
    , states: _states
    , stateIndex: _stateIndex
    , allTokens: _allTokens
    , allNTs: _allNTs
    }

computeInput
  :: forall r
   . { grammar :: SAugmented
     , states :: SStates
     , stateIndex :: Map Int Int
     | r
     }
  -> String
  -> { tokens :: Maybe (List CodePoint)
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
    table = toTable states (Map.lookup <@> stateIndex)
  in
    \value ->
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
  pure $ grammarComponent buttonName initialGrammar upstream.receive upstream.send

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
  void $ subscribe io.input.receive \x -> do
    log $ "LOCAL RECEIVE INPUT " <> show x
    sendOthers x
  initialGrammar <- fromMaybe sampleGrammar <$> io.grammar.current
  fallbackInput <- fromMaybe' (\_ -> sampleS (withProducible initialGrammar)) <<< Json.toString <$> attrs "input"
  initialInput <- fromMaybe fallbackInput <$> io.input.current
  void $ subscribe io.grammar.receive \_ ->
    io.input.current >>= fromMaybe initialInput >>> sendOthers
  let
    sendInput = sendOthers <> io.input.send
    upstream =
      { send: sendInput
      , receive: io.input.receive
      , current: io.input.current
      }
  -- For all of the other data, it might not be updated (e.g. because we only
  -- received a grammar from the query)
  sendOthers initialInput
  pure $
    inputComponent initialInput upstream.receive upstream.send
      { grammar: io.grammar.receive
      , states: io.states.receive
      , tokens: io.tokens.loopback
      , tokens': io.tokens'.loopback
      , parseSteps: io.parseSteps.loopback
      , stateId: io.stateId.loopback
      , state: io.stateId.loopback
      , allTokens: io.allTokens.receive
      , allNTs: io.allNTs.receive
      , validTokens: io.validTokens.loopback
      , validNTs: io.validNTs.loopback
      , producible: io.producible.receive
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

withGrammar :: (SAugmented -> Dragon) -> Widget
withGrammar component { interface } = do
  let
    grammarLake = filterMap (hush <<< decode grammarCodec) (interface "grammar").receive
  pure $ component @< pure sampleGrammar <|> grammarLake

withProducibleSendTokens :: (SProducible -> (Array CodePoint -> Effect Unit) -> Dragon) -> Widget
withProducibleSendTokens component { interface } = do
  let
    grammarLake = filterMap (hush <<< decode producibleCodec) (interface "producible").receive
    sendTokens = (interface "input").send <<< Json.fromString <<< String.fromCodePointArray
  pure $ (flip component sendTokens) @< pure (withProducible sampleGrammar) <|> grammarLake

-- TODO optimize with mailboxed
spotlightBeh :: forall a f. Applicative f => Ord a => Lake a -> f (a -> Lake Boolean)
spotlightBeh e = pure \k -> eq k <$> e

widgetStateTable :: Widget
widgetStateTable { interface } = do
  let
    stateIdI = adaptInterface CA.int (interface "stateId")
    currentGetCurrentState = spotlightBeh stateIdI.receive
    currentStates = filterMap (hush <<< decode statesCodec) (interface "states").receive
    currentStatesAndGetState = (sampleOnRight currentGetCurrentState (map (/\) currentStates))
  pure $ currentStatesAndGetState >@
    \(x /\ getCurrentState) -> renderStateTable { getCurrentState } x

widgetParseTable :: Widget
widgetParseTable { interface } = do
  let
    stateIdI = adaptInterface CA.int (interface "stateId")
    currentGetCurrentState = spotlightBeh stateIdI.receive
    currentStates = filterMap (hush <<< decode statesCodec) (interface "states").receive
    currentStatesAndGetState = (sampleOnRight currentGetCurrentState (map (/\) currentStates))
    currentGrammar = filterMap (hush <<< decode grammarCodec) (interface "grammar").receive
  initialGrammar <- fromMaybe sampleGrammar <<< join <<< map (hush <<< decode grammarCodec) <$> (interface "grammar").current
  pure $ D.Replacing $ map
    (\(grammar /\ x /\ getCurrentState) -> renderParseTable { getCurrentState: getCurrentState } grammar x)
    (flip sampleOnRight ((/\) <<< _.augmented <$> (pure initialGrammar <|> currentGrammar)) (currentStatesAndGetState))

inputComponent :: forall r.
  String ->
  Lake String ->
  (String -> Effect Unit) ->
  { grammar :: Lake SAugmented
  , states :: Lake SStates
  , tokens :: Lake (Maybe (List CodePoint))
  , tokens' :: Lake (Maybe (List CodePoint))
  , parseSteps :: Lake (Maybe SCParseSteps)
  , stateId :: Lake Int
  , state :: Lake Int
  , allTokens :: Lake (Array CodePoint)
  , allNTs :: Lake (Array NonEmptyString)
  , validTokens :: Lake (Set CodePoint)
  , validNTs :: Lake (Set NonEmptyString)
  , producible :: Lake SProducible
  | r
  } ->
  Dragon
inputComponent initialInput inputStream sendInput current = eggy \shell -> do
  { send: pushInput, stream: localInput } <- shell.track $ createRiverStore Nothing
  currentValue <- shell.instStore (pure initialInput <|> inputStream <|> localInput)
  getCurrentValue <- shell.storeLast initialInput currentValue
  getCurrentProducible <- shell.storeLast Nothing (Just <$> current.producible)
  let
    currentValue' = {- notFrom localInput -} currentValue
    currentParseSteps = current.parseSteps
    currentGrammarTokens = current.allTokens
    currentGrammarNTs = current.allNTs
    currentValidTokens = current.validTokens
    currentValidNTs = current.validNTs

    renderTokenHere mtok = do
      let
        onClick = do
          v <- getCurrentValue
          pushInput <> sendInput $ case mtok of
            Just tok -> v <> String.singleton tok
            Nothing -> String.take (String.length v - 1) v
        valid = case mtok of
          Just tok -> currentValidTokens <#> Set.member tok
          Nothing -> currentValue <#> not String.null
        toktext = case mtok of
          Nothing -> codePointFromChar '⌫'
          Just tok -> tok
      renderTok' (map (append "clickable") $ valid <#> if _ then "" else " unusable") (pure $ Just onClick) toktext
    renderNTHere nt = do
      let
        onClick = do
          v <- getCurrentValue
          getCurrentProducible >>= traverse_ \prod -> do
            genned <- map (maybe "" String.fromCodePointArray) $ sequence $
              QC.randomSampleOne <$> genNT prod.produced nt
            pushInput <> sendInput $ (v <> genned)
        valid = currentValidNTs <#> Set.member nt
      renderNT' (map (append "clickable") $ valid <#> if _ then "" else " unusable") (pure $ Just onClick) nt
  pure $
    D.div $ D.Fragment
      [ D.div $ D.Fragment
          [ D.span $ currentGrammarTokens >@
              \x -> D.Fragment $
                ([ Nothing ] <> map Just x) <#> renderTokenHere
          , D.span $ currentGrammarNTs >@
              \x -> D.Fragment $
                x <#> renderNTHere
          ]
      , D.div $ D.span' [ D.className "terminal" ] $ inputC' "Source text" "" currentValue' (pushInput <> sendInput)
      , currentParseSteps >@ \todaysSteps -> eggy \_ -> do
          actions <- sequenceRecord
            { toggleLeft: createRiver -- Unit
            , toggleRight: createRiver -- Unit
            , slider: createRiver -- Int
            , rate: createRiverStore $ Just 1.0 -- Number
            , toggleAnimation: createRiver
            }
          let
            contentAsMonad = showMaybeParseSteps $ todaysSteps
            -- Run the first layer of the monad, to get the number of items being rendered up-front
            contentAsMonad2 /\ nEntities = runTrampoline (runStateT contentAsMonad 0)

            yes state = { state, emit: Just state }
            adjust :: (Int -> Int) -> { idx :: Int | _ } -> Lake _
            adjust fn = \state ->
              pure $ state { idx = state.idx # fn }

            actAs :: StepAction ->
              ({ idx :: Int | _ } -> Lake _) ->
              ({ idx :: Int | _ } -> Lake { state::_, emit::_ })
            actAs actor fn = map yes <<< fn >>> map \state ->
              state
                { idx = clamp 0 (nEntities - 1) state.idx
                , actor = actor
                , running = state.running && actor == Play
                }

            playAt rate state@{ idx } =
              let
                { start, count } =
                  if idx == nEntities - 1
                    then { start: 0, count: nEntities }
                    else { start: idx + 1, count: nEntities - idx - 1 }
              in selfGating (not _.running) $
                (pure 0 <|> add 1 <$> interval (1000.0 / rate # Milliseconds)) <#> \n ->
                  state { idx = start + n, running = n < count - 1, rate = rate }

            fullState = join animationLoop
              { idx: nEntities - 1
              , running: false
              , actor: Initial
              , rate: 1.0
              }
              [ actions.toggleLeft.stream  $> actAs Toggle (adjust (_ - 1))
              , actions.toggleRight.stream $> actAs Toggle (adjust (_ + 1))
              , actions.slider.stream <#> \n ->
                  actAs Slider (adjust (const n))
              , actions.rate.stream <#> \rate -> case _ of
                  state@{ running: false } -> pure
                    { state: state { rate = rate }, emit: Nothing }
                  state -> actAs Play (playAt rate) state
              , actions.toggleAnimation.stream $> actAs Play case _ of
                  state@{ running: true } -> pure $ state { running = false }
                  state@{ rate } -> state # playAt rate
              ]

            -- Maintain the current index, clamped between 0 and nEntitities
            -- (Note: it is automatically reset, since `switcher` resubscribes,
            -- creating new state for it)
          currentState <- shell.instStore fullState
          let stackIndex = dedupOn _.idx currentState
          let isRunning = _.running <$> currentState
          sweeper <- spotlightBeh (map _.idx stackIndex)
          let content = contentAsMonad2 sweeper
          pure $ D.div $ D.Fragment
            [ D.div $ D.button'
                [ D.onClickE $ actions.toggleAnimation.send unit ] $ D.Fragment
                  [ D.Text
                      ( isRunning <#> case _ of
                          true -> "Pause"
                          false -> "Play"
                      )
                  ]
            , D.div $ D.Fragment
                [ D.text "Speed"
                , D.span $ D.Fragment $ map
                    ( \(n /\ l) -> D.label $ D.Fragment
                        [ D.input'
                            [ D.attr "type" "radio"
                            , D.prop "checked" (l == "1x")
                            , D.attr "name" "speed"
                            , D.value $ show n
                            , D.onClickE $ actions.rate.send n
                            ]
                        , D.text l
                        ]
                    )
                    [ 0.5 /\ "0.5x", 1.0 /\ "1x", 2.0 /\ "2x", 4.0 /\ "4x", 8.0 /\ "8x" ]
                ]
            , D.div $ D.Fragment
                [ D.input'
                    [ D.attr "type" "range"
                    , D.attr "min" "0"
                    , D.attr "max" $ show (nEntities - 1)
                    , D.value $ show (nEntities - 1)
                    , D.value' $ map show $ stackIndex
                        # filterMap case _ of
                            { actor: Slider } -> Nothing
                            { idx: x } -> Just x
                    , D.onInputNumber $ actions.slider.send <<< floor
                    ]
                ]
            , D.div $ D.Fragment
                [ D.button' [ D.onClickE $ actions.toggleLeft.send unit ]
                    (D.text "<")
                , D.button' [ D.onClickE $ actions.toggleRight.send unit ]
                    (D.text ">")
                ]
            , content
            ]
      ]
