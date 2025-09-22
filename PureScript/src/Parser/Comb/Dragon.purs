module Parser.Comb.Dragon where

import Prelude

import Control.Plus (empty)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isRight, note)
import Data.Foldable (fold, foldMap)
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lazy (force)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap, un)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Dodo as T
import Idiolect (intercalateMap)
import Parser.Algorithms (getResultICM', revertICST', toAdvanceTo)
import Parser.Comb.Comber (type (~), FullParseError, OrEOF)
import Parser.Comb.Comber as Comber
import Parser.Comb.Run (Rec(..), combPrecedence, fromMaybeWS, mapWSStates, normalizeWSFragment)
import Parser.Comb.Run as Comb.Run
import Parser.Comb.Types as Comb
import Parser.Languages.ShowFast as ShowFast
import Parser.Lexing (FailReason(..), FailedStack(..), Rawr, Similar(..), bestRegexOrString, errorName, len, squintAt, unRawr, userErrors, (?), (??))
import Parser.Lexing as Lexing
import Parser.Main (Dragonss, Dragons)
import Parser.Main as Parser.Main
import Parser.Printer.Types (Ann(..))
import Parser.Printer.Types as IO
import Parser.Proto (Stack(..), topOf)
import Parser.Types (ICST(..), ShiftReduce(..), State(..), Zipper(..))
import Parser.Types as P
import Riverdragon.Dragon (Dragon(..))
import Riverdragon.Dragon.Ansi (ansiToAttrs)
import Riverdragon.Dragon.Bones (smarties, ($$), ($~~), (.$), (.$$), (.$~~), (:.), (:~), (<:>), (=!=), (=:=), (>@))
import Riverdragon.Dragon.Bones as D
import Riverdragon.River (Lake, River)
import Riverdragon.River as River
import Stylish.Types (Classy(..))
import Whitespace (MaybeWS(..), defaultWS)
import Whitespace as WS

nicerShow :: forall s. Show s => s -> Dragon
nicerShow s = D.pre [ D.className =:= "formatted-show" ] $$ ShowFast.mkReShow Nothing (show s) T.twoSpaces

tooltip :: Dragon -> Dragon -> Dragon
tooltip desc content =
  D.span [ D.className =:= "tooltipped" ] $ D.Fragment
    [ D.span [] content
    , D.span [ D.className =:= "tooltip", D.aria_"hidden" =:= "true" ] desc
    ]

tooltipT :: String -> String -> Dragon
tooltipT desc content = tooltip (D.text desc) (D.text content)

annDragon :: T.Printer Dragon Ann Dragon
annDragon = T.Printer
  { emptyBuffer: mempty
  , writeText: \_len str buff -> buff <> D.text str
  , writeIndent: \_len str buff -> buff <> D.text str
  , writeBreak: \buff -> buff <> D.br[]
  , enterAnnotation: \_ _ buff -> buff
  , leaveAnnotation: \ann stack buff -> wrapAnn stack ann buff
  , flushBuffer: D.pre [ D.className =:= "ansi-block" ]
  }
  where
  wrapAnn :: List Ann -> Ann -> Dragon -> Dragon
  wrapAnn stack (Ann { ansi, classes, styles, dragon: Endo wrapDragon, tooltip: maybeTooltip }) inner =
    let
      addTooltip = case maybeTooltip of
        Nothing -> identity
        Just desc -> tooltip desc
      { classes: moreClasses, style: ansiStyle } =
        ansiToAttrs (stack <#> \(Ann ann) -> ann.ansi) ansi

      addAttrs | { moreClasses, classes, ansiStyle, styles } == mempty = identity
      addAttrs = D.span
        [ D.classy =:= moreClasses <> classes
        , D.stylish =:= ansiStyle <> styles
        ]
    in addTooltip $ wrapDragon $ addAttrs inner

renderParseError :: FullParseError -> Dragon
renderParseError theError = case theError of
  CrashedStack s -> D.div .$ D.text "Internal parser error: " <> D.text s
  FailedStack info@{ lookupState, initialInput, currentInput, failedStack } ->
    let
      previewState st = lookupState st # foldMap \state ->
        listing (unwrap state.items) \{ pName, rName, rule: Zipper before after, lookahead } -> fold
          [ D.span:."non-terminal".$$ either identity identity pName
          , renderMeta "#"
          , foldMap (renderRule <<< Just) rName
          , renderMeta ": "
          , renderZipper $ Zipper (normalizeWSFragment before) (normalizeWSFragment after)
          , renderLookahead (Classy if Array.null after then "reducible" else "") $
              map (lmap SetWS) lookahead
          ]
      renderFailReason = case _ of
        StackInvariantFailed -> D.text $ "Internal parser error: Stack invariant failed"
        UnknownState { state } -> D.text "Internal parser error: Unknown state " <> nicerShow state
        Uhhhh { token: cat } -> D.text "Internal parser error: Uhhhh " <> nicerShow cat
        UnknownReduction { reduction: nt /\ r } -> D.text $ "Internal parser error: UnknownReduction " <> show nt <> "#" <> show r
        -- SpaceFailed { parsed: space /\ air /\ cat /\ o /\ i, expected: spaceE /\ catE } -> fold
        --   [ D.text "Internal parser error: Whitespace failed. "
        --   , D.text "Parsed "
        --   , renderWS space
        --   , renderCatTok cat o
        --   , D.text ", expected "
        --   , renderWS spaceE
        --   , renderCat catE
        --   ]
        ExternalError msg -> D.text $ "Internal parser error: " <> msg

        UserRejection { userErr } -> fold
          [ D.text "Parse error: User rejection"
          , listing userErr D.text
          ]
        NoTokenMatches { advance } -> fold
          [ D.text "Parse error: No token matches"
          , D.text ", expected"
          , D.text case Map.size advance of
              0 -> " nothing??"
              1 -> ""
              _ -> " one of"
          , listing (Map.toUnfoldable advance) \(cat /\ space /\ _sr) ->
              renderWS space <> renderCat cat
          ]
        UnexpectedToken { advance, matched }
          | [ _space /\ air /\ cat0 /\ o /\ _i ] <- NEA.toArray matched -> fold
          [ D.text "Parse error: Unexpected token "
          , renderWS $ fromMaybe one $ squintAt air
          , renderCatTok cat0 o
          , D.text ", expected"
          , D.text case Map.size advance of
              0 -> " nothing??"
              1 -> ""
              _ -> " one of"
          , listing (Map.toUnfoldable advance) \(cat /\ space /\ _sr) ->
              renderWS space <> renderCat cat
          ]
        UnexpectedToken { advance, matched: matched } -> fold
          [ D.text "Parse error: Unexpected+ambiguous token "
          , listing (NEA.toArray matched) \(_space /\ air /\ cat0 /\ o /\ _i) ->
              renderWS (fromMaybe one $ squintAt air) <> renderCatTok cat0 o
          , D.text "\nExpected"
          , D.text case Map.size advance of
              0 -> " nothing??"
              1 -> ""
              _ -> " one of"
          , listing (Map.toUnfoldable advance) \(Tuple cat _) ->
              renderCat cat
          ]
        AllActionsRejected { possibilities } -> fold
          [ D.text "Parse error: All actions rejected"
          , listing (NEA.toArray possibilities) \((sr /\ space /\ air /\ cat /\ o /\ i) /\ errs) -> fold
              [ D.text "Pondering "
              , renderWS space
              , renderCatTok cat o
              , D.text " with actions "
              , flip listing identity $ sr # bifoldMap
                  do \s -> pure $ renderCmd "s" <> renderSt s <> previewState s
                  do \(rule /\ nt /\ r) -> pure $
                      renderCmd "r" <> renderNTR nt r <> renderMeta ": " <> foldMap renderPart (normalizeWSFragment rule)
              , D.text " gave errors "
              , listing errs \err -> fold
                  [ D.detailsW (errorName err).$ renderFailReason err
                  , listing (userErrors err) D.text
                  ]
              ]
          ]
        Ambiguity { filtered, userErr } -> fold
          [ D.text "Parse error: LR(1) ambiguity"
          , listing (NEA.toArray filtered) \(sr /\ space /\ air /\ cat /\ o /\ i) -> fold
              [ D.text "Pondering "
              , renderWS space
              , renderCatTok cat o
              , D.text ": "
              , case sr of
                  Shift s -> D.text "Shift to " <> renderSt s
                  ShiftReduces s rs -> D.text "Shift to " <> renderSt s <> D.text ", or reduce " <>
                    intercalateMap (D.text " or ") (uncurry renderNTR <<< snd) rs
                  Reduces rs -> D.text "Reduce " <>
                    intercalateMap (D.text " or ") (uncurry renderNTR <<< snd) rs
              , flip listing identity $ sr # bifoldMap
                  do \s -> pure $ renderCmd "s" <> renderSt s <> previewState s
                  do \(rule /\ nt /\ r) -> pure $
                      renderCmd "r" <> renderNTR nt r <> renderMeta ": " <> foldMap renderPart (normalizeWSFragment rule)
              ]
          , listing userErr D.text
          ]
    in D.div.$~~
      [ renderFailReason info.failReason
      , D.text $ "\nat character " <> show (1 + len initialInput - len currentInput)
      , D.text case initialInput of
          P.Continue s -> "\n  " <> show (String.drop (len initialInput - len currentInput) s)
          P.EOF -> ""
      , D.br[]
      , fold $ renderSt <$> (info.failedState <#> _.sName)
      , D.detailsW "Show".$ nicerShow info.failedState
      , D.br[]
      , D.detailsW "Stack: ".$ D.div:."error-stack full stack".$ renderStack failedStack
      ]

listing :: forall a. Array a -> (a -> Dragon) -> Dragon
listing items f = D.ul.$~~ items <#> f >>> D.li[]

renderNT :: Either String String -> Dragon
renderNT (Left x) = D.span:."non-terminal".$$ x
renderNT (Right x) = D.span:."non-terminal".$$ x

renderNTR :: Either String String -> Maybe Int -> Dragon
renderNTR (Left x) Nothing = (D.span:."non-terminal".$$ x) <> renderMeta "#"
renderNTR (Right x) (Just r) = (D.span:."non-terminal".$$ x) <> renderMeta "#" <> renderRule (Just r)
renderNTR _ _ = D.text "???"

renderRule :: Maybe Int -> Dragon
renderRule (Just rName) = D.span:."rule".$$ show rName
renderRule Nothing = D.span:."rule".$$ Comber.topName

renderCat :: P.OrEOF (String ~ Rawr) -> Dragon
renderCat = renderPart <<< P.Terminal

renderTok :: P.OrEOF String -> Dragon
renderTok = case _ of
  P.EOF -> tooltip (D.text "EOF") $ renderMeta "$"
  P.Continue s -> D.span:."terminal string".$$ show s

renderCatTok :: P.OrEOF (String ~ Rawr) -> P.OrEOF String -> Dragon
renderCatTok = case _, _ of
  P.EOF, P.EOF -> tooltip (D.text "EOF") $ renderMeta "$"
  P.Continue (Similar (Left s)), P.Continue s' | s == s' -> D.span:."terminal string".$$ show s
  P.Continue (Similar (Right r)), P.Continue s -> tooltip
    do D.span:."terminal regex match".$$ show r
    do D.span:."terminal string match".$$ show s
  _, _ -> D.text "??"

renderPart :: P.Part (WS.MaybeWS WS.ParseWS) (Either String String) (P.OrEOF (String ~ Rawr)) -> Dragon
renderPart = case _ of
  P.NonTerminal (Right v) -> D.span:."non-terminal".$$ v
  P.NonTerminal (Left v) -> (D.span:."non-terminal".$$ v) <> renderMeta "#"
  P.Terminal P.EOF -> tooltip (D.text "EOF") $ renderMeta "$"
  P.Terminal (P.Continue (Similar (Left s))) -> D.span:."terminal string".$$ show s
  P.Terminal (P.Continue (Similar (Right r))) -> D.span:."terminal regex".$$ "/" <> unRawr r <> "/"
  P.InterTerminal ws -> renderWS $ fromMbWS ws

fromMbWS :: WS.MaybeWS WS.ParseWS -> WS.ParseWS
fromMbWS = case _ of
  WS.DefaultWS -> defaultWS
  WS.SetOrDefaultWS space -> defaultWS + space
  WS.SetWS space -> space

renderPart' :: P.Part WS.ParseWS (Either String String) (P.OrEOF (String ~ Rawr)) -> Dragon
renderPart' = case _ of
  P.NonTerminal (Right v) -> D.span:."non-terminal".$$ v
  P.NonTerminal (Left v) -> (D.span:."non-terminal".$$ v) <> renderMeta "#"
  P.Terminal P.EOF -> tooltip (D.text "EOF") $ renderMeta "$"
  P.Terminal (P.Continue (Similar (Left s))) -> D.span:."terminal string".$$ show s
  P.Terminal (P.Continue (Similar (Right r))) -> D.span:."terminal regex".$$ "/" <> unRawr r <> "/"
  P.InterTerminal ws -> renderWS ws

renderWS :: WS.ParseWS -> Dragon
renderWS ws' =
  case WS.unParseWS ws' of
    { allowed_newline: false, allowed_space: false, required: true } ->
      tooltipSp "(never match)" "\xA1"
    ws@{ allowed_newline: true, allowed_space: false } ->
      tooltipSp ("newline " <> if ws.required then "required" else "allowed") $
        "\\n" <> guard (not ws.required) "?"
    ws@{ allowed_newline: true, allowed_space: true } ->
      tooltipSp ("any space " <> if ws.required then "required" else "allowed") $
        "\\s" <> guard (not ws.required) "?"
    ws@{ allowed_newline: false, allowed_space: true } ->
      tooltipSp ("horizontal space " <> if ws.required then "required" else "allowed") $
        "sp" <> guard (not ws.required) "?"
    { allowed_newline: false, allowed_space: false, required: false } ->
      tooltipSp "no space allowed" "\x2205"
  where
  tooltipSp description display = tooltip (D.span:."whitespace".$$ description) $ D.span:."whitespace".$$ display

renderCSTTree :: Comber.CST -> Dragon
renderCSTTree ast =
  D.ol :."AST CST".$
    D.li.$ renderCSTChild ast

renderCSTChild :: Comber.CST -> Dragon
renderCSTChild (IAir "") = D.span:."leaf node whitespace".$
  tooltip (D.text "no space parsed") $ D.span:."whitespace".$$"\x2205"
renderCSTChild (IAir air) =
  D.span :."leaf node whitespace".$$ air
    # String.replaceAll (String.Pattern "\n") (String.Replacement "\\n")
    # String.replaceAll (String.Pattern "\t") (String.Replacement "\\t")
    # String.replaceAll (String.Pattern "\r") (String.Replacement "\\r")
    # String.replaceAll (String.Pattern " ") (String.Replacement "\\s")
renderCSTChild (ILeaf tok) =
  D.span :."leaf node".$ renderTok tok
renderCSTChild (IBranch (nt /\ r) []) =
  D.span :."node".$ renderNTR nt r
renderCSTChild (IBranch (nt /\ r) cs) = D.Fragment
  [ D.span :."node".$ renderNTR nt r
  , D.ol :."layer".$~~
      cs <#> \c -> D.li.$ renderCSTChild c
  ]


liveDebug :: Comber.Comber Dragon -> Dragon
liveDebug parser = executeLive Nothing (Comb.Run.compile Comber.topName (unwrap parser)) Nothing

livePrinterParser :: forall t. Show t => IO.PrinterParser t t -> Dragon
livePrinterParser (IO.PrinterParser { printer, parser }) =
  liveDebug $
    un Compose (unwrap (WS.withBoundaryF parser)) <#> \(IO.Parsed { ast, cst }) -> fold
      [ D.html_"h5" [ D.style =:= "margin: 0" ] $$ "show"
      , nicerShow (force ast)
      , D.html_"h5" [ D.style =:= "margin: 0" ] $$ "cst"
      , renderDoc $ cst mempty
      , D.html_"h5" [ D.style =:= "margin: 0" ] $$ "ast"
      , renderDoc $ WS.renderDoc $
          printer (\t -> T.flexGroup $ T.text "(" <> t <> T.text ")") (force ast) Nothing mempty
      ]
  where
  renderDoc doc = D.pre[] $ T.print annDragon T.twoSpaces doc

executeLive ::
  Maybe Comber.Conf ->
  Comber.Compiled Dragon ->
  Maybe (River String) ->
  Dragon
executeLive conf0 { states: { stateMap, start, states: states0 }, resultants, options, precedences, grammar } = do
  let
    xx = Comb.fullMapOptions
      { rec: identity
      , nt: pure
      , r: pure
      , cat: P.Continue
      , o: P.Continue
      , rule: P.normalizeFragment
      , cst: note [] <<< revertICST'
      }
    options' ::
      String ->
      Comber.Options ->
      _
    options' name opts =
      let
        x = xx opts
      in Comb.Options
          [ { pName: Left name
            , rName: Nothing
            , rule: [P.NonTerminal (pure x /\ Right name), P.Terminal P.EOF]
            , logicParts: mempty
            , advanced: []
            }
          ]
    conf1 :: Comber.Conf
    conf1 = fromMaybe ({ best: bestRegexOrString, defaultSpace: defaultWS } :: Comber.Conf) conf0
    states = mapWSStates (fromMaybeWS conf1.defaultSpace) states0
    conf :: Comber.Conf
    conf = conf1 { best = \x -> ((<=<) conf1.best $ Lexing.screenPrecedence $ combPrecedence precedences) x }
    rec = Rec \name input optionsHere result -> do
      state <- Map.lookup name stateMap?? CrashedStack "Could not find parser in table"
      stack <- Lexing.contextLexingParse conf (state /\ states) (options' name optionsHere) rec (P.Continue input)
      cst <- getResultICM' stack?? Lexing.CrashedStack "Failed to extract result"
      result cst? \userErr -> FailedStack
        { states
        , lookupState: Array.index (unwrap states)
        , initialInput: P.Continue input
        , failedStack: stack
        , failedState: unwrap states !! snd (topOf stack)
        , currentInput: P.EOF
        , failReason: Lexing.UserRejection { userErr }
        }
    addInput withInput (Just input) = withInput input
    addInput withInput Nothing = Egg do
      { stream: input, send } <- River.createRiver
      pure $ D.div.$~~
        [ D.div.$ D.label :."input-wrapper text".$~~
          [ D.span.$$ "Input text"
          , D.input [ D.onInputValue =:= send ]
          ]
        , withInput input
        ]
    parseInput input = do
      stack <- Lexing.contextLexingParse conf (start /\ states) (options' (fst resultants) options) rec (P.Continue input)
      let
        failHere failReason = Lexing.FailedStack
          { states
          , lookupState: Array.index (unwrap states)
          , initialInput: P.Continue input
          , failedStack: stack
          , failedState: unwrap states !! snd (topOf stack)
          , currentInput: P.EOF
          , failReason
          }
      cst <- getResultICM' (spy "stack" stack)??
        failHere (ExternalError "Failed to extract result")
      -- Apply the function that parses from the CST to the desired result type
      map (Tuple stack) $ uncurry (Comb.matchRule rec) resultants cst?
        \userErr -> failHere $ Lexing.UserRejection { userErr }
    renderParsed (Left err) = renderParseError err
    renderParsed (Right (stack /\ result)) = D.Fragment
      [ D.div:."result".$ result
      , D.div:."result-stack full stack".$ renderStack stack
      ]
  addInput \inputStream -> Egg do
    pure $ D.div.$~~
      [ D.div [] $ inputStream >@ parseInput >>> renderParsed
      , D.html_"hr" [] mempty
      , D.detailsW "State Table".$ renderStateTable { getCurrentState: const empty } $ mapWSStates SetWS states
      , D.detailsW "Parse Table".$ renderParseTable { getCurrentState: const empty } $ mapWSStates SetWS states
      ]

renderStack :: Stack (_ /\ Int) Comber.CST -> Dragon
renderStack (Zero st) = D.sub.$ renderStackState st
renderStack (Snoc more cst st) = renderStack more <> D.space <> renderCSTTree cst <> D.space <> (D.sub.$ renderStackState st)
renderStackState :: forall a90. Tuple a90 Int -> Dragon
renderStackState (_ /\ stateNumber) = renderSt stateNumber

renderStateTable :: forall r. { getCurrentState :: Int -> Lake Boolean | r } -> Comber.States -> Dragon
renderStateTable info (P.States states) = do
  let
    mkTH n 0 0 = D.th [ D.attr "rowspan" =:= show n ]
    mkTH _ _ 0 = mempty
    mkTH _ _ _ = D.td []
    stateClass sName = smarties { "active": info.getCurrentState sName }
    renderStateHere items =
      let n = Array.length items in
      items # mapWithIndex \j -> D.tr[] <<< D.Fragment <<< mapWithIndex (mkTH n j)
  D.table :."data-table state-table".$~~
    states <#> \s@{ sName, items } ->
      D.tbody [ D.classy <:> stateClass sName ] $~~
        renderStateHere $ renderState s items

renderState :: Comber.StateInfo -> Comber.State -> Dragonss
renderState s (State items) = (\j v -> renderItem s j v) `mapWithIndex` items

renderItem :: Comber.StateInfo -> Int -> Comber.StateItem -> Dragons
renderItem s j { pName, rName, rule: rule@(Zipper _ after), lookahead } =
  [ if j == 0 then renderSt s.sName else D.text ""
  , D.span:."non-terminal".$$ either identity identity pName
  , renderMeta "#"
  , foldMap (renderRule <<< Just) rName -- TODO: right align?
  , renderMeta ": "
  , renderZipper rule
  , renderLookahead (Classy if Array.null after then "reducible" else "") lookahead
  , D.span.$~~ case toAdvanceTo s rule of
      Nothing -> []
      Just s' -> [ renderMeta " —> ", renderSt s' ]
  ]

renderZipper :: Zipper (WS.MaybeWS WS.ParseWS) (Either String String) (OrEOF (String ~ Rawr)) -> Dragon
renderZipper (Zipper before after) =
  D.span [ D.classy =:= D.smarts { "zipper": true, "reducible": Array.null after } ] $~~
    [ D.span:."parsed".$~~ before <#> renderPart
    , if Array.null after then D.text "" else
        D.span.$~~ after <#> renderPart
    ]

renderLookahead :: Classy -> Array (WS.MaybeWS WS.ParseWS /\ OrEOF (String ~ Rawr)) -> Dragon
renderLookahead moreClass items =
  D.span [ D.classy =:= Classy "lookahead" <> moreClass ] $ mempty
    <> renderMeta "{ "
    <> Array.intercalate (renderMeta ", ") (items <#> \(sp /\ tok) -> renderWS (fromMbWS sp) <> renderPart (P.Terminal tok))
    <> renderMeta " }"


renderParseTable :: forall r.
  { getCurrentState :: Int -> Lake Boolean | r } ->
  Comber.States ->
  Dragon
renderParseTable info (P.States states) = Egg do
  { stream: stateHighlighted, send: push } <- River.createRiverStore $ Just Nothing
  let
    terminals /\ nonTerminals = Parser.Main.getHeader (P.States states)
      <#> Array.filter isRight
    renderTerminals x = renderCat x
    renderNonTerminals x = renderNT x
    hover s =
      [ D.on_"mouseenter" =!= push (Just s)
      , D.on_"mouseleave" =!= push Nothing
      ]
    renderStHere s =
      D.span :."state hoverable":~hover s $$ show s
    renderShiftReduce Nothing = mempty
    renderShiftReduce (Just (Additive space /\ Shift s)) = D.span.$~~
      [ renderWS (fromMbWS space), D.span (hover s) (renderCmd "s"), renderStHere s ]
    renderShiftReduce (Just (Additive space /\ Reduces rs)) =
      D.div [ if NEA.length rs > 1 then D.className =:= "conflict" else empty ] $
        rs # foldMap \r -> D.div.$~~ [ renderWS (fromMbWS space), renderCmd "r", uncurry renderNTR r ]
    renderShiftReduce (Just (Additive space /\ ShiftReduces s rs)) =
      D.div :."conflict".$
        D.div[] (renderWS (fromMbWS space) <> D.span (hover s) (renderCmd "s") <> renderStHere s) <>
        (rs # foldMap \r -> D.div.$~~ [ renderCmd "r", uncurry renderNTR r ])
    renderGoto Nothing = mempty
    renderGoto (Just s) = D.span (hover s) (renderCmd "g" <> renderStHere s)
    cols state =
      let
        forTerminal tok = map (map snd) <$> Map.lookup tok (unwrap state.advance)
        forNonTerminal nt = Map.lookup nt state.receive
      in
        map (renderShiftReduce <<< forTerminal) terminals <> map (renderGoto <<< forNonTerminal) nonTerminals

    header = D.tr.$~~ mapWithIndex (\i -> D.th [ Parser.Main.col (Array.length terminals + 1) i ]) $
      [ D.text "" ] <> map renderTerminals terminals <> map renderNonTerminals nonTerminals
    clsFor s = D.smarties
      { "active": info.getCurrentState s
      , "hover": stateHighlighted <#> eq (Just s)
      }
    rows = states <#> \state ->
      D.tr [ D.classy <:> clsFor state.sName] $~~
        Array.cons (D.th[] $ renderStHere state.sName) $
          cols state # mapWithIndex
            \i -> D.td [ Parser.Main.col (Array.length terminals) i ]
  pure $ D.table :."data-table parse-table".$~~
    [ D.thead.$ header
    , D.tbody.$~~ rows
    ]


renderMeta :: String -> Dragon
renderMeta text = D.span:."meta".$$ text

renderSt :: Int -> Dragon
renderSt x = D.span:."state".$$ show x

renderCmd :: String -> Dragon
renderCmd "r" = tooltip (D.text "reduce by this rule") $ D.span:."cmd".$$ "r"
renderCmd "s" = tooltip (D.text "shift a token to enter this state") $ D.span:."cmd".$$ "s"
renderCmd "g" = tooltip (D.text "goto this state after a reduction") $ D.span:."cmd".$$ "g"
renderCmd c = D.span:."cmd".$$ c

