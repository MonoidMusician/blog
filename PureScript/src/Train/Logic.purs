module Train.Logic where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec, untilJust)
import Control.Monad.State (class MonadState, State, evalState, get, gets)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Functor.Compose (Compose(..))
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Optical ((<@=), (@<>), (@=), (~@))
import Data.Ord.Min (Min(..))
import Data.Pair (Pair(..))
import Data.Semigroup.First (First(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Idiolect (incorporate, (#..), (#:..), (..$), (<#>:), (<#?>), (>==), (\|/))
import Math.Matrix (inv)
import Train.Types (Canonized, Feature(..), HitMap, Layout, Pos, Standard(..))
import Type.Proxy (Proxy(..))

analyzeLayout :: forall m r. MonadState { hitmap :: HitMap | r } m => Array Canonized -> m Layout
analyzeLayout array = ado
  in
    { array
    , segments
    , switches
    , straights
    , components
    , loops
    , feature
    , logical:   Map.empty
    , physical:  Map.empty
    , clusters:  Set.empty
    , crossings: Set.empty
    }
  where
  getPos i = (array Array.!! i) <#> \(Pair { pos: Pair pos _ } _) -> pos
  -- Both directions of a pair
  directions (Pair p q) = Pair (Pair p q) (Pair (rev q) (rev p))
  rev { at, to } = { at, to: inv to }
  andRev p = [ p, rev p ]

  -- Put all of the individual segments in a map, both ways
  segments = array #:.. \i (Pair { pos } _) ->
    directions pos #.. \(Pair p q) ->
      Map.singleton p (Map.singleton q (wrap i))
  -- Switches have more than one destination
  simpleSwitches = segments
    # Map.filter \dests -> Map.size dests >= 2

  -- Pull out the segments with one destination
  singleStraights = segments
    # Map.filter (\dests -> Map.size dests == 1)
    # Map.mapMaybe Map.findMin -- convert to a { key, value } record
  -- These are the starts of straights
  startStraights = Map.difference singleStraights $
    Set.toMap $ singleStraights #.. _.key >>> Set.singleton
  -- Keep exploring until reaching a branch, a loop, or an end
  exploreStraight { key, value: First step0 } = go (Set.singleton step0) (NEL.singleton step0) key
    where
    go stepSet steps pos = case Map.lookup pos singleStraights of
      -- Not a loop
      Just { key: next, value: First step } | not Set.member step stepSet ->
        go (Set.insert step stepSet) (NEL.cons step steps) next
      -- Loop or end
      _ -> { end: pos, segments: NEA.reverse $ NEA.fromFoldable1 steps }
  -- Compose them end-to-end
  straights = exploreStraight <$> startStraights

  -- Incorporate the straights into the switches
  switches = simpleSwitches <#> foldMapWithIndex \step (First seg) ->
    case array Array.!! seg of
      Nothing -> mempty
      Just (Pair { canon: Standard { radius } } _) ->
        Map.singleton radius
          case Map.lookup step straights of
            Nothing -> { step, segments: pure seg, end: step }
            Just { segments: segs, end } -> { step, segments: NEA.cons seg segs, end }

  -- Perfect loops have no start, so they were included in `singleStraights` but
  -- not in `startStraights` and thus not in `straights`
  partOfLoops = map _.key $
    Map.difference (Map.difference singleStraights startStraights) $
      Set.toMap $ getPoses ..$ straights
  getPoses straight =
    Set.fromFoldable $ NEA.toArray straight.segments >>=
      -- Account for reverse directions, since loops are bidirectional
      -- but straights may not be
      getPos >>> maybe [] andRev
  -- Look for the next loop to start, scan it, and add it
  findLoops building taking =
    case Map.findMin taking of
      Nothing -> building
      Just { key, value: next } ->
        let Tuple k v = buildLoop key next (Set.singleton key <> Set.singleton next)
        in findLoops (Map.insert k v building) (Map.difference taking $ Set.toMap v.positions)
  -- Scan through a whole loop
  buildLoop start next soFar
    | start == next = Tuple start { chosen: Min start, positions: soFar }
    | otherwise =
      case Map.lookup next partOfLoops of
        Nothing -> Tuple start { chosen: Min start, positions: soFar }
        Just next' -> buildLoop start next' (Set.insert next' soFar)
  loops = findLoops Map.empty partOfLoops

  allStraights = Map.unions $ Compose $
    straights <#>: \start { end, segments: segs } ->
      let feat = FeatEndpoints start end in
      NEA.toArray segs <#?> getPos >== \p -> Map.singleton p feat
  allLoops = Map.unions $ loops <#> \loop@{ positions } ->
    Set.toMap positions $> loop
  feature = map First $ Map.union allStraights $
    Map.union (FeatSwitch <$> switches) (FeatLoop <$> loops)

  scanComponent :: State ScanComponent (Set Pos)
  scanComponent = untilJust do
    { noticed } <- get
    -- Grab from noticed, look it up in switches or straights
    productive <- case Set.findMin noticed of
      Just pos -> do
        -- Shift it from noticed to searched
        Set.delete pos ~@ Proxy @"noticed"
        Set.insert pos ~@ Proxy @"searched"
        { searched } <- get
        -- Look up it as a point of interest
        case Map.lookup pos switches \|/ Map.lookup pos straights of
          Just (Left dirs) -> do
            -- Places to explore next
            let tendrils = Set.fromFoldable $ _.end <$> dirs
            -- Segments visited along the way
            let segs = foldMap (NEA.toArray <<< _.segments) dirs
            Proxy @"noticed" @<> Set.difference tendrils searched
            Proxy @"found" @<> Set.fromFoldable segs
            pure true
          Just (Right { end, segments: segs }) -> do
            -- Place to explore next
            let tendrils = Set.singleton end
            Proxy @"noticed" @<> Set.difference tendrils searched
            -- Segments visited along the way
            Proxy @"found" @<> Set.fromFoldable segs
            pure true
          Nothing -> pure true
      Nothing -> pure false
    -- We may loop immediately, or check if we are finished
    case productive of
      true -> pure Nothing -- Loop!
      false -> do
        -- Drain the set of found segments
        found <- Proxy @"found" <@= Set.empty
        -- Convert it into positions, both endpoints, forwards and backwards
        let getEnds i = (array Array.!! i) # maybe [] \(Pair { pos: Pair x y } _) -> [ x, y ] >>= andRev
        let checkPoses = Set.fromFoldable $ Set.toUnfoldable found >>= getEnds
        -- See what positions are actually new
        novel <- gets $ Set.difference checkPoses <<< _.searched
        Proxy @"noticed" @= novel
        if Set.isEmpty novel
          then gets $ Just <<<  _.searched
          else pure Nothing -- Restart
  -- Scan a connected component from a seed position, then convert the set of
  -- positions into the desired map format
  scan from =
    let
      positions = evalState scanComponent
        { found: Set.empty, searched: Set.empty
        , noticed: Set.singleton from }
    in case Min <$> Set.findMin positions of
      Nothing -> Map.empty
      Just chosen -> Set.toMap positions $> { chosen, positions }

  launchPoints = Map.keys switches <> Map.keys straights
  -- Loops are their own connected component, convert them to be bidirectional
  biLoops = allLoops <#>: \pos loop ->
    incorporate loop $ Map.lookup (rev pos) allLoops
  -- Iterate until all of the launch points are covered by connected components
  components = biLoops # tailRec \soFar -> do
    let possibilities = Set.difference launchPoints (Map.keys soFar)
    case Set.findMin possibilities of
      Nothing -> Done soFar
      Just from -> Loop $ Map.union soFar $ scan from


type ScanComponent =
  { found :: Set Int
  , searched :: Set Pos
  , noticed :: Set Pos
  }

