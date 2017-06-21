module Blossom where

-- THIS DOES NOT WORK YET
-- SORRY FOR BEING ON GOOGLE

import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Set (Set)
-- import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GraphBasics

data BlossomLabel = S | T | Breadcrumb | Free deriving (Eq, Ord, Show)

data BlossomCommonState v n l = BlossomCommonState {
     bcLabel :: BlossomLabel
   , bcLabelEnd :: Maybe v
   , bcBestEdge :: Maybe (Edge v n l)
 } deriving (Eq, Show)

data BlossomBlossomState v n l = BlossomBlossomState {
     bbBase :: v -- should this be common?
   , bbCommonState :: BlossomCommonState v n l
 } deriving (Eq, Show)

type BlossomID = Int

data BlossomVertexState v n l = BlossomVertexState {
   vInBlossom :: Maybe BlossomID
    -- it turns out a vertex can have a different label than its blossom. sigh.
  , vMate :: Maybe v
  , vCommonState :: BlossomCommonState v n l
} deriving (Eq, Show)

-- A Hopcroft-Karp state includes the whole graph, the mapping from vertices to
-- vertex states, and the distance to the virtual Nil vertex. Other
-- implementations treat Nil as an actual vertex but they aren't married to
-- ultra-strict type systems like I am!
data BlossomState v n l = BlossomState
  { bGraph :: Graph v n l
  , bVertices :: Map v (BlossomVertexState v n l)
  , bBlossoms :: Map BlossomID  (BlossomBlossomState v n l)
  , bAllowedEdges :: Set (Edge v n l)
  } deriving (Eq, Show)

commonState :: (Eq v, Ord v) => BlossomState v n l -> Either v BlossomID -> BlossomCommonState v n l
commonState state node = case node of Left v -> vCommonState ((bVertices state)!v)
                                      Right b -> bbCommonState ((bBlossoms state)!b)

updateCommonState :: (Ord v) => BlossomState v n l -> Either v BlossomID -> (BlossomCommonState v n l -> BlossomCommonState v n l) -> BlossomState v n l
updateCommonState state node f = case node of Left v -> updateCommonStateForVertex state v f
                                              Right b -> updateCommonStateForBlossom state b f

updateCommonStateForVertex :: (Ord v) => BlossomState v n l -> v -> (BlossomCommonState v n l -> BlossomCommonState v n l) -> BlossomState v n l
updateCommonStateForVertex state vertex f = let
  oldCState = commonState state (Left vertex)
  newCState = f oldCState
  newVState = ((bVertices state)!vertex){vCommonState = newCState}
  newVMap = Map.insert vertex newVState (bVertices state)
  in state{bVertices = newVMap}
  
updateCommonStateForBlossom :: (Ord v) => BlossomState v n l -> BlossomID -> (BlossomCommonState v n l -> BlossomCommonState v n l) -> BlossomState v n l
updateCommonStateForBlossom state blossom f = let
  oldCState = commonState state (Right blossom)
  newCState = f oldCState
  newBState = ((bBlossoms state)!blossom){bbCommonState = newCState}
  newBMap = Map.insert blossom newBState (bBlossoms state)
  in state{bBlossoms = newBMap}
  
updateLabel :: (Ord v) => BlossomState v n l -> Either v BlossomID -> BlossomLabel -> BlossomState v n l
updateLabel state node newLabel = let
  f = \state -> state{bcLabel = newLabel}
  in updateCommonState state node f

-- the same function but foldable
relabel :: (Ord v) => BlossomLabel -> BlossomState v n l -> Either v BlossomID -> BlossomState v n l
relabel label state node = updateLabel state node label

updateLabelEnd :: (Ord v) => BlossomState v n l -> Either v BlossomID -> Maybe v -> BlossomState v n l
updateLabelEnd state node newLabelEnd = let
  f = \state -> state{bcLabelEnd = newLabelEnd}
  in updateCommonState state node f

updateBestEdge :: (Ord v) => BlossomState v n l -> Either v BlossomID -> Maybe (Edge v n l) -> BlossomState v n l
updateBestEdge state node newBestEdge = let
  f = \state -> state{bcBestEdge = newBestEdge}
  in updateCommonState state node f

scanBlossom :: (Eq v, Ord v) => BlossomState v n l -> v -> v -> (Maybe v, BlossomState v n l)
scanBlossom state v w = scanBlossom0 state (Just v) (Just w) [] Nothing

scanBlossom0 :: (Eq v, Ord v) => BlossomState v n l -> Maybe v -> Maybe v -> [Either v BlossomID] -> Maybe v -> (Maybe v, BlossomState v n l)
-- we need to be able to figure inblossom[v]
-- and blossombase[b]
scanBlossom0 state v w path base
  | v == Nothing || w == Nothing = (base, removeBreadcrumbs)
  | bcLabel bCState == Breadcrumb = (Just newBase, removeBreadcrumbs)
  | bcLabel bCState /= S = error "Uh this blossom is not labeled S"
  | v2 /= newBasesMate = error "labelend does not match base mate"
  | (bcLabel nextBState) /= T = error "nextB should have been a T blossom"
  | (bcLabelEnd nextBState) == Nothing = error "nextB should have a labelEnd"
  | otherwise = scanBlossom0 state2 nextV nextW newPath (Just newBase)
  where
    removeBreadcrumbs = foldl (relabel S) state path
    Just justV = v
--    Just justW = w
    Right b = inBlossomOrTrivial state justV
    bState = (bBlossoms state)!b
    bCState = bbCommonState bState
    newBase = bbBase bState
    newPath = (Right b):path
    state2 = updateLabel state (Right b) Breadcrumb
    newBasesMate = vMate ((bVertices state)!newBase)
    v2 = bcLabelEnd bCState
    finalV = if (v2 == Nothing) then Nothing else vBackAStep
    v2State = (bVertices state2)!(fromJust v2)
    nextB = inBlossomOrTrivial state2 (fromJust v2)
    nextBState = commonState state2 nextB
    vBackAStep = bcLabelEnd nextBState
    (nextV, nextW) = if (w == Nothing) then (finalV, w) else (w, finalV)

assignLabel :: Ord v => BlossomState v n l -> BlossomLabel -> v -> v -> BlossomState v n l
assignLabel state0 label v from = let
  b = inBlossomOrTrivial state0 v
  state1 = updateLabel state0 (Left v) label
  state2 = updateLabelEnd state1 (Left v) (Just from)
  state3 = updateLabel state2 b label
  state4 = updateLabelEnd state3 b (Just from)
  in state4

slack :: BlossomState v n l -> Edge v n l -> Int
slack s k = error "slack not implemented" -- 19791211

processAllowedEdge :: Ord v => BlossomState v n l -> Edge v n l -> BlossomState v n l

processAllowedEdge state edge = let
  Edge v w wt _ = edge
  blossomOfW = inBlossomOrTrivial state w
  labelOfW = bcLabel $ commonState state (Left w)
  labelOfBlossomOfW = bcLabel $ commonState state blossomOfW
  doScanBlossomThings = error "doScanBlossomThings not implemented"
  updateLabelsOnW0 = updateLabel state (Left w) T
  updateLabelsOnW = updateLabelEnd state (Left w) (Just v)
  returnVal
    | labelOfBlossomOfW == Free = assignLabel state T w v
    | labelOfBlossomOfW == S = doScanBlossomThings
    | labelOfW == Free && labelOfBlossomOfW /= T = error "why is this not T"
    | labelOfW == Free = updateLabelsOnW
  in returnVal

findAugmentingPathOrNewBlossom :: Ord v => BlossomState v n l -> Edge v n l -> BlossomState v n l
findAugmentingPathOrNewBlossom state0 edge = let
  Edge v w _ _ = edge
  (base, state1) = scanBlossom state0 v w
  in case base of Nothing -> error "augmentMatching not implemented"
                  Just baseVertex -> addBlossom state1 baseVertex

addBlossom :: BlossomState v n l -> v -> BlossomState v n l
addBlossom _ _ = error "addBlossom not implemented"

maybeUpdateBestEdge :: (Eq n, Eq l,  Ord v) => BlossomState v n l -> Either v BlossomID -> Edge v n l -> BlossomState v n l
maybeUpdateBestEdge state node k = let
  kSlack = slack state k
  oldBest = bcBestEdge $ commonState state node
  newBest
    | oldBest == Nothing = Just k
    | kSlack < (slack state $ fromJust oldBest) = Just k
    | otherwise = oldBest
  in updateBestEdge state node newBest

inBlossomOrTrivial :: (Ord v) => BlossomState v n l -> v -> Either v BlossomID
inBlossomOrTrivial state v = let
  blossomV = vInBlossom ((bVertices state)!v)
  in case blossomV of Nothing -> Left v
                      Just b -> Right b

scanNeighbors0 :: (Ord v, Ord n, Ord l) => BlossomState v n l -> v -> Maybe v -> [Edge v n l] -> (Maybe v, BlossomState v n l)
scanNeighbors0 state0 v base [] = (base, state0)
scanNeighbors0 state0 v base (k:xs) = let
  Edge _ w _ _ = k
  blossomV = inBlossomOrTrivial state0 v
  blossomW = inBlossomOrTrivial state0 w
  sameBlossom = blossomV == blossomW
  kSlack = slack state0 k
  newAllowedEdges = if (kSlack <= 0) then Set.insert k (bAllowedEdges state0) else (bAllowedEdges state0)
  kAllowed = Set.member k newAllowedEdges
  state1 = state0{bAllowedEdges = newAllowedEdges}
  state2
    | kAllowed = processAllowedEdge state1 k
    | (bcLabel $ commonState state1 blossomW) == S = maybeUpdateBestEdge state1 blossomW k
    | (bcLabel $ commonState state1 (Left w)) == Free = maybeUpdateBestEdge state1 (Left w) k
    | otherwise = state1
  newBase = base
  in case sameBlossom of True -> scanNeighbors0 state0 v base xs
                         False -> scanNeighbors0 state2 v newBase xs
   
