module HopcroftKarp where

import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Sequence ((|>), ViewL(..), Seq)
import Data.Set (Set)
-- import Debug.Trace (trace)
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)
import GraphBasics

-- An attempt at a functional implementation of
-- https://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm#Pseudocode

-- A lot of descriptions keep two sets of vertices. But I figure this isn't
-- any less efficient. I just keep the partition assignment in the "vertex
-- state" and map from vertices to vertex states.
data Partition = Pitcher | Catcher deriving (Eq, Ord, Show)
data HKDistance = FiniteDistance Int | Infinity deriving (Eq, Show)

-- I could use a "Maybe Int" where Nothing==infinity but this algorithm is
-- already confusing so I think this will be more intuitive later.
instance Ord HKDistance where
  compare Infinity (FiniteDistance _) = GT
  compare (FiniteDistance _) Infinity = LT
  compare Infinity Infinity = EQ
  compare (FiniteDistance d1) (FiniteDistance d2) = compare d1 d2

onePlus :: HKDistance -> HKDistance
onePlus Infinity = error "You can't onePlus infinity. Duh."
onePlus (FiniteDistance d) = FiniteDistance (d + 1)

data HKVertexState v = HKVertexState {
   vPartition :: Partition
 , vPair :: (Maybe v)
 , vDistance :: HKDistance
 } deriving (Eq, Ord, Show)

-- A Hopcroft-Karp state includes the whole graph, the mapping from vertices to
-- vertex states, and the distance to the virtual Nil vertex. Other
-- implementations treat Nil as an actual vertex but they aren't married to
-- ultra-strict type systems like I am!
data HKState v n l = HKState { hkGraph :: Graph v n l
                             , hkMapping :: Map v (HKVertexState v)
                             , hkDistNil :: HKDistance }  deriving (Eq, Show)

-- partitionFromSet is an inner function of partition. After we have the set of
-- all pitchers, partitionFromSet just does the ugly work of creating an
-- HKState with all the HKVertexStates.
partitionFromSet :: (Ord v, Show v) => Graph v n l -> Set v -> HKState v n l
partitionFromSet graph pitchers = let
  allEdges = edges graph
  membership v = if Set.member v pitchers then Pitcher else Catcher
  transformVertex v = HKVertexState (membership v) Nothing Infinity
  stateMap = Map.fromSet transformVertex (Set.fromList $ vertices graph)
  in HKState graph stateMap Infinity

partition0 :: (Ord v, Show v) => Graph v n l -> Set v -> Set v -> Set v -> [v] -> HKState v n l
-- By this time we've assigned all the vertices to one set or the other.
partition0 graph pitchers _ _ [] = partitionFromSet graph pitchers
partition0 graph pitchers catchers seen (vertex:xs)
--  | trace (show (pitchers, catchers, seen, vertex)) False = undefined
  -- If we've seen this vertex before we can ignore it.
  | Set.member vertex seen = partition0 graph pitchers catchers seen xs
  -- If this vertex is a pitcher we make sure none of its neighbors are
  -- also pitchers and throw an error if they are.
  | Set.member vertex pitchers = if (any (\n -> Set.member n pitchers) (neighbors graph vertex)) then error (show vertex ++ "is a Pitcher and has Pitcher neighbors among " ++ (show $ neighbors graph vertex)) else partition0 graph pitchers newCatchers newSeen newStack
  -- If this vertex is a catcher we make sure none of its neighbors are
  -- also catchers and throw an error if they are.
  | Set.member vertex catchers = if (any (\n -> Set.member n catchers) (neighbors graph vertex)) then error (show vertex ++ "is a Catcher and has Catcher neighbors among " ++ (show $ neighbors graph vertex)) else partition0 graph newPitchers catchers newSeen newStack
  -- otherwise put it in pitcher?
  | otherwise = partition0 graph (Set.insert vertex pitchers) catchers seen (vertex:xs)
  where newPitchers = foldr Set.insert pitchers (neighbors graph vertex)
        newCatchers = foldr Set.insert catchers (neighbors graph vertex)
        newSeen = Set.insert vertex seen
        newStack = (neighbors graph vertex) ++ xs

partition :: (Ord v, Show v) => Graph v n l -> HKState v n l
partition graph = let
  initialPitchers = Set.empty
  initialCatchers = Set.empty
  initialSeen = Set.empty
  initialStack = vertices graph
  in partition0 graph initialPitchers initialCatchers initialSeen initialStack

testGraph = partition $ undirectedGraphFromEdges [
    Edge 0 1 1 0
  , Edge 0 3 1 0
  , Edge 2 1 1 0
  , Edge 2 5 1 0
  , Edge 4 1 1 0
  , Edge 4 5 1 0
  ]

testGraph2 = partition $ undirectedGraphFromEdges [
    Edge 0 1 1 0
  , Edge 2 3 1 0
  , Edge 4 5 1 0
  ]

isPitcher :: (Ord v) => HKState v n l -> v -> Bool
isPitcher (HKState graph mapping distNil) v = vPartition (mapping!v) == Pitcher

isFree :: (Ord v) => HKState v n l -> v -> Bool
isFree (HKState graph mapping distNil) v = vPair (mapping!v) == Nothing

-- Implementation of the BFS function from Wikipedia's pseudocode. That one
-- returns a Bool but has side effects, so this one also returns an HKState.
breadthFirstHK :: (Ord v, Show v, Show n, Show l) => HKState v n l -> (Bool, HKState v n l)
breadthFirstHK state = let
  HKState graph mapping distNil = state
  -- FIX THIS to mapping!v
  vState x = Map.findWithDefault (error "fuck") x mapping
  initialDist x = if (isFree state x) then (FiniteDistance 0) else Infinity
  newVState x = (mapping!x){vDistance = initialDist x}
  pitchers = filter (isPitcher state) $ vertices graph
  -- updateMap is the function we are going to fold to put all the new
  -- distances into the vertex state map.
  updateMap k m = Map.insert k (newVState k) m
  newMapping = foldr updateMap mapping pitchers 
  freePitchers = filter (isFree state) pitchers
  initialQueue = Sequence.fromList freePitchers
  in breadthFirstHK0 (HKState graph newMapping Infinity) initialQueue

breadthFirstHK0 :: (Ord v, Show v, Show n, Show l) => HKState v n l -> Seq v -> (Bool, HKState v n l)
breadthFirstHK0 state queue
--  | traceShow (mapping, queue, distNil) False = undefined
  -- when the queue is empty we just return whether or not distNil is infinite.
  | Sequence.null queue = (distNil /= Infinity, state)
  | distU < distNil = processU
  | otherwise = ignoreU
  where u :< remainder = Sequence.viewl queue
        distU = vDistance $ mapping!u
        ignoreU = breadthFirstHK0 state remainder
        HKState graph mapping distNil = state
        -- if distNil is infinite and any of u's neighbors have Nil as a pairing
        -- we set distNil to distU+1
        newDistNil = if (distNil == Infinity && any (isFree state) (neighbors graph u)) then (onePlus distU) else distNil
        -- get neighbors with pairings but with dist=Nothing
        shouldUpdate v = case (vPair $ mapping!v) of Nothing      -> False
                                                     Just pairOfV -> (vDistance $ mapping!pairOfV) == Infinity
        toUpdate = filter shouldUpdate (neighbors graph u)
        justPairOf neighbor = fromJust $ vPair $ mapping!neighbor
        -- helper function to set a distance for pairs of neighbors of u
        updateMap neighbor m = Map.adjust (\s -> s{vDistance = onePlus distU}) (justPairOf neighbor) m
        newMapping = foldr updateMap mapping toUpdate
        -- add all the pairs of u's neighbors to the queue
        newQueue = foldl (|>) remainder $ map (\v -> fromJust $ vPair $ mapping!v) toUpdate
        processU = breadthFirstHK0 (HKState graph newMapping newDistNil) newQueue

-- depthFirstHK is called on one vertex (which could be Nil) and then iterates
-- through that vertex's neighbors. It's always true for Nil.
depthFirstHK :: (Ord v, Show v, Show n, Show l) => HKState v n l -> Maybe v -> (Bool, HKState v n l)
depthFirstHK state Nothing = (True, state)
depthFirstHK state (Just u) = depthFirstHK0 state u (neighbors graph u)
  where HKState graph _ _ = state

depthFirstHK0 :: (Ord v, Show v, Show n, Show l) => HKState v n l -> v -> [v] -> (Bool, HKState v n l)
-- if we've gotten to the end of the neighbors, we set dist[u] to infinite and
-- return False.
depthFirstHK0 (HKState graph mapping distNil) u [] = let
  newUEntry = (mapping!u){vDistance = Infinity}
  newMap = Map.insert u newUEntry mapping
  in (False, HKState graph newMap distNil)
depthFirstHK0 state u (x:xs)
  | trace ("DFS " ++ show (u, distNil, mapping)) False = undefined
  -- we're looking for "if Dist[ Pair_V[v] ] == Dist[u] + 1"
  -- if that equation holds, then we call DFS again for "lowerVerdict"
  -- if the DFS returns true, we can consider u and pairOfX a match.
  | (distPX == onePlus distU) && lowerVerdict = (True, makePairing)
  | otherwise = recurseXS
  where
    HKState graph mapping distNil = state
    distU = vDistance (mapping!u)
    pairOfX = vPair (mapping!x)
    justPX = fromJust pairOfX
    distPX = if (pairOfX == Nothing) then distNil else vDistance (mapping!justPX)
    (lowerVerdict, newState) = depthFirstHK state pairOfX
    newUEntry = (mapping!u){vPair = Just x}
    newXEntry = (mapping!x){vPair = Just u}
    newMap = Map.insert u newUEntry $ Map.insert x newXEntry mapping
    makePairing = HKState graph newMap distNil
    recurseXS = depthFirstHK0 state u xs

-- I split the outer HopcroftKarp routines into hKarp and hKarpIterate.
-- recursing down hKarpIterate handles the "for each u in U" loop.
hKarpIterate :: (Ord v, Show v, Show n, Show l) => HKState v n l -> Int -> (Int, HKState v n l)
hKarpIterate (HKState graph mapping distNil) matching = let
  pitchers = filter (\v -> vPartition (mapping!v) == Pitcher) $ vertices graph
  in hKarpIterate0 (HKState graph mapping distNil) matching pitchers

hKarpIterate0 :: (Ord v, Show v, Show n, Show l) => HKState v n l -> Int -> [v] -> (Int, HKState v n l)
hKarpIterate0 state matching [] = (matching, state)
hKarpIterate0 state matching (u:xs)
  | trace ("HKR " ++ show (u, mapping)) False = undefined
  | vPair (mapping!u) == Nothing && lowerVerdict = hKarpIterate0 newState (matching + 1) xs
  | otherwise = hKarpIterate0 newState matching xs
  where (lowerVerdict, newState) = depthFirstHK state (Just u)
        HKState graph mapping _ = state

-- recursing down hKarp handles the "while BFS is true" loop.
hKarp :: (Ord v, Show v, Show n, Show l) => HKState v n l -> Int -> (Int, HKState v n l)
hKarp state matching = let
  (bfsVerdict, bfsState) = breadthFirstHK state
  (nextMatching, nextState) = hKarpIterate bfsState matching
  HKState graph mapping distNil = state
  -- this is so ugly but I don't know a better way of getting the trace in.
  doStuffAndDebug
    | trace ("HK " ++ show mapping) False = undefined
    | otherwise = case bfsVerdict of False -> (matching, bfsState)
                                     True  -> hKarp nextState nextMatching
  in doStuffAndDebug

runTest = hKarp testGraph2 0
