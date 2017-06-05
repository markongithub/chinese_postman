module DijkstraSSSP where

-- import Debug.Trace (trace, traceShowId)
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import qualified Data.Set as Set
import GraphBasics

type DijkstraQueue v n l = Heap.MinPrioHeap n (Edge v n l)
data SSSPAnswer v n l = SSSPAnswer {cost :: n, predecessor :: Maybe (Edge v n l)} deriving (Eq, Show)
type SSSPCache v n l = Map.Map v (SSSPAnswer v n l)
type APSPCache v n l = Map.Map v (SSSPCache v n l)

dijkstra0 :: (Ord v, Real n, Show v, Show n, Show l) => Graph v n l -> DijkstraQueue v n l -> SSSPCache v n l -> Set.Set v -> SSSPCache v n l
dijkstra0 graph queue output targets
--  | trace ("Current queue: " ++ show queue) False = undefined
--  | trace ("Current cache: " ++ show output) False = undefined
--  | trace ("Current targets: " ++ show targets) False = undefined
  | (Map.size graph) == (Map.size output) = output
  | otherwise = case nextInQueue of
    Nothing -> output
    Just ((totalCost, edge), rest) ->
--      let Edge from to edgeCost _ = traceShowId edge
      let Edge from to newCost _ = edge
          doNothing = dijkstra0 graph rest output targets
          newQueue = foldl (queueEdge totalCost) rest (outEdges graph to)
          newAnswer = SSSPAnswer {cost = totalCost, predecessor = Just edge}
          newCache = Map.insert to newAnswer output
          newTargets = Set.delete to targets
          includeTo = dijkstra0 graph newQueue newCache newTargets
          in if (Set.null targets)
                then output
                else if (Map.member to output) then doNothing else includeTo
  where nextInQueue = Heap.view queue
 
queueEdge :: (Real n, Ord v, Show v, Show n, Show l) => n -> DijkstraQueue v n l -> Edge v n l -> DijkstraQueue v n l
queueEdge baseCost queue (Edge from to edgeCost label)
--  | trace ("Adding this edge with priority " ++ show newCost ++ " to the queue: " ++ show (Edge from to edgeCost label)) False = undefined
  | otherwise = Heap.insert (newCost, (Edge from to edgeCost label)) queue
  where newCost = baseCost + edgeCost
  
dijkstra :: (Real n, Ord v, Show v, Show n, Show l) => Graph v n l -> v -> Set.Set v -> SSSPCache v n l
dijkstra graph s t
  | otherwise = dijkstra0 graph initialQueue initialCache t
  where initialQueue = foldl (queueEdge 0)  Heap.empty (outEdges graph s)
        initialCache = Map.singleton s SSSPAnswer {cost = 0,
                                                   predecessor = Nothing}

shortestPath :: (Real n, Ord v, Show v, Show n, Show l) => Graph v n l -> v -> v -> [Edge v n l]
shortestPath graph source dest = let
  sssp = dijkstra graph source (Set.singleton dest)
  in tracePath sssp source dest []
 
getPredecessor :: (Ord v, Show v) => SSSPCache v n l -> v -> Maybe (Edge v n l)
getPredecessor cache vertex = predecessor answer
  where answer = Map.findWithDefault (error ("vertex missing:" ++ show vertex)) vertex cache

tracePath :: (Ord v, Show v) => SSSPCache v n l -> v -> v -> [Edge v n l] -> [Edge v n l]
tracePath cache source current path
  | source == current = path
  | otherwise = case (getPredecessor cache current) of
      Nothing -> error "Our path went dry. This should not happen."
      Just edge -> tracePath cache source predSource (edge : path)
        where Edge predSource _ _ _ = edge

-- This only works if the edge weights are positive. Maybe I should put in
-- a check for that.
allPointsShortestPathLimited :: (Real n, Ord v, Show v, Show n, Show l) => Graph v n l -> Set.Set v -> APSPCache v n l
allPointsShortestPathLimited graph targets = Map.fromSet (\s -> dijkstra graph s targets) targets

allPointsShortestPath :: (Real n, Ord v, Show v, Show n, Show l) => Graph v n l -> APSPCache v n l
allPointsShortestPath graph = let
  allVertices = Set.fromList $ Map.keys graph
  in allPointsShortestPathLimited graph allVertices
