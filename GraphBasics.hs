-- An attempt at purely functional Data.Map-based adjacency list representations
-- of textbook graph.
module GraphBasics where

import Data.List (delete, foldl')
import qualified Data.Map as Map

type Vertex = Int
type Edge = (Vertex, WeightedDestination)
type Graph = Map.Map Vertex [WeightedDestination]
type WeightedDestination = (Vertex, Int)
type ShortestPathDistances = Map.Map Vertex Int

makeEdge :: Vertex -> WeightedDestination -> Edge
makeEdge vertex destination = (vertex, destination)

insertEdge :: Graph -> Edge -> Graph
insertEdge graph (from, (to, weight))
  | Map.notMember to graph = Map.insert to [] addNewEdge
  | otherwise              = addNewEdge
  where addNewEdge = Map.insertWith (++) from [(to, weight)] graph

reverseEdge :: Edge -> Edge
reverseEdge (from, (to, weight)) = (to, (from, weight))

graphFromEdges :: [Edge] -> Graph
graphFromEdges edges = foldl' insertEdge Map.empty edges

undirectedGraphFromEdges :: [Edge] -> Graph
undirectedGraphFromEdges edges = foldl' insertDoubleEdge Map.empty edges

unweightedEdge :: (Vertex, Vertex) -> Edge
unweightedEdge (from, to) = (from, (to, 1))

outDests :: Graph -> Vertex -> [WeightedDestination]
outDests graph v = Map.findWithDefault [] v graph

outEdges :: Graph -> Vertex -> [Edge]
outEdges graph v = map (\(t, w) -> (v, (t, w))) $ outDests graph v

deleteEdge :: Graph -> Edge -> Graph
deleteEdge graph (f, wd) = case children of
  [] -> error ("uh " ++ show f ++ "has no children what is this i dont even")
  [wd] -> Map.delete f graph
  _ -> Map.insert f (delete wd children) graph
  where children = outDests graph f

-- Maybe I should have undirected graph as its own type or something. This
-- works for now.
insertDoubleEdge :: Graph -> Edge -> Graph
insertDoubleEdge graph edge = insertEdge tempGraph (reverseEdge edge)
  where tempGraph = insertEdge graph edge

deleteDoubleEdge :: Graph -> Edge -> Graph
deleteDoubleEdge graph edge = deleteEdge tempGraph (reverseEdge edge)
  where tempGraph = deleteEdge graph edge

