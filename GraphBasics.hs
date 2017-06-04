-- An attempt at purely functional Data.Map-based adjacency list representations
-- of textbook graph.
module GraphBasics where

import Data.List (delete, foldl')
import qualified Data.Map as Map

data Edge v n l = Edge v v n l deriving (Eq, Show)
type Graph v n l = Map.Map v [Edge v n l]

insertEdge :: Ord v => Graph v n l -> Edge v n l -> Graph v n l
insertEdge graph (Edge from to weight label)
  | Map.notMember to graph = Map.insert to [] addNewEdge
  | otherwise              = addNewEdge
  where addNewEdge = Map.insertWith (++) from [Edge from to weight label] graph

reverseEdge :: Edge v n l -> Edge v n l
reverseEdge (Edge from to weight label) = Edge to from weight label

graphFromEdges :: Ord v => [Edge v n l] -> Graph v n l
graphFromEdges edges = foldl' insertEdge Map.empty edges

undirectedGraphFromEdges :: Ord v => [Edge v n l] -> Graph v n l
undirectedGraphFromEdges edges = foldl' insertDoubleEdge Map.empty edges

unweightedEdge :: (a, a) -> Edge a Int String
unweightedEdge (from, to) = Edge from to 1 "whatever"

outEdges :: Ord v => Graph v n l -> v -> [Edge v n l]
outEdges graph v = Map.findWithDefault [] v graph

deleteEdge :: (Eq v, Ord v, Show v, Real n, Eq l) => Graph v n l -> Edge v n l -> Graph v n l
deleteEdge graph edge = case children of
  [] -> error ("uh " ++ show f ++ "has no children what is this i dont even")
  [edge] -> Map.delete f graph
  _      -> Map.insert f (delete edge children) graph
  where children = outEdges graph f
        Edge f _ _ _ = edge

-- Maybe I should have undirected graph as its own type or something. This
-- works for now.
insertDoubleEdge :: Ord v => Graph v n l -> Edge v n l -> Graph v n l
insertDoubleEdge graph edge = insertEdge tempGraph (reverseEdge edge)
  where tempGraph = insertEdge graph edge

deleteDoubleEdge :: (Eq v, Ord v, Show v, Real n, Eq l) => Graph v n l -> Edge v n l -> Graph v n l
deleteDoubleEdge graph edge = deleteEdge tempGraph (reverseEdge edge)
  where tempGraph = deleteEdge graph edge
