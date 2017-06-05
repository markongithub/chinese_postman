module ChinesePostman where

import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Debug.Trace (trace)
import DijkstraSSSP
import GraphBasics
import Matching

-- I based this implementation on the example in
-- http://www.geeksforgeeks.org/hierholzers-algorithm-directed-graph/

data HierholzerState v n l = HierholzerState { graph :: Graph v n l
                                             , curPath :: [v]
                                             , circuit :: [v] } deriving Show

backtrack :: HierholzerState v n l -> HierholzerState v n l
backtrack (HierholzerState graph0 curPath0 circuit0)
  | null curPath0 = error "Nowhere to backtrack!"
--  | trace ("backtracking from " ++ show (head curPath0)) False = undefined
  | otherwise = HierholzerState graph0 (tail curPath0) newCircuit
  where newCircuit = ((head curPath0):circuit0)

advance :: (Eq l, Ord v, Real n, Show v) => HierholzerState v n l -> HierholzerState v n l
advance (HierholzerState graph0 curPath0 circuit0)
--  | trace ("Advancing from " ++ show curVertex ++ " to " ++ show nextVertex) False = undefined
   | otherwise = HierholzerState newGraph newPath circuit0
  where curVertex = head curPath0
        nextEdge = head $ outEdges graph0 curVertex
        newGraph = deleteDoubleEdge graph0 nextEdge
        Edge _ nextVertex _ _ = nextEdge
        newPath = (nextVertex:curPath0)

hierholzer0 :: (Eq l, Ord v, Real n, Show v) => HierholzerState v n l -> [v]
hierholzer0 state
--  | trace ("circuit0: " ++ show circuit0) False = undefined
  | allDone = revConcat curPath0 circuit0 -- no more edges, we are done
  | atDeadEnd = hierholzer0 $ backtrack state
  | otherwise = hierholzer0 $ advance state
  where (HierholzerState graph0 curPath0 circuit0) = state
        curVertex = head curPath0
        atDeadEnd = null $ outEdges graph0 curVertex
        allDone = null graph0

hierholzer :: (Eq l, Ord v, Real n, Show v) => Graph v n l -> v -> [v]
hierholzer graph vertex
  | numOddDegrees == 2 && not (isOdd $ outDegree graph vertex) = error "A circuit must start from a vertex of odd degree"
  | numOddDegrees == 2 || numOddDegrees == 0 = proceed
  | otherwise = error "this graph is non-Eulerian"
  where proceed = hierholzer0 $ HierholzerState graph [vertex] []
        numOddDegrees = countOddDegrees graph

isOdd :: Int -> Bool
isOdd x = x `mod` 2 == 1

countOddDegrees :: Graph v n l -> Int
countOddDegrees graph = length $ filter isOdd $ map length $ Map.elems graph

outDegree :: Ord v => Graph v n l -> v -> Int
outDegree graph vertex = length $ outEdges graph vertex

revConcat :: [v] -> [v] -> [v]
revConcat [] forward = forward
revConcat (x:xs) forward = revConcat xs (x:forward)

testGraph :: Graph Int Int String
testGraph = undirectedGraphFromEdges $ map unweightedEdge
  [ (0, 1)
  , (0, 3)
  , (1, 2)
  , (2, 3)
  , (3, 4)
  , (3, 5)
  , (4, 5)
  , (5, 6)
  , (5, 7)
  , (6, 8)
  , (7, 8)
  , (8, 9)
  , (8, 11)
  , (9, 10)
  , (10, 11)
  ]

-- a test from https://math.stackexchange.com/questions/1595157/is-it-possible-to-draw-this-picture-without-lifting-the-pen
testGraph2 = undirectedGraphFromEdges $ map unweightedEdge
  [ (0, 1)
  , (0, 2)
  , (1, 2)
  , (1, 3)
  , (1, 4)
  , (2, 3)
  , (2, 4)
  , (3, 4)
  , (3, 5)
  , (4, 5)
  ]

-- this one is only semi-Eulerian
testGraph3 = undirectedGraphFromEdges $ map unweightedEdge
  [ (0, 1)
  , (0, 2)
  , (1, 3)
  , (2, 3)
  , (2, 4)
  , (3, 5)
  , (4, 5)
  ]

data NoLabel = NoLabel deriving (Eq, Show)

verticesOfOddDegree :: Graph v n l -> [v]
verticesOfOddDegree graph = map fst $ filter (isOdd . length . snd) $ Map.toList graph

fakeEdgesFromSSSP :: v -> SSSPCache v n l -> [Edge v n NoLabel]
fakeEdgesFromSSSP source sssp = let
  answerToEdge (to, SSSPAnswer weight _) = Edge source to weight NoLabel
  in map answerToEdge $ Map.toList sssp

fakeGraphOfOddVertices :: (Real n, Ord v, Show v, Show n, Show l) => Graph v n l -> Graph v n NoLabel
fakeGraphOfOddVertices graph = let
  oddVertices = Set.fromList $ verticesOfOddDegree graph
  apsp = allPointsShortestPathLimited graph oddVertices
  cacheToEdges (from, cache) = fakeEdgesFromSSSP from cache
  in graphFromEdges $ concat $ map cacheToEdges $ Map.toList apsp

makeGraphEulerian :: (Real n, Ord v, Show v, Show n) => Graph v n String -> Graph v n String
makeGraphEulerian graph = let
  fakeGraph = fakeGraphOfOddVertices graph
  matching = perfectMatching fakeGraph
  relabel (Edge f t w l) = Edge f t w "virtual backtracker edge"
  in case matching of
    NoMatching -> error "we failed"
    PerfectMatching edges _ -> foldl insertDoubleEdge graph $ map relabel edges
