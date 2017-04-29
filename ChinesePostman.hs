module ChinesePostman where

import Debug.Trace
import GraphBasics
import qualified Data.Map as Map (elems)

-- I based this implementation on the example in
-- http://www.geeksforgeeks.org/hierholzers-algorithm-directed-graph/

data HierholzerState = HierholzerState { graph :: Graph 
                                       , curPath :: [Vertex]
                                       , circuit :: [Vertex] } deriving Show

backtrack :: HierholzerState -> HierholzerState
backtrack (HierholzerState graph0 curPath0 circuit0)
  | null curPath0 = error "Nowhere to backtrack!"
  | trace ("backtracking from " ++ show (head curPath0)) False = undefined
  | otherwise = HierholzerState graph0 (tail curPath0) newCircuit
  where newCircuit = ((head curPath0):circuit0)

advance :: HierholzerState -> HierholzerState
advance (HierholzerState graph0 curPath0 circuit0)
  | trace ("Advancing from " ++ show curVertex ++ " to " ++ show nextVertex) False = undefined
   | otherwise = HierholzerState newGraph newPath circuit0
  where curVertex = head curPath0
        nextEdge = head $ outEdges graph0 curVertex
        newGraph = deleteDoubleEdge graph0 nextEdge
        (_, (nextVertex, _)) = nextEdge
        newPath = (nextVertex:curPath0)

hierholzer0 :: HierholzerState -> [Vertex]
hierholzer0 state
  | trace ("circuit0: " ++ show circuit0) False = undefined
  | allDone = revConcat curPath0 circuit0 -- no more edges, we are done
  | atDeadEnd = hierholzer0 $ backtrack state
  | otherwise = hierholzer0 $ advance state
  where (HierholzerState graph0 curPath0 circuit0) = state
        curVertex = head curPath0
        atDeadEnd = null $ outEdges graph0 curVertex
        allDone = null graph0

hierholzer :: Graph -> Vertex -> [Vertex]
hierholzer graph vertex
  | numOddDegrees == 2 && not (isOdd $ outDegree graph vertex) = error "A circuit must start from a vertex of odd degree"
  | numOddDegrees == 2 || numOddDegrees == 0 = proceed
  | otherwise = error "this graph is non-Eulerian"
  where proceed = hierholzer0 $ HierholzerState graph [vertex] []
        numOddDegrees = countOddDegrees graph

isOdd :: Int -> Bool
isOdd x = x `mod` 2 == 1

countOddDegrees :: Graph -> Int
countOddDegrees graph = length $ filter isOdd $ map length $ Map.elems graph

outDegree :: Graph -> Vertex -> Int
outDegree graph vertex = length $ outDests graph vertex

revConcat :: [a] -> [a] -> [a]
revConcat [] forward = forward
revConcat (x:xs) forward = revConcat xs (x:forward)

testGraph :: Graph
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

