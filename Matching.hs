module Matching where

import qualified Data.Map as Map (elems)
import qualified Data.Set as Set
-- import Debug.Trace (trace)
import GraphBasics

type VertexSet a = Set.Set a

data Matching v n l = NoMatching | PerfectMatching [Edge v n l] n deriving (Eq, Show)

instance (Eq v, Eq n, Eq l, Ord n) => Ord (Matching v n l) where
  compare m1 m2
    | m1 == m2 = EQ
  compare NoMatching (PerfectMatching _ _) = LT
  compare (PerfectMatching _ _) NoMatching = GT
  compare (PerfectMatching _ w1) (PerfectMatching _ w2) = compare w2 w1

data SortableEdge v n l = SortableEdge (Edge v n l) deriving (Eq, Show)
instance (Eq l, Ord v, Ord n) => Ord (SortableEdge v n l) where
  compare e1 e2
    | e1 == e2 = EQ
  compare (SortableEdge (Edge f1 t1 w1 l1)) (SortableEdge (Edge f2 t2 w2 l2))
    | w1 /= w2 = compare w1 w2
    | f1 /= f2 = compare f1 f2
    | otherwise = compare t1 t2
 
weightSum :: Real n => [Edge v n l] -> n
weightSum edges = sum $ map (\(Edge _ _ w _) -> w) edges

data MatchingState v n l = MatchingState (Set.Set (SortableEdge v n l)) (VertexSet v) (VertexSet v) (Matching v n l) n [Edge v n l] deriving (Eq, Show)

perfectMatching0 :: (Eq v, Eq n, Eq l, Ord v, Real n) => MatchingState v n l -> Matching v n l
perfectMatching0 (MatchingState edgesLeft covered uncovered best weightSoFar pathSoFar)
--  | traceShow (MatchingState edgesLeft covered uncovered best weightSoFar pathSoFar) False = undefined
  | PerfectMatching [] (weightSoFar + weight) <= best = NoMatching
  | Set.null uncovered = PerfectMatching pathSoFar weightSoFar
  | (Set.size edgesLeft * 2) < (Set.size uncovered) = NoMatching
  | headEdgeInappropriate = notUsingHeadEdge
  | otherwise = max usingHeadEdge notUsingHeadEdge
  where SortableEdge headEdge = Set.findMin edgesLeft
        Edge from to weight _ = headEdge
        newWSF = weightSoFar + weight
        newPSF = (headEdge:pathSoFar)
        headEdgeInappropriate = Set.member from covered || Set.member to covered
        usingHeadEdge = perfectMatching0 (MatchingState newEdgesLeft newCovered newUncovered best newWSF newPSF)
        notUsingHeadEdge = perfectMatching0 (MatchingState newEdgesLeft covered uncovered newBest weightSoFar pathSoFar)
        newEdgesLeft = Set.deleteMin edgesLeft
        newCovered = Set.insert from $ Set.insert to covered
        newUncovered = Set.delete from $ Set.delete to uncovered
        newBest = if headEdgeInappropriate then best else max usingHeadEdge best

perfectMatching :: (Eq v, Eq n, Eq l, Ord v, Real n) => Graph v n l -> Matching v n l
perfectMatching graph = let
  edgesLeft = Set.fromList $ map SortableEdge $ edges graph
  covered = Set.empty
  uncovered = Set.fromList $ vertices graph
  initialState = MatchingState edgesLeft covered uncovered NoMatching 0 []
  in perfectMatching0 initialState

testEdges = [ Edge 0 1 2 ""
            , Edge 0 2 1 ""
            , Edge 0 3 10 ""
            , Edge 1 2 10 ""
            , Edge 1 3 3 ""
            , Edge 2 3 4 ""
            ]
testPerfectMatching = perfectMatching $ graphFromEdges testEdges
