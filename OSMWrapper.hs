module OSMWrapper where

import Control.Lens (view)
import Data.Geo.OSM (childrenL, foldChildren, idL, kL, latL, lonL, ndL, nodes, Node, NodeWayRelation, readOsmFile, refL, Way, tagsL, vL, ways)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import GraphBasics

lFail = error "i never learned to use Lens"

testFile = "./data/map1.osm"

parseFileIO :: String -> IO [NodeWayRelation]
parseFileIO path = do
  myOSM <- readOsmFile path
  let kids = view childrenL $ head myOSM
  let nwrs = foldChildren lFail lFail lFail lFail lFail id kids
  return nwrs

wayName :: Way -> String
wayName w = let
  nameTags = filter (\t -> (view kL t) == "name") (view tagsL w)
  in case (length nameTags) of
    0 -> "Nameless Way"
    1 -> view vL (head nameTags)
    _ -> error "More than one name wtf " ++ show nameTags

pairsOnList :: [a] -> [(a, a)]
pairsOnList [] = []
pairsOnList [x] = []
pairsOnList (x:y:ys) = (x,y):(pairsOnList (y:ys))

dedup :: (Eq a) => [a] -> [a]
dedup [] = []
dedup [x] = [x]
dedup (x:y:ys)
  | x == y    = dedup (y:ys)
  | otherwise = x:(dedup (y:ys))

edgesFromWay :: NodeIndex -> Way -> [Edge String Float String]
edgesFromWay index w = let
  nodesOnWay = map (view refL) $ view ndL w
  pairToEdge (x,y) = Edge x y (nodeDistance index x y) (wayName w)
  in map pairToEdge $ pairsOnList nodesOnWay

graphFromNWRs :: [NodeWayRelation] -> Graph String Float String
graphFromNWRs nwrs = let
  index = makeNodeIndex nwrs
  in undirectedGraphFromEdges  $ concat $ map (edgesFromWay index) $ filter (\w -> (wayName w) /= "Nameless Way") $ (ways nwrs)

graphFromOSMFile :: String -> IO (Graph String Float String)
graphFromOSMFile path = fmap graphFromNWRs $ parseFileIO path

betterNameVertex :: Graph String n String -> String -> String
betterNameVertex graph v = let
  getLabel (Edge _ _ _ label) = label
  streetNameSet = Set.fromList $ map getLabel $ outEdges graph v
  in v ++ (show $ Set.toList streetNameSet)
 
getLatLong :: Node -> (Float, Float)
getLatLong n = let
  parseMember f n = read $ view f n
  in (parseMember latL n, parseMember lonL n)

type NodeIndex = Map.Map String (Float, Float)

makeNodeIndex :: [NodeWayRelation] -> NodeIndex
makeNodeIndex nwrs = Map.fromList $ map (\n -> (view idL n, getLatLong n)) $ nodes nwrs

toRadians :: Float -> Float
toRadians d = pi * d / 180.0

-- adapted from
-- https://github.com/F6F/SimpleOsmRouter/blob/master/router/router.py
latLongDistance :: Float -> Float -> Float -> Float -> Float
latLongDistance x0 y0 x1 y1 = let
  [lat0, lon0, lat1, lon1] = map toRadians [x0, y0, x1, y1]
  dLat = lat1 - lat0
  dLon = lon1 - lon0
  a = (sin(dLat/2)**2) + (cos(lat0) * cos(lat1) * sin(dLon/2)**2)
  c = 2 * asin(sqrt(a)) 
  in 6367 * c

nodeDistance :: NodeIndex -> String -> String -> Float
nodeDistance index from to = let
  (x0, y0) = index!from
  (x1, y1) = index!to
  in latLongDistance x0 y0 x1 y1
