module Lib.Graph where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

data Graph a = Graph {
  vertices :: M.Map Int a,
  edges    :: M.Map Int (S.Set Int),
  next     :: Int
}

{-|
   Returns a graph containing a single vertex
-}
makeGraph :: a -> Graph a
makeGraph vertex = Graph
  (M.insert 2 vertex M.empty)
  (M.insert 2 (S.singleton 1) (M.insert 0 (S.singleton 2) M.empty))
  3

{-|
  Insert a vertex and a list of directed edges from the vertex into the graph
-}
insertVertex :: a -> [Int] -> Graph a -> Graph a
insertVertex v e g = let
  vertexId = next g
  inserted = g {
        vertices = M.insert vertexId v (vertices g),
        edges = let
            oldHead = fromJust $ M.lookup 0 (edges g)
          in
            M.insert 0 (S.insert vertexId oldHead) (M.insert vertexId (S.singleton 1) (edges g)),
        next = vertexId + 1
  }
  addEdges = map (\x -> (vertexId, x)) e
  newGraph = foldr insertEdge inserted addEdges
  in newGraph

{-|
   Insert an edge into the graph from v1 to v2
-}
insertEdge :: (Int, Int) -> Graph a -> Graph a
insertEdge (v1, v2) g = let
  edges' = case M.lookup v1 (edges g) of
    Just a -> S.insert v2 a
    Nothing -> S.singleton v2
  -- The to vertex is no longer connected to the head
  fixHead = case M.lookup 0 (edges g) of
    Just a -> S.delete v2 a
    Nothing -> S.empty
  -- The from vertex is no longer connected to the tail
  fixTail = case M.lookup 1 (edges g) of
    Just a -> S.delete v1 a
    Nothing -> S.empty in
  g {edges = M.insert 0 fixHead
      $ M.insert 1 fixTail
      $ M.insert v1 edges' (edges g)}

{-|
   Get the vertex associated with a number
-}
getVertex :: Int -> Graph a -> Maybe a
getVertex i g = M.lookup i (vertices g)

{-|
   Get the edges associated with a number
-}
getEdges :: Int -> Graph a -> Maybe (S.Set Int)
getEdges i g = M.lookup i (edges g)

{-|
   Get the reference to the last inserted vertex
-}
getLatest :: Graph a -> Int
getLatest g = next g - 1

{-|
  Construct a new graph which has g2 following g1
-}
sequence :: Graph a -> Graph a -> Graph a
sequence g1 g2 = let
  Graph vert1 edge1 next1 = g1
  Graph vert2 edge2 next2 = offset g2 next1
  edge1' = fmap (S.map (\x -> if x == 1 then next1 else x)) edge1 in
  Graph (M.union vert1 vert2) (M.union edge1' edge2) next2

{-|
  Increments all the ids in the graph by n
-}
offset :: Graph a -> Int -> Graph a
offset g n = let
  newVerts = M.mapKeys (+n) (vertices g)
  newEdges = fmap (S.map (incEdgesBy n)) (edges g)
  in g{vertices = newVerts,
       edges = newEdges,
       next = next g + n
      }
  -- Don't increment edges to the tail
  where incEdgesBy amt x = if x == 1 then 1 else x + amt
