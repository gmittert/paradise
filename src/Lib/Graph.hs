module Lib.Graph where
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Graph a = Graph {
  vertices :: (M.Map Int a),
  edges    :: (M.Map Int (S.Set Int)),
  next     :: Int
}

{-|
  Insert a vertex and a list of directed edges from the vertex into the graph
-}
insertVertex :: a -> [Int] -> Graph a -> Graph a
insertVertex v e g = g {
  vertices = M.insert (next g) v (vertices g),
  edges = M.insert (next g) (S.fromList e) (edges g),
  next = next g + 1
  }

{-|
   Insert an edge into the graph
-}
insertEdge :: (Int, Int) -> Graph a -> Graph a
insertEdge (v1, v2) g = let
  edges' = case M.lookup v1 (edges g) of
    Just a -> S.insert v2 a
    Nothing -> S.singleton v2 in
  g {edges = M.insert v1 edges' (edges g)}


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
