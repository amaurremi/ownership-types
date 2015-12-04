module CollectionFuncs where

import qualified Data.Set as Set
import qualified Data.Map as Map

(+++):: Ord a => a -> Set.Set a -> Set.Set a
(+++) = Set.insert

(⊆) :: Ord a => Set.Set a -> Set.Set a -> Bool
(⊆) = Set.isSubsetOf

dom = Map.keysSet

keyVals = Map.assocs

vals = Map.elems

newMap :: Ord a => [(a, b)] -> Map.Map a b
newMap = Map.fromList

newSet :: Ord a => [a] -> Set.Set a
newSet = Set.fromList

getVal :: Ord k => k -> Map.Map k a -> Maybe a
getVal = Map.lookup