-- File created: 2009-01-06 12:59:53

{-# LANGUAGE TemplateHaskell #-}

module Tests.Properties (tests) where

import Control.Arrow                       ((&&&))
import Data.Maybe                          (fromJust, isNothing)

import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck                     ((==>))

import qualified Data.Trie.Set.Eq
import qualified Data.Trie.Set.Ord
import qualified Data.Trie.Set.Enum
import qualified Data.Trie.Map.Eq
import qualified Data.Trie.Map.Ord
import qualified Data.Trie.Map.Enum
import qualified Data.Trie.Patricia.Set.Eq
import qualified Data.Trie.Patricia.Set.Ord
import qualified Data.Trie.Patricia.Set.Enum
import qualified Data.Trie.Patricia.Map.Eq
import qualified Data.Trie.Patricia.Map.Ord
import qualified Data.Trie.Patricia.Map.Enum

import Tests.Base
import Tests.TH

-- List of tests is at the bottom because it doesn't work at the top: looks
-- like a TH limitation.

-- The size of a set built from a list should be <= the length of the list
$(makeFunc allTries ["fromList","size"] [d|
   prop_size1 fromList size l_ = let l = map unArb l_
                                  in size (fromList l :: TrieType) <= length l_
 |])

-- The size of a set should be == its length in list form
$(makeFunc allTries ["toList","size"] [d|
   prop_size2 toList size m = size (m :: TrieType) == length (toList m)
 |])

-- A set built from a list should include all elements of the original list
$(makeFunc allTries ["fromList","member"] [d|
  -- using flip avoids GHC #2956
  prop_member1 fromList member l_ =
     let l = map unArb l_
         m = fromList l :: TrieType
      in all (flip member m . getKey) (l :: [ListElemType])
 |])

-- A map built from a list should have the same key/value pairs as the list
$(makeFunc mapsOnly ["fromList","lookup"] [d|
   prop_lookup1 fromList lookup l_ =
      let l = map unArb l_
          m = fromList l :: TrieType
       in all (\(k,v) -> fromJust (lookup k m) == v) l
 |])

-- lookupWithDefault should return the default if the key is not a member of
-- the map
$(makeFunc mapsOnly ["lookupWithDefault","empty"] [d|
   prop_lookupWithDefault1 lookupWithDefault empty k v =
      lookupWithDefault v (unArb k) (empty :: TrieType) == v
 |])

-- Sets/maps should be subsets/submaps of themselves
$(makeFunc setsOnly ["isSubsetOf"] [d|
   prop_isSubsetOf1 isSubsetOf m = isSubsetOf m (m :: TrieType)
 |])
$(makeFunc mapsOnly ["isSubmapOf"] [d|
   prop_isSubmapOf1 isSubmapOf m = isSubmapOf m (m :: TrieType)
 |])

-- Sets/maps should not be proper subsets/submaps of themselves
$(makeFunc setsOnly ["isProperSubsetOf"] [d|
   prop_isProperSubsetOf1 isProperSubsetOf m =
      not (isProperSubsetOf m (m :: TrieType))
 |])
$(makeFunc mapsOnly ["isProperSubmapOf"] [d|
   prop_isProperSubmapOf1 isProperSubmapOf m =
      not (isProperSubmapOf m (m :: TrieType))
 |])

-- Looking up a singleton's key in a singleton should return the singleton's
-- value
$(makeFunc mapsOnly ["lookup","singleton"] [d|
   prop_singleton1 lookup singleton k_ v =
      let k = unArb k_
       in (fromJust . lookup k) (singleton k v :: TrieType) == v
 |])

-- Inserting a value into a map and then looking it up should return that value
-- (note: regardless of whether it was there previously, the new value should
-- overwrite)
$(makeFunc mapsOnly ["lookup","insert"] [d|
   prop_insert1 lookup insert m k_ v =
      let k = unArb k_
       in (fromJust . lookup k . insert k v) (m :: TrieType) == v
 |])

-- Deleting a key means it should no longer be in the set
$(makeFunc allTries ["notMember","delete"] [d|
   prop_delete1 notMember delete k_ m =
      let k = unArb k_
       in notMember k . delete k $ (m :: TrieType)
 |])

-- A union should include all keys of the original sets
$(makeFunc allTries ["union","member","toList"] [d|
   prop_union1 union member toList m n =
      let u = union m (n :: TrieType)
       in all (flip member u . getKey) (toList m ++ toList n)
 |])

-- Union with empty is the identity function
$(makeFunc allTries ["union","empty"] [d|
   prop_union2 union empty m = union m empty == (m :: TrieType)
 |])
$(makeFunc allTries ["union","empty"] [d|
   prop_union3 union empty m = union empty m == (m :: TrieType)
 |])

-- Difference with oneself should result in an empty set
$(makeFunc allTries ["null","difference"] [d|
   prop_difference1 null difference m = null (difference m (m :: TrieType))
 |])

-- Difference with empty is the identity function
$(makeFunc allTries ["empty","difference"] [d|
   prop_difference2 difference empty m = difference (m :: TrieType) empty == m
 |])

-- Difference of anything from empty should stay empty
$(makeFunc allTries ["empty","difference","null"] [d|
   prop_difference3 difference empty null m =
      null $ difference empty (m :: TrieType)
 |])

-- Intersection with oneself is the identity function
$(makeFunc allTries ["intersection"] [d|
   prop_intersection1 intersection m = intersection m m == (m :: TrieType)
 |])

-- Intersection with empty should result in the empty set
$(makeFunc allTries ["intersection","null","empty"] [d|
   prop_intersection2 intersection null empty =
      null . intersection (empty :: TrieType)
 |])

-- The maximum of the left side of a split about k is the predecessor of k
$(makeFunc allTries ["split","findMax","findPredecessor"] [d|
   prop_splitMaxPredecessor split findMax findPredecessor m k_ =
      let k = unArb k_
          (a,_) = split k (m :: TrieType)
       in findMax a == findPredecessor m k
 |])
-- The minimum of the right side of a split about k is the successor of k
$(makeFunc allTries ["split","findMin","findSuccessor"] [d|
   prop_splitMinSuccessor split findMin findSuccessor m k_ =
      let k = unArb k_
          (_,b) = split k (m :: TrieType)
       in findMin b == findSuccessor m k
 |])

-- min/maxView should be equivalent to separately finding and deleting the
-- min/max
$(makeFunc allTries ["minView","findMin","deleteMin"] [d|
   prop_minView1 minView findMin deleteMin m =
      minView m == (findMin &&& deleteMin) (m :: TrieType)
 |])
$(makeFunc allTries ["maxView","findMax","deleteMax"] [d|
   prop_maxView1 maxView findMax deleteMax m =
      maxView m == (findMax &&& deleteMax) (m :: TrieType)
 |])

-- [] has no predecessor
$(makeFunc allTries ["findPredecessor"] [d|
   prop_findPredecessor1 findPredecessor m =
      isNothing (findPredecessor (m :: TrieType) [])
 |])

-- The successor of [] is the minimum (unless [] itself is the minimum)
$(makeFunc allTries ["findSuccessor","findMin","notMember","null"] [d|
   prop_findSuccessor1 findSuccessor findMin notMember null m =
      not (null m) && notMember [] m ==>
         findSuccessor (m :: TrieType) [] == findMin m
 |])

-- The minimum has no predecessor
$(makeFunc allTries ["findPredecessor","findMin","null"] [d|
   prop_findPredecessor2 findPredecessor findMin null m =
      not (null m) ==>
         isNothing.findPredecessor (m :: TrieType).getKey.fromJust.findMin $ m
 |])
-- The maximum has no successor
$(makeFunc allTries ["findSuccessor","findMax","null"] [d|
   prop_findSuccessor2 findSuccessor findMax null m =
      not (null m) ==>
         isNothing.findSuccessor (m :: TrieType).getKey.fromJust.findMax $ m
 |])

-- Splitting away the common prefix and adding it back should change nothing
$(makeFunc allTries ["addPrefix","splitPrefix"] [d|
   prop_prefixOps1 addPrefix splitPrefix m =
      uncurry addPrefix (splitPrefix m) == (m :: TrieType)
 |])

-- Looking up the common prefix and then adding it back should change nothing
$(makeFunc allTries ["addPrefix","splitPrefix","lookupPrefix"] [d|
   prop_prefixOps2 addPrefix splitPrefix lookupPrefix m =
      let (k,_) = splitPrefix (m :: TrieType)
       in addPrefix k (lookupPrefix k m) == m
   |])

tests = concat
   [ $(makeTests allTries "prop_size1")
   , $(makeTests allTries "prop_size2")
   , $(makeTests allTries "prop_member1")
   , $(makeTests mapsOnly "prop_lookup1")
   , $(makeTests mapsOnly "prop_lookupWithDefault1")
   , $(makeTests setsOnly "prop_isSubsetOf1")
   , $(makeTests setsOnly "prop_isProperSubsetOf1")
   , $(makeTests mapsOnly "prop_isSubmapOf1")
   , $(makeTests mapsOnly "prop_isProperSubmapOf1")
   , $(makeTests mapsOnly "prop_singleton1")
   , $(makeTests mapsOnly "prop_insert1")
   , $(makeTests allTries "prop_delete1")
   , $(makeTests allTries "prop_union1")
   , $(makeTests allTries "prop_union2")
   , $(makeTests allTries "prop_union3")
   , $(makeTests allTries "prop_difference1")
   , $(makeTests allTries "prop_difference2")
   , $(makeTests allTries "prop_difference3")
--   , $(makeTests allTries "prop_intersection1")
--   , $(makeTests allTries "prop_intersection2")
   , $(makeTests allTries "prop_splitMaxPredecessor")
   , $(makeTests allTries "prop_splitMinSuccessor")
   , $(makeTests allTries "prop_minView1")
   , $(makeTests allTries "prop_maxView1")
   , $(makeTests allTries "prop_findPredecessor1")
   , $(makeTests allTries "prop_findSuccessor1")
   , $(makeTests allTries "prop_findPredecessor2")
   , $(makeTests allTries "prop_findSuccessor2")
   , $(makeTests allTries "prop_prefixOps1")
   , $(makeTests allTries "prop_prefixOps2")
   ]
