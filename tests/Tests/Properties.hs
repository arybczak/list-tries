-- File created: 2009-01-06 12:59:53

{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}

module Tests.Properties (tests) where

import Control.Arrow    ((&&&), first)
import Data.Binary      (encode,decode)
import Data.Foldable    (foldMap)
import Data.Function    (on)
import Data.List        (nubBy)
import Data.Maybe       (fromJust, isNothing)
import Data.Monoid      (mappend, mempty)
import Data.Traversable (fmapDefault, foldMapDefault)

import Test.Framework                       (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      ((==>))

import qualified Data.ListTrie.Base.Map as Map
import qualified Data.ListTrie.Set.Eq
import qualified Data.ListTrie.Set.Ord
import qualified Data.ListTrie.Set.Enum
import qualified Data.ListTrie.Map.Eq
import qualified Data.ListTrie.Map.Ord
import qualified Data.ListTrie.Map.Enum
import qualified Data.ListTrie.Patricia.Set.Eq
import qualified Data.ListTrie.Patricia.Set.Ord
import qualified Data.ListTrie.Patricia.Set.Enum
import qualified Data.ListTrie.Patricia.Map.Eq
import qualified Data.ListTrie.Patricia.Map.Ord
import qualified Data.ListTrie.Patricia.Map.Enum

import Tests.Base
import Tests.TH

keyNub = nubBy ((==) `on` getKey)

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
-- Of course the list needs to be nubbed; and in case of duplicates the last
-- value is preferred, so reversed
$(makeFunc mapsOnly ["fromList","lookup"] [d|
   prop_lookup1 fromList lookup l_ =
      let l = map unArb l_
          m = fromList l :: TrieType
       in all (\(k,v) -> fromJust (lookup k m) == v) (keyNub . reverse $ l)
 |])

-- lookupWithDefault should return the default if the key is not a member of
-- the map
$(makeFunc mapsOnly ["lookupWithDefault","notMember"] [d|
   prop_lookupWithDefault1 lookupWithDefault notMember m k_ v =
      let k = unArb k_
       in notMember k (m :: TrieType) ==> lookupWithDefault v k m == v
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

$(makeFunc setsOnly ["isSubsetOf", "isProperSubsetOf"] [d|
   prop_isProperSubsetOf2 isSubsetOf isProperSubsetOf m n =
      if isProperSubsetOf m n
         then isSubsetOf m (n :: TrieType)
         else True
 |])
$(makeFunc mapsOnly ["isSubmapOf", "isProperSubmapOf"] [d|
   prop_isProperSubmapOf2 isSubmapOf isProperSubmapOf m n =
      if isProperSubmapOf m n
         then isSubmapOf (m :: TrieType) (n :: TrieType)
         else True
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

-- Inserting into empty is the same thing as a singleton
$(makeFunc setsOnly ["empty","insert","singleton"] [d|
   prop_insert2_s empty insert singleton k_ =
      let k = unArb k_
       in insert k empty == (singleton k :: TrieType)
 |])
$(makeFunc mapsOnly ["empty","insert","singleton"] [d|
   prop_insert2_m empty insert singleton k_ v =
      let k = unArb k_
       in insert k v empty == (singleton k v :: TrieType)
 |])

-- Deleting a key means it should no longer be in the set
$(makeFunc allTries ["notMember","delete"] [d|
   prop_delete1 notMember delete k_ m =
      let k = unArb k_
       in notMember k . delete k $ (m :: TrieType)
 |])

-- Altering a value within a map and then looking it up is the same as first
-- looking it up and then altering it: lookup k (alter f k m) == f (lookup k m)
$(makeFunc mapsOnly ["alter","lookup"] [d|
   prop_alter1 alter lookup k_ m =
      let k = unArb k_
       in lookup k (alter (const Nothing) k m :: TrieType) == Nothing
 |])
$(makeFunc mapsOnly ["alter","lookup"] [d|
   prop_alter2 alter lookup k_ m =
      let k = unArb k_
       in lookup k (alter (const (Just 2)) k m :: TrieType) == Just 2
 |])
$(makeFunc mapsOnly ["alter","lookup"] [d|
   prop_alter3 alter lookup k_ m =
      let k = unArb k_
       in lookup k (alter (fmap ((+) 1)) k m :: TrieType)
          == fmap ((+) 1) (lookup k m)
 |])

-- updateLookup (const Nothing) is equivalent to lookup &&& delete
--
-- Run on head.toList as well to make sure that the key's actually in there
--
-- Avoids #2956
$(makeFunc mapsOnly ["updateLookup","lookup","delete","toList","null"] [d|
   prop_updateLookup1 updateLookup lookup delete toList null k_ m =
      check (unArb k_) && (null m || check (getKey.head.toList $ m))
    where
      check k =
         updateLookup (const Nothing) k (m :: TrieType)
            == (lookup k &&& delete k) m
 |])
-- updateLookup (Just . f) is equivalent to lookup &&& adjust f
--
-- Run on head.toList as well to make sure that the key's actually in there
--
-- Avoids #2956
$(makeFunc mapsOnly ["updateLookup","lookup","adjust","toList","null"] [d|
   prop_updateLookup2 updateLookup lookup adjust toList null k_ m =
      check (unArb k_) && (null m || check (getKey.head.toList $ m))
    where
      check k =
         updateLookup (Just . (+) 1) k (m :: TrieType)
            == (lookup k &&& adjust ((+) 1) k) m
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
   prop_intersection2 intersection null empty m =
      null $ intersection empty (m :: TrieType)
 |])

-- De Morgan's laws: union and intersection interchange under complementation
$(makeFunc allTries ["union","difference","intersection"] [d|
   prop_deMorgan1 union difference intersection a b c =
      complement (intersection a b) == union (complement a) (complement b)
    where
      complement :: TrieType -> TrieType
      complement = difference c
 |])
$(makeFunc allTries ["union","difference","intersection"] [d|
   prop_deMorgan2 union difference intersection a b c =
      complement (union a b) == intersection (complement a) (complement b)
    where
      complement :: TrieType -> TrieType
      complement = difference c
 |])

-- Partition is equivalent to two filters
--
-- #2956 avoidance
$(makeFunc mapsOnly ["filter","partition"] [d|
   prop_partition1 filter partition m =
      let (a,b) = partition p (m :: TrieType)
       in a == filter p m && b == filter (not.p) m
    where
      p = (==) 0 . flip mod 2
 |])

-- mapMaybe can function as a filter and mapEither as a partition
--
-- #2956 avoidance
$(makeFunc mapsOnly ["mapMaybe","filter"] [d|
   prop_mapMaybe1 mapMaybe filter m =
      mapMaybe (\x -> if p x then Just x else Nothing) m
         == filter p (m :: TrieType)
    where
      p = (==) 0 . flip mod 2
 |])
$(makeFunc mapsOnly ["mapEither","partition"] [d|
   prop_mapEither1 mapEither partition m =
      mapEither (\x -> if p x then Left x else Right x) m
         == partition p (m :: TrieType)
    where
      p = (==) 0 . flip mod 2
 |])

-- The maximum of the left side of a split about k is the predecessor of k
$(makeFunc allTries ["split","findMax","findPredecessor"] [d|
   prop_splitMaxPredecessor split findMax findPredecessor m k_ =
      let k = unArb k_
          (a,_) = split k (m :: TrieType)
       in findMax a == findPredecessor k m
 |])
-- The minimum of the right side of a split about k is the successor of k
$(makeFunc allTries ["split","findMin","findSuccessor"] [d|
   prop_splitMinSuccessor split findMin findSuccessor m k_ =
      let k = unArb k_
          (_,b) = split k (m :: TrieType)
       in findMin b == findSuccessor k m
 |])

-- The centre of a splitLookup/Member is the result of a lookup/member
$(makeFunc mapsOnly ["splitLookup","lookup"] [d|
   prop_splitLookup1 splitLookup lookup m k_ =
      let k = unArb k_
          (_,v,_) = splitLookup k (m :: TrieType)
       in v == lookup k m
 |])
$(makeFunc setsOnly ["splitMember","member"] [d|
   prop_splitMember1 splitMember member m k_ =
      let k = unArb k_
          (_,v,_) = splitMember k (m :: TrieType)
       in v == member k m
 |])

-- toList (map trie) should be equivalent to map (toList trie)
-- modulo ordering, hence toAscList
--
-- #2956 avoidance
$(makeFunc setsOnly ["map","toAscList"] [d|
   prop_mapKeys1_s map toAscList m =
      toAscList (map f (m :: TrieType)) ==
         keyNub (Prelude.map f $ toAscList m)
    where f = (:) 'x'
 |])
$(makeFunc mapsOnly ["mapKeys","toAscList"] [d|
   prop_mapKeys1_m mapKeys toAscList m =
      toAscList (mapKeys f (m :: TrieType)) ==
         keyNub (Prelude.map (first f) $ toAscList m)
    where f = (:) 'x'
 |])
$(makeFunc setsOnly ["mapIn","toAscList"] [d|
   prop_mapInKeys1_s mapIn toAscList m =
      toAscList (mapIn f (m :: TrieType)) ==
         keyNub (map (map f) $ toAscList m)
    where f = toEnum . (+) 1 . fromEnum :: Char -> Char
 |])
$(makeFunc mapsOnly ["mapInKeys","toAscList"] [d|
   prop_mapInKeys1_m mapInKeys toAscList m =
      toAscList (mapInKeys f (m :: TrieType)) ==
         keyNub (map (first (map f)) $ toAscList m)
    where f = toEnum . (+) 1 . fromEnum :: Char -> Char
 |])

-- toAscList = reverse . toDescList
$(makeFunc allTries ["toAscList","toDescList"] [d|
   prop_ascDesc1 toAscList toDescList m =
      toAscList (m :: TrieType) == reverse (toDescList m)
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
      isNothing (findPredecessor [] (m :: TrieType))
 |])

-- The successor of [] is the minimum (unless [] itself is the minimum)
$(makeFunc allTries ["findSuccessor","findMin","notMember","null"] [d|
   prop_findSuccessor1 findSuccessor findMin notMember null m =
      not (null m) && notMember [] m ==>
         findSuccessor [] (m :: TrieType) == findMin m
 |])

-- The minimum has no predecessor
$(makeFunc allTries ["findPredecessor","findMin","null"] [d|
   prop_findPredecessor2 findPredecessor findMin null m =
      not (null m) ==>
         isNothing $ findPredecessor (getKey.fromJust.findMin $ m)
                                     (m :: TrieType)
 |])
-- The maximum has no successor
$(makeFunc allTries ["findSuccessor","findMax","null"] [d|
   prop_findSuccessor2 findSuccessor findMax null m =
      not (null m) ==>
         isNothing $ findSuccessor (getKey.fromJust.findMax $ m)
                                   (m :: TrieType)
 |])

-- Splitting away the common prefix and adding it and its value back
-- should change nothing
$(makeFunc setsOnly ["addPrefix","splitPrefix","insert"] [d|
   prop_prefixOps1_s addPrefix splitPrefix insert m =
      let (k,b,t) = splitPrefix (m :: TrieType)
       in (if b then insert k else id) (addPrefix k t) == m
 |])
$(makeFunc mapsOnly ["addPrefix","splitPrefix","insert"] [d|
   prop_prefixOps1_m addPrefix splitPrefix insert m =
      let (k,mv,t) = splitPrefix (m :: TrieType)
       in (case mv of Just v -> insert k v; _ -> id) (addPrefix k t) == m
 |])

-- Looking up the common prefix and then adding it back should change nothing
$(makeFunc allTries ["addPrefix","splitPrefix","deletePrefix"] [d|
   prop_prefixOps2 addPrefix splitPrefix deletePrefix m =
      let (k,_,_) = splitPrefix (m :: TrieType)
       in addPrefix k (deletePrefix k m) == m
 |])

-- Splitting away the prefix shouldn't affect the children
$(makeFunc allTries ["splitPrefix","children"] [d|
   prop_prefixOps3 splitPrefix children t =
      let (_,_,t') = splitPrefix (t :: TrieType)
       in children t == children t'
 |])

-- Adding the common prefix and value to the union of the children should give
-- back the original trie
$(makeFunc setsOnly ["addPrefix","splitPrefix","children","unions","insert"]
 [d|
   prop_prefixOps4_s addPrefix splitPrefix children unions insert t =
      let (k,b,_) = splitPrefix (t :: TrieType)
       in t == ((if b then insert k else id) . addPrefix k .
                   unions $ map (uncurry $ addPrefix . return)
                                (Map.toList $ children t))
 |])
$(makeFunc mapsOnly ["addPrefix","splitPrefix","children","unions","insert"]
 [d|
   prop_prefixOps4_m addPrefix splitPrefix children unions insert t =
      let (k,mv,_) = splitPrefix (t :: TrieType)
       in t == ((case mv of Just v -> insert k v; _ -> id) . addPrefix k .
                   unions $ map (uncurry $ addPrefix . return)
                                (Map.toList $ children t))
 |])

-- Deleting an added prefix should change nothing
$(makeFunc allTries ["addPrefix","deletePrefix"] [d|
   prop_prefixOps5 addPrefix deletePrefix t k_ =
      let k = unArb k_
       in deletePrefix k (addPrefix k t) == (t :: TrieType)
 |])


-- The monoid laws: associativity, left identity, right identity
$(makeFunc allTries [] [d|
   prop_monoidLaw1 x y z =
      mappend x (mappend y z) == mappend (mappend x y) (z :: TrieType)
 |])
$(makeFunc allTries [] [d|
   prop_monoidLaw2 x = mappend mempty x == (x :: TrieType)
 |])
$(makeFunc allTries [] [d|
   prop_monoidLaw3 x = mappend x mempty == (x :: TrieType)
 |])

-- The functor laws: fmap id == id, fmap (f.g) == (fmap f . fmap g)
$(makeFunc mapsOnly [] [d|
   prop_functorLaw1 x = fmap id x == (x :: TrieType)
 |])
$(makeFunc mapsOnly [] [d|
   prop_functorLaw2 x = fmap (f.g) x == (fmap f . fmap g) (x :: TrieType)
    where
      f = (+) 10; g = (*) 2;
 |])

-- The Traversable laws: fmap == fmapDefault, foldMap == foldMapDefault
-- Both avoid #2956 again
$(makeFunc mapsOnly [] [d|
   prop_traversableLaw1 x =
      fmap ((+) 1) x == fmapDefault ((+) 1) (x :: TrieType)
 |])
$(makeFunc mapsOnly [] [d|
   prop_traversableLaw2 x =
      foldMap (flip (:) []) x == foldMapDefault (flip (:) []) (x :: TrieType)
 |])

-- (read.show) is the identity function
$(makeFunc allTries [] [d|
   prop_showRead1 x = (read.show) (x :: TrieType) == x
 |])

-- (compare `on` toAscList) should be equivalent to compare
$(makeFunc allTries ["toAscList"] [d|
   prop_ord1 toAscList x y =
      compare x (y :: TrieType) == compare (toAscList x) (toAscList y)
 |])

-- serialization followed by deserialization should not alter the trie
$(makeFunc allTries [] [d|
   prop_serializeDeserialize x = (decode.encode) (x :: TrieType) == x
 |])

tests = testGroup "QuickCheck properties"
   [ $(makeProps allTries "prop_size1")
   , $(makeProps allTries "prop_size2")
   , $(makeProps allTries "prop_member1")
   , $(makeProps mapsOnly "prop_lookup1")
   , $(makeProps mapsOnly "prop_lookupWithDefault1")
   , $(makeProps setsOnly "prop_isSubsetOf1")
   , $(makeProps setsOnly "prop_isProperSubsetOf1")
   , $(makeProps mapsOnly "prop_isSubmapOf1")
   , $(makeProps mapsOnly "prop_isProperSubmapOf1")
   , $(makeProps setsOnly "prop_isProperSubsetOf2")
   , $(makeProps mapsOnly "prop_isProperSubmapOf2")
   , $(makeProps mapsOnly "prop_singleton1")
   , $(makeProps mapsOnly "prop_insert1")
   , $(makeProps setsOnly "prop_insert2_s")
   , $(makeProps mapsOnly "prop_insert2_m")
   , $(makeProps allTries "prop_delete1")
   , $(makeProps mapsOnly "prop_alter1")
   , $(makeProps mapsOnly "prop_alter2")
   , $(makeProps mapsOnly "prop_alter3")
   , $(makeProps mapsOnly "prop_updateLookup1")
   , $(makeProps mapsOnly "prop_updateLookup2")
   , $(makeProps allTries "prop_union1")
   , $(makeProps allTries "prop_union2")
   , $(makeProps allTries "prop_union3")
   , $(makeProps allTries "prop_difference1")
   , $(makeProps allTries "prop_difference2")
   , $(makeProps allTries "prop_difference3")
   , $(makeProps allTries "prop_intersection1")
   , $(makeProps allTries "prop_intersection2")
   , $(makeProps allTries "prop_deMorgan1")
   , $(makeProps allTries "prop_deMorgan2")
   , $(makeProps mapsOnly "prop_partition1")
   , $(makeProps mapsOnly "prop_mapMaybe1")
   , $(makeProps mapsOnly "prop_mapEither1")
   , $(makeProps allTries "prop_splitMaxPredecessor")
   , $(makeProps allTries "prop_splitMinSuccessor")
   , $(makeProps mapsOnly "prop_splitLookup1")
   , $(makeProps setsOnly "prop_splitMember1")
   , $(makeProps setsOnly "prop_mapKeys1_s")
   , $(makeProps mapsOnly "prop_mapKeys1_m")
   , $(makeProps setsOnly "prop_mapInKeys1_s")
   , $(makeProps mapsOnly "prop_mapInKeys1_m")
   , $(makeProps allTries "prop_ascDesc1")
   , $(makeProps allTries "prop_minView1")
   , $(makeProps allTries "prop_maxView1")
   , $(makeProps allTries "prop_findPredecessor1")
   , $(makeProps allTries "prop_findSuccessor1")
   , $(makeProps allTries "prop_findPredecessor2")
   , $(makeProps allTries "prop_findSuccessor2")
   , $(makeProps setsOnly "prop_prefixOps1_s")
   , $(makeProps mapsOnly "prop_prefixOps1_m")
   , $(makeProps allTries "prop_prefixOps2")
   , $(makeProps allTries "prop_prefixOps3")
   , $(makeProps setsOnly "prop_prefixOps4_s")
   , $(makeProps mapsOnly "prop_prefixOps4_m")
   , $(makeProps allTries "prop_prefixOps5")
   , $(makeProps allTries "prop_monoidLaw1")
   , $(makeProps allTries "prop_monoidLaw2")
   , $(makeProps allTries "prop_monoidLaw3")
   , $(makeProps mapsOnly "prop_functorLaw1")
   , $(makeProps mapsOnly "prop_functorLaw2")
   , $(makeProps mapsOnly "prop_traversableLaw1")
   , $(makeProps mapsOnly "prop_traversableLaw2")
   , $(makeProps allTries "prop_showRead1")
   , $(makeProps allTries "prop_ord1")
   , $(makeProps allTries "prop_serializeDeserialize")
   ]
