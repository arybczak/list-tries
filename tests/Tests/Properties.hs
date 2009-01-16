-- File created: 2009-01-06 12:59:53

{-# LANGUAGE TemplateHaskell #-}

module Tests.Properties (tests) where

import Data.Maybe                          (fromJust)
import Test.Framework.Providers.QuickCheck (testProperty)

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
-- like a TH limitation

$(makeFunc allTries ["fromList","size"] [d|
   prop_size1 fromList size l_ = let l = map unArb l_
                                  in size (fromList l :: TrieType) <= length l_
 |])

$(makeFunc allTries ["toList","size"] [d|
   prop_size2 toList size m = size (m :: TrieType) == length (toList m)
 |])

$(makeFunc allTries ["fromList","member"] [d|
  -- using flip avoids GHC #2956
  prop_member1 fromList member l_ =
     let l = map unArb l_
         m = fromList l :: TrieType
      in all (flip member m . getKey) (l :: [ListElemType])
 |])

$(makeFunc mapsOnly ["fromList","lookup"] [d|
   prop_lookup1 fromList lookup l_ =
      let l = map unArb l_
          m = fromList l :: TrieType
       in all (\(k,v) -> fromJust (lookup k m) == v) l
 |])

$(makeFunc mapsOnly ["lookupWithDefault","empty"] [d|
   prop_lookupWithDefault1 lookupWithDefault empty k v =
      lookupWithDefault v (unArb k) (empty :: TrieType) == v
 |])

$(makeFunc setsOnly ["isSubsetOf"] [d|
   prop_isSubsetOf1 isSubsetOf m = isSubsetOf m (m :: TrieType)
 |])
$(makeFunc setsOnly ["isProperSubsetOf"] [d|
   prop_isProperSubsetOf1 isProperSubsetOf m =
      not (isProperSubsetOf m (m :: TrieType))
 |])
$(makeFunc mapsOnly ["isSubmapOf"] [d|
   prop_isSubmapOf1 isSubmapOf m = isSubmapOf m (m :: TrieType)
 |])
$(makeFunc mapsOnly ["isProperSubmapOf"] [d|
   prop_isProperSubmapOf1 isProperSubmapOf m =
      not (isProperSubmapOf m (m :: TrieType))
 |])

$(makeFunc mapsOnly ["lookup","singleton"] [d|
   prop_singleton1 lookup singleton k_ v =
      let k = unArb k_
       in (fromJust . lookup k) (singleton k v :: TrieType) == v
 |])

$(makeFunc mapsOnly ["lookup","insert"] [d|
   prop_insert1 lookup insert m k_ v =
      let k = unArb k_
       in (fromJust . lookup k . insert k v) (m :: TrieType) == v
 |])

$(makeFunc allTries ["notMember","delete"] [d|
   prop_delete1 notMember delete k_ m =
      let k = unArb k_
       in notMember k . delete k $ (m :: TrieType)
 |])

$(makeFunc allTries ["union","member","toList"] [d|
   prop_union1 union member toList m n =
      let u = union m (n :: TrieType)
       in all (flip member u . getKey) (toList m ++ toList n)
 |])

$(makeFunc allTries ["union","empty"] [d|
   prop_union2 union empty m = union m empty == (m :: TrieType)
 |])
$(makeFunc allTries ["union","empty"] [d|
   prop_union3 union empty m = union empty m == (m :: TrieType)
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
   ]
