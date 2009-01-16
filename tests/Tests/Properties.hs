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

$(makeFunc allTries [fromList_t,("size",Nothing)] [d|
   prop_size1 fromList size l_ = let l = map unArb l_
                                  in size (fromList l) <= length l_
 |])

$(makeFunc allTries [toList_t,("size",Nothing)] [d|
   prop_size2 toList size m = size m == length (toList m)
 |])

$(makeFunc allTries [fromList_t,("member",Nothing)] [d|
  -- using flip avoids GHC #2956
  prop_member1 fromList member l_ =
     let l = map unArb l_
         m = fromList l
      in all (flip member m . getKey) l_
 |])

$(makeFunc mapsOnly [fromList_t,("lookup",Nothing)] [d|
   prop_lookup1 fromList lookup l_ =
      let l = map unArb l_
          m = fromList l
       in all (\(k,v) -> fromJust (lookup k m) == v) l
 |])

$(makeFunc mapsOnly [("lookupWithDefault",Nothing),empty_t] [d|
   prop_lookupWithDefault1 lookupWithDefault empty k v =
      lookupWithDefault v (unArb k) empty == v
 |])

$(makeFunc setsOnly [("isSubsetOf",Nothing)] [d|
   prop_isSubsetOf1 isSubsetOf m = isSubsetOf m (m :: TrieType)
 |])
$(makeFunc setsOnly [("isProperSubsetOf",Nothing)] [d|
   prop_isProperSubsetOf1 isProperSubsetOf m =
      not (isProperSubsetOf m (m :: TrieType))
 |])
$(makeFunc mapsOnly [("isSubmapOf",Nothing)] [d|
   prop_isSubmapOf1 isSubmapOf m = isSubmapOf m (m :: TrieType)
 |])
$(makeFunc mapsOnly [("isProperSubmapOf",Nothing)] [d|
   prop_isProperSubmapOf1 isProperSubmapOf m =
      not (isProperSubmapOf m (m :: TrieType))
 |])

tests = concat
   [ $(makeTests allTries "prop_size1"              "size-1")
   , $(makeTests allTries "prop_size2"              "size-2")
   , $(makeTests allTries "prop_member1"            "member-1")
   , $(makeTests mapsOnly "prop_lookup1"            "lookup-1")
   , $(makeTests mapsOnly "prop_lookupWithDefault1" "lookupWithDefault-1")
   , $(makeTests setsOnly "prop_isSubsetOf1"        "isSubsetOf-1")
   , $(makeTests setsOnly "prop_isProperSubsetOf1"  "isProperSubsetOf-1")
   , $(makeTests mapsOnly "prop_isSubmapOf1"        "isSubmapOf-1")
   , $(makeTests mapsOnly "prop_isProperSubmapOf1"  "isProperSubmapOf-1")
   ]
