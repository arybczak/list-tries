-- File created: 2009-01-06 12:59:53

{-# LANGUAGE TemplateHaskell #-}

module Tests.Properties (tests) where

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

$(makeFunc allTries [("fromList",Just fromList_t),("size",Nothing)] [d|
   prop_size1 fromList size l_ = let l = map unArb l_
                                  in size (fromList l) <= length l
 |])

$(makeFunc allTries [("toList",Just toList_t),("size",Nothing)] [d|
   prop_size2 toList size m = size m == length (toList m)
 |])

$(makeFunc allTries [("fromList",Just fromList_t),("member",Nothing)] [d|
	-- using flip avoids GHC #2956
	prop_member1 fromList member l_ = let l = map unArb l_
	                                      m = fromList l
	                                   in all (flip member m . getKey) l
 |])

tests = concat
   [ $(makeTests allTries "prop_size1"   "size-1")
   , $(makeTests allTries "prop_size2"   "size-2")
   , $(makeTests allTries "prop_member1" "member-1")
   ]
