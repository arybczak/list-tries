-- File created: 2009-01-06 12:59:53

{-# LANGUAGE CPP, TemplateHaskell #-}

module Tests.Properties (tests) where

import Language.Haskell.TH (Type(..))
import Test.Framework.Providers.QuickCheck (testProperty)

import Data.Trie.Base.Map (Map)

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

-- List of tests is at the bottom because it doesn't work at the top, not sure
-- whether it's because of the CPP or just a TH limitation

-- GHC's TH impl doesn't allow referring to functions in the same module—not
-- even constants. So we use CPP to get partial application instead; moving the
-- constants to another module doesn't make much sense.
#define SETS [SetModule "Data.Trie.Set.Eq"            \
             ,SetModule "Data.Trie.Set.Ord"           \
             ,SetModule "Data.Trie.Set.Enum"          \
             ,SetModule "Data.Trie.Patricia.Set.Eq"   \
             ,SetModule "Data.Trie.Patricia.Set.Ord"  \
             ,SetModule "Data.Trie.Patricia.Set.Enum" \
             ]
#define MAPS [MapModule "Data.Trie.Map.Eq"            \
             ,MapModule "Data.Trie.Map.Ord"           \
             ,MapModule "Data.Trie.Map.Enum"          \
             ,MapModule "Data.Trie.Patricia.Map.Eq"   \
             ,MapModule "Data.Trie.Patricia.Map.Ord"  \
             ,MapModule "Data.Trie.Patricia.Map.Enum" \
             ]
#define ALL (SETS ++ MAPS)

#define FUNC_MAPANDSET makeFunc ALL

#define FROMLIST_T \
   (ArrowT `AppT` (ListT `AppT` ConT ''ListElemType) `AppT` ConT ''TrieType)

#define TOLIST_T \
   (ArrowT `AppT` ConT ''TrieType `AppT` (ListT `AppT` ConT ''ListElemType))

$(FUNC_MAPANDSET [("fromList",Just FROMLIST_T),("size",Nothing)] [d|
   prop_size1 fromList size l_ = let l = map unArb l_
                                  in size (fromList l) <= length l
 |])

$(FUNC_MAPANDSET [("toList",Just TOLIST_T),("size",Nothing)] [d|
   prop_size2 toList size m = size m == length (toList m)
 |])

#define TEST_MAPANDSET makeTests ALL

tests = concat
   [ $(TEST_MAPANDSET "prop_size1" "size-1")
   , $(TEST_MAPANDSET "prop_size2" "size-2")
   ]
