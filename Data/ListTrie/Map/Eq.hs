-- File created: 2009-01-06 13:26:25

-- | A map from lists of elements that can be compared for equality to
-- arbitrary values, based on a trie.
module Data.ListTrie.Map.Eq (TrieMap, module Data.ListTrie.Map) where

import Data.ListTrie.Base.Map   (AList)
import Data.ListTrie.Map hiding (TrieMap)
import qualified Data.ListTrie.Map as Base

type TrieMap = Base.TrieMap AList
