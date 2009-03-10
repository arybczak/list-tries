-- File created: 2009-01-06 13:18:32

-- A map with lists of elements that can be totally ordered as keys, based on
-- a trie.

module Data.ListTrie.Map.Ord (TrieMap, module Data.ListTrie.Map) where

import Data.Map                 (Map)
import Data.ListTrie.Map hiding (TrieMap)
import qualified Data.ListTrie.Map as Base

type TrieMap = Base.TrieMap Map
