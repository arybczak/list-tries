-- File created: 2009-01-06 13:49:30

-- A map with lists of elements that can be compared for equality as keys,
-- based on a Patricia trie.

module Data.ListTrie.Patricia.Map.Eq ( TrieMap
                                     , module Data.ListTrie.Patricia.Map
                                     ) where

import Data.ListTrie.Base.Map            (AList)
import Data.ListTrie.Patricia.Map hiding (TrieMap)
import qualified Data.ListTrie.Patricia.Map as Base

type TrieMap = Base.TrieMap AList
