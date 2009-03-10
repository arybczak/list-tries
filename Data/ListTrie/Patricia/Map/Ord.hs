-- File created: 2009-01-06 13:48:52

-- A map with lists of elements that can be totally ordered as keys, based on a
-- Patricia trie.

module Data.ListTrie.Patricia.Map.Ord ( TrieMap
                                      , module Data.ListTrie.Patricia.Map
                                      ) where

import Data.Map                          (Map)
import Data.ListTrie.Patricia.Map hiding (TrieMap)
import qualified Data.ListTrie.Patricia.Map as Base

type TrieMap = Base.TrieMap Map
