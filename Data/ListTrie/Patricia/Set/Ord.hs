-- File created: 2009-01-06 13:50:00

-- A set of lists of elements that can be totally ordered, based on a Patricia
-- trie.

module Data.ListTrie.Patricia.Set.Ord ( TrieSet
                                      , module Data.ListTrie.Patricia.Set
                                      ) where

import Data.Map                          (Map)
import Data.ListTrie.Patricia.Set hiding (TrieSet)
import qualified Data.ListTrie.Patricia.Set as Base

type TrieSet = Base.TrieSet Map
