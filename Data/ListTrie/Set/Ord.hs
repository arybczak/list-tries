-- File created: 2009-01-06 13:18:32

-- | A set of lists of elements that can be totally ordered, based on a trie.
module Data.ListTrie.Set.Ord (TrieSet, module Data.ListTrie.Set) where

import Data.Map                 (Map)
import Data.ListTrie.Set hiding (TrieSet)
import qualified Data.ListTrie.Set as Base

type TrieSet = Base.TrieSet Map
