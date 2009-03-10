-- File created: 2009-01-06 13:26:03

-- | A set of lists of elements that can be compared for equality, based on a
-- trie.
module Data.ListTrie.Set.Eq (TrieSet, module Data.ListTrie.Set) where

import Data.ListTrie.Base.Map   (AList)
import Data.ListTrie.Set hiding (TrieSet)
import qualified Data.ListTrie.Set as Base

type TrieSet = Base.TrieSet AList
