-- File created: 2008-10-18 21:33:40

-- | A set of lists of enumerable elements, based on a trie.
--
-- Note that those operations which require an ordering, such as 'toAscList',
-- do not compare the elements themselves, but rather their Int representation
-- after 'fromEnum'.
module Data.ListTrie.Set.Enum (TrieSet, module Data.ListTrie.Set) where

import Data.ListTrie.Base.Map   (WrappedIntMap)
import Data.ListTrie.Set hiding (TrieSet)
import qualified Data.ListTrie.Set as Base

type TrieSet = Base.TrieSet WrappedIntMap
