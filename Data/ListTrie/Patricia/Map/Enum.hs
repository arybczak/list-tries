-- File created: 2008-12-29 12:42:12

-- | A map from lists of enumerable elements to arbitrary values, based on a
-- Patricia trie.
--
-- Note that those operations which require an ordering, such as 'toAscList',
-- do not compare the elements themselves, but rather their 'Int'
-- representation after 'fromEnum'.
module Data.ListTrie.Patricia.Map.Enum ( TrieMap
                                       , module Data.ListTrie.Patricia.Map
                                       ) where

import Data.ListTrie.Base.Map            (WrappedIntMap)
import Data.ListTrie.Patricia.Map hiding (TrieMap)
import qualified Data.ListTrie.Patricia.Map as Base

type TrieMap = Base.TrieMap WrappedIntMap
