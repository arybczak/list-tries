-- File created: 2009-01-06 13:47:08

-- A map with lists of enumerable elements as keys, based on a trie.
--
-- Note that those operations which require an ordering, such as 'toAscList',
-- do not compare the elements themselves, but rather their Int representation
-- after 'fromEnum'.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.ListTrie.Map.Enum (MAP_EXPORTS) where

import Data.ListTrie.Base.Map   (WrappedIntMap)
import Data.ListTrie.Map hiding (TrieMap)
import qualified Data.ListTrie.Map as Base
import Prelude hiding (filter, foldr, lookup, map, null)

type TrieMap = Base.TrieMap WrappedIntMap
