-- File created: 2009-01-06 13:48:52

-- A map with lists of elements that can be totally ordered as keys, based on a
-- Patricia trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.ListTrie.Patricia.Map.Ord (MAP_EXPORTS) where

import Data.Map                      (Map)
import Data.ListTrie.Patricia.Map hiding (TrieMap)
import qualified Data.ListTrie.Patricia.Map as Base
import Prelude hiding (filter, foldr, lookup, map, null)

type TrieMap = Base.TrieMap Map
