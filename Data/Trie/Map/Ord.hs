-- File created: 2009-01-06 13:18:32

-- A map with lists of elements that can be totally ordered as keys, based on
-- a trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.Trie.Map.Ord (MAP_EXPORTS) where

import Data.Map             (Map)
import Data.Trie.Map hiding (TrieMap)
import qualified Data.Trie.Map as Base
import Prelude hiding (filter, foldr, lookup, map, null)

type TrieMap = Base.TrieMap Map
