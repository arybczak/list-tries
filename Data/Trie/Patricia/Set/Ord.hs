-- File created: 2009-01-06 13:50:00

-- A set of lists of elements that can be totally ordered, based on a Patricia
-- trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.Trie.Patricia.Set.Ord (SET_EXPORTS) where

import Data.Map                      (Map)
import Data.Trie.Patricia.Set hiding (TrieSet)
import qualified Data.Trie.Patricia.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet Map
