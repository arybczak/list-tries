-- File created: 2009-01-06 13:18:32

-- A set of lists of elements that can be totally ordered, based on a trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.Trie.Set.Ord (SET_EXPORTS) where

import Data.Map             (Map)
import Data.Trie.Set hiding (TrieSet)
import qualified Data.Trie.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet Map
