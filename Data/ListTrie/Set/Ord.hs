-- File created: 2009-01-06 13:18:32

-- A set of lists of elements that can be totally ordered, based on a trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.ListTrie.Set.Ord (SET_EXPORTS) where

import Data.Map             (Map)
import Data.ListTrie.Set hiding (TrieSet)
import qualified Data.ListTrie.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet Map
