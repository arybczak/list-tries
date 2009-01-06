-- File created: 2009-01-06 13:51:25

-- A set of lists of elements that can be compared for equality as keys, based
-- on a Patricia trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.Trie.Patricia.Set.Eq (SET_EXPORTS) where

import Data.Trie.Base.Map            (AList)
import Data.Trie.Patricia.Set hiding (TrieSet)
import qualified Data.Trie.Patricia.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet AList
