-- File created: 2009-01-06 13:26:03

-- A set of lists of elements that can be compared for equality, based on a
-- trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.Trie.Set.Eq (SET_EXPORTS) where

import Data.Trie.Base.Map   (AList)
import Data.Trie.Set hiding (TrieSet)
import qualified Data.Trie.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet AList
