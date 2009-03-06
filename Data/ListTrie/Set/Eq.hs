-- File created: 2009-01-06 13:26:03

-- A set of lists of elements that can be compared for equality, based on a
-- trie.

{-# LANGUAGE CPP #-}

#include "exports.h"

module Data.ListTrie.Set.Eq (SET_EXPORTS) where

import Data.ListTrie.Base.Map   (AList)
import Data.ListTrie.Set hiding (TrieSet)
import qualified Data.ListTrie.Set as Base
import Prelude hiding (filter, foldr, map, null)

type TrieSet = Base.TrieSet AList
