-- File created: 2009-01-16 18:54:26

{-# LANGUAGE TemplateHaskell #-}

module Tests.Cases (tests) where

import Test.Framework                      (testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)

import qualified Data.Trie.Set.Eq
import qualified Data.Trie.Set.Ord
import qualified Data.Trie.Set.Enum
import qualified Data.Trie.Map.Eq
import qualified Data.Trie.Map.Ord
import qualified Data.Trie.Map.Enum
import qualified Data.Trie.Patricia.Set.Eq
import qualified Data.Trie.Patricia.Set.Ord
import qualified Data.Trie.Patricia.Set.Enum
import qualified Data.Trie.Patricia.Map.Eq
import qualified Data.Trie.Patricia.Map.Ord
import qualified Data.Trie.Patricia.Map.Enum

import Tests.TH

$(makeFunc allTries ["null","empty"] [d|
   nullEmpty null empty = null (empty :: TrieType)
 |])

tests =
   [ $(makeCases allTries "nullEmpty")
   ]
