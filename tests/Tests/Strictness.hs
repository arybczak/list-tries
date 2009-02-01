-- File created: 2009-01-06 13:08:00

{-# LANGUAGE CPP, TemplateHaskell #-}

module Tests.Strictness (tests) where

import Test.ChasingBottoms.IsBottom   (isBottom)
import Test.Framework                 (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assert)

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

import Tests.Base
import Tests.TH

-- size doesn't evaluate the values but it does traverse the whole trie
-- returning a single result, so it works well for checking whether there are
-- any bottoms in the trie
#define IS_LAZY   (not.isBottom.size)
#define IS_STRICT (    isBottom.size)

-- insertWith' should apply the combining function strictly, insertWith should
-- not. We use a singleton of [] to make sure that the combining function is
-- called.
$(makeFunc mapsOnly ["size","singleton","insertWith"] [d|
   insertWith size singleton insertWith =
      IS_LAZY   . insertWith  undefined [] undefined $
         (singleton [] 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'1 size singleton insertWith' =
      IS_STRICT . insertWith' undefined [] undefined $
         (singleton [] 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'2 size singleton insertWith' =
      IS_STRICT . insertWith' (+)       [] undefined $
         (singleton [] 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'3 size singleton insertWith' =
      IS_STRICT . insertWith' undefined [] 0 $
         (singleton [] 0 :: TrieType)
 |])

-- As above, but for adjust' and adjust.
$(makeFunc mapsOnly ["size","singleton","adjust"] [d|
   adjust size singleton adjust =
      IS_LAZY   . adjust  undefined [] $ (singleton [] 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","adjust'"] [d|
   adjust' size singleton adjust' =
      IS_STRICT . adjust' undefined [] $ (singleton [] 0 :: TrieType)
 |])

-- As above, but for alter and alter'.
--
-- Need to use more sophisticated testing here because now the value itself is
-- ⊥, including whether it's Just or not; size wants that info.
--
-- And there's also the following facts:
--   - Patricia's alter is lazy for only one case: the key to be altered is the
--     prefix of more than one key in the trie.
--   - Non-Patricia's alter is lazy for only one case: the key to be altered is
--     the prefix of at least one key in the trie.
--
-- So we have to be careful about the case we test.
$(makeFunc mapsOnly ["member","fromList","alter"] [d|
   alter member fromList alter =
      not.isBottom.member "foob" . alter undefined "foo" $
         (fromList [("foo",1),("foob",2),("fooz",3)] :: TrieType)
 |])
$(makeFunc mapsOnly ["member","fromList","alter'"] [d|
   alter' member fromList alter' =
      isBottom.member "foob" . alter' undefined "foo" $
         (fromList [("foo",1),("foob",2),("fooz",3)] :: TrieType)
 |])

-- As above, but for the union family.
$(makeFunc mapsOnly ["size","singleton","union"] [d|
   union size singleton union =
      IS_LAZY   $ union  (singleton "foo" undefined :: TrieType)
                         (singleton "foo" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","union'"] [d|
   union' size singleton union' =
      IS_STRICT $ union' (singleton "foo" undefined :: TrieType)
                         (singleton "foo" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","unionWith"] [d|
   unionWith size singleton unionWith =
      IS_LAZY   $ unionWith undefined  (singleton "foo" 1 :: TrieType)
                                       (singleton "foo" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","unionWith'"] [d|
   unionWith' size singleton unionWith' =
      IS_STRICT $ unionWith' undefined (singleton "foo" 1 :: TrieType)
                                       (singleton "foo" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","unionWithKey"] [d|
   unionWithKey size singleton unionWithKey =
      IS_LAZY   $ unionWithKey undefined  (singleton "foo" 1 :: TrieType)
                                          (singleton "foo" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","unionWithKey'"] [d|
   unionWithKey' size singleton unionWithKey' =
      IS_STRICT $ unionWithKey' undefined (singleton "foo" 1 :: TrieType)
                                          (singleton "foo" 1)
 |])

-- As above, but for the unions family.
$(makeFunc mapsOnly ["size","singleton","unions"] [d|
   unions size singleton unions =
      IS_LAZY   $ unions  [singleton "foo" undefined :: TrieType
                          ,singleton "foo" 1
                          ]
 |])
$(makeFunc mapsOnly ["size","singleton","unions'"] [d|
   unions' size singleton unions' =
      IS_STRICT $ unions' [singleton "foo" undefined :: TrieType
                          ,singleton "foo" 1
                          ]
 |])
$(makeFunc mapsOnly ["size","singleton","unionsWith"] [d|
   unionsWith size singleton unionsWith =
      IS_LAZY   $ unionsWith undefined  [singleton "foo" 1 :: TrieType
                                        ,singleton "foo" 1
                                        ]
 |])
$(makeFunc mapsOnly ["size","singleton","unionsWith'"] [d|
   unionsWith' size singleton unionsWith' =
      IS_STRICT $ unionsWith' undefined [singleton "foo" 1 :: TrieType
                                        ,singleton "foo" 1
                                        ]
 |])
$(makeFunc mapsOnly ["size","singleton","unionsWithKey"] [d|
   unionsWithKey size singleton unionsWithKey =
      IS_LAZY   $ unionsWithKey undefined  [singleton "foo" 1 :: TrieType
                                           ,singleton "foo" 1
                                           ]
 |])
$(makeFunc mapsOnly ["size","singleton","unionsWithKey'"] [d|
   unionsWithKey' size singleton unionsWithKey' =
      IS_STRICT $ unionsWithKey' undefined [singleton "foo" 1 :: TrieType
                                           ,singleton "foo" 1
                                           ]
 |])

-- As above, but for the intersection family.
$(makeFunc mapsOnly ["size","singleton","intersection"] [d|
   intersection size singleton intersection =
      IS_LAZY   $ intersection  (singleton "a" undefined :: TrieType)
                                (singleton "a" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","intersection'"] [d|
   intersection' size singleton intersection' =
      IS_STRICT $ intersection' (singleton "a" undefined :: TrieType)
                                (singleton "a" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","intersectionWith"] [d|
   intersectionWith size singleton intersectionWith =
      IS_LAZY   $ intersectionWith undefined  (singleton "a" 1 :: TrieType)
                                              (singleton "a" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","intersectionWith'"] [d|
   intersectionWith' size singleton intersectionWith' =
      IS_STRICT $ intersectionWith' undefined (singleton "a" 1 :: TrieType)
                                              (singleton "a" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","intersectionWithKey"] [d|
   intersectionWithKey size singleton intersectionWithKey =
      IS_LAZY   $ intersectionWithKey undefined  (singleton "a" 1 :: TrieType)
                                                 (singleton "a" 1)
 |])
$(makeFunc mapsOnly ["size","singleton","intersectionWithKey'"] [d|
   intersectionWithKey' size singleton intersectionWithKey' =
      IS_STRICT $ intersectionWithKey' undefined (singleton "a" 1 :: TrieType)
                                                 (singleton "a" 1)
 |])

tests = testGroup "Strictness"
   [ $(makeCases mapsOnly "insertWith")
   , $(makeCases mapsOnly "insertWith'1")
   , $(makeCases mapsOnly "insertWith'2")
   , $(makeCases mapsOnly "insertWith'3")
   , $(makeCases mapsOnly "adjust")
   , $(makeCases mapsOnly "adjust'")
   , $(makeCases mapsOnly "alter")
   , $(makeCases mapsOnly "alter'")
   , $(makeCases mapsOnly "union")
   , $(makeCases mapsOnly "union'")
   , $(makeCases mapsOnly "unionWith")
   , $(makeCases mapsOnly "unionWith'")
   , $(makeCases mapsOnly "unionWithKey")
   , $(makeCases mapsOnly "unionWithKey'")
   , $(makeCases mapsOnly "unions")
   , $(makeCases mapsOnly "unions'")
   , $(makeCases mapsOnly "unionsWith")
   , $(makeCases mapsOnly "unionsWith'")
   , $(makeCases mapsOnly "unionsWithKey")
   , $(makeCases mapsOnly "unionsWithKey'")
   , $(makeCases mapsOnly "intersection")
   , $(makeCases mapsOnly "intersection'")
   , $(makeCases mapsOnly "intersectionWith")
   , $(makeCases mapsOnly "intersectionWith'")
   , $(makeCases mapsOnly "intersectionWithKey")
   , $(makeCases mapsOnly "intersectionWithKey'")
   ]
