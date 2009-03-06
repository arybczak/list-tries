-- File created: 2009-01-06 13:08:00

{-# LANGUAGE CPP, TemplateHaskell #-}

module Tests.Strictness (tests) where

import Test.ChasingBottoms.IsBottom   (isBottom)
import Test.Framework                 (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assert)

import qualified Data.ListTrie.Set.Eq
import qualified Data.ListTrie.Set.Ord
import qualified Data.ListTrie.Set.Enum
import qualified Data.ListTrie.Map.Eq
import qualified Data.ListTrie.Map.Ord
import qualified Data.ListTrie.Map.Enum
import qualified Data.ListTrie.Patricia.Set.Eq
import qualified Data.ListTrie.Patricia.Set.Ord
import qualified Data.ListTrie.Patricia.Set.Enum
import qualified Data.ListTrie.Patricia.Map.Eq
import qualified Data.ListTrie.Patricia.Map.Ord
import qualified Data.ListTrie.Patricia.Map.Enum

import Tests.Base
import Tests.TH

-- size doesn't evaluate the values but it does traverse the whole trie
-- returning a single result, so it works well for checking whether there are
-- any bottoms in the trie
#define IS_LAZY   (not.isBottom.size)
#define IS_STRICT (    isBottom.size)

-- insertWith' should apply the combining function strictly, insertWith should
-- not. We use a singleton to make sure that the combining function is called.
$(makeFunc mapsOnly ["size","singleton","insertWith"] [d|
   insertWith size singleton insertWith =
      IS_LAZY   . insertWith  undefined "foo" undefined $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'1 size singleton insertWith' =
      IS_STRICT . insertWith' undefined "foo" undefined $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'2 size singleton insertWith' =
      IS_STRICT . insertWith' (+)       "foo" undefined $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","insertWith'"] [d|
   insertWith'3 size singleton insertWith' =
      IS_STRICT . insertWith' undefined "foo" 0 $
         (singleton "foo" 0 :: TrieType)
 |])

-- As above, but for adjust' and adjust.
$(makeFunc mapsOnly ["size","singleton","adjust"] [d|
   adjust size singleton adjust =
      IS_LAZY   . adjust  undefined "foo" $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","adjust'"] [d|
   adjust' size singleton adjust' =
      IS_STRICT . adjust' undefined "foo" $ (singleton "foo" 0 :: TrieType)
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

-- As above, but for the map family.
$(makeFunc mapsOnly ["size","singleton","map"] [d|
   map size singleton map =
      IS_LAZY   . map  undefined $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","map'"] [d|
   map' size singleton map' =
      IS_STRICT . map' undefined $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapWithKey"] [d|
   mapWithKey size singleton mapWithKey =
      IS_LAZY   . mapWithKey  undefined $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapWithKey'"] [d|
   mapWithKey' size singleton mapWithKey' =
      IS_STRICT . mapWithKey' undefined $ (singleton "foo" 0 :: TrieType)
 |])

-- As above, but for the mapInKeys family.
--
-- The *With ones need to actually trigger the union function, hence a simple
-- singleton won't do.
$(makeFunc mapsOnly ["size","fromList","mapInKeys"] [d|
   mapInKeys size fromList mapInKeys =
      IS_LAZY   . mapInKeys (const 'x') $
         (fromList [("xy",0),("xz",undefined)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromList","mapInKeys'"] [d|
   mapInKeys' size fromList mapInKeys' =
      IS_STRICT . mapInKeys' (const 'x') $
         (fromList [("xy",0),("xz",undefined)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromList","mapInKeysWith"] [d|
   mapInKeysWith size fromList mapInKeysWith =
      IS_LAZY   . mapInKeysWith  undefined (const 'x') $
         (fromList [("xy",0),("xz",1)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromList","mapInKeysWith'"] [d|
   mapInKeysWith' size fromList mapInKeysWith' =
      IS_STRICT . mapInKeysWith' undefined (const 'x') $
         (fromList [("xy",0),("xz",1)] :: TrieType)
 |])

-- As above, but for the mapAccum family.
$(makeFunc mapsOnly ["size","singleton","mapAccum"] [d|
   mapAccum size singleton mapAccum =
      IS_LAZY   . snd . mapAccum  undefined 0 $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccum'"] [d|
   mapAccum' size singleton mapAccum' =
      IS_STRICT . snd . mapAccum' undefined 0 $ (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumWithKey"] [d|
   mapAccumWithKey size singleton mapAccumWithKey =
      IS_LAZY   . snd . mapAccumWithKey  undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumWithKey'"] [d|
   mapAccumWithKey' size singleton mapAccumWithKey' =
      IS_STRICT . snd . mapAccumWithKey' undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumAsc"] [d|
   mapAccumAsc size singleton mapAccumAsc =
      IS_LAZY   . snd . mapAccumAsc  undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumAsc'"] [d|
   mapAccumAsc' size singleton mapAccumAsc' =
      IS_STRICT . snd . mapAccumAsc' undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumAscWithKey"] [d|
   mapAccumAscWithKey size singleton mapAccumAscWithKey =
      IS_LAZY   . snd . mapAccumAscWithKey  undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumAscWithKey'"] [d|
   mapAccumAscWithKey' size singleton mapAccumAscWithKey' =
      IS_STRICT . snd . mapAccumAscWithKey' undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumDesc"] [d|
   mapAccumDesc size singleton mapAccumDesc =
      IS_LAZY   . snd . mapAccumDesc  undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumDesc'"] [d|
   mapAccumDesc' size singleton mapAccumDesc' =
      IS_STRICT . snd . mapAccumDesc' undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumDescWithKey"] [d|
   mapAccumDescWithKey size singleton mapAccumDescWithKey =
      IS_LAZY   . snd . mapAccumDescWithKey  undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])
$(makeFunc mapsOnly ["size","singleton","mapAccumDescWithKey'"] [d|
   mapAccumDescWithKey' size singleton mapAccumDescWithKey' =
      IS_STRICT . snd . mapAccumDescWithKey' undefined 0 $
         (singleton "foo" 0 :: TrieType)
 |])

-- As above, but for the fromListWith family.
$(makeFunc mapsOnly ["size","fromListWith"] [d|
   fromListWith size fromListWith =
      IS_LAZY   (fromListWith  undefined [("a",1),("a",2)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromListWith'"] [d|
   fromListWith' size fromListWith' =
      IS_STRICT (fromListWith' undefined [("a",1),("a",2)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromListWithKey"] [d|
   fromListWithKey size fromListWithKey =
      IS_LAZY   (fromListWithKey  undefined [("a",1),("a",2)] :: TrieType)
 |])
$(makeFunc mapsOnly ["size","fromListWithKey'"] [d|
   fromListWithKey' size fromListWithKey' =
      IS_STRICT (fromListWithKey' undefined [("a",1),("a",2)] :: TrieType)
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
   , $(makeCases mapsOnly "map")
   , $(makeCases mapsOnly "map'")
   , $(makeCases mapsOnly "mapWithKey")
   , $(makeCases mapsOnly "mapWithKey'")
   , $(makeCases mapsOnly "mapInKeys")
   , $(makeCases mapsOnly "mapInKeys'")
   , $(makeCases mapsOnly "mapInKeysWith")
   , $(makeCases mapsOnly "mapInKeysWith'")
   , $(makeCases mapsOnly "mapAccum")
   , $(makeCases mapsOnly "mapAccum'")
   , $(makeCases mapsOnly "mapAccumWithKey")
   , $(makeCases mapsOnly "mapAccumWithKey'")
   , $(makeCases mapsOnly "mapAccumAsc")
   , $(makeCases mapsOnly "mapAccumAsc'")
   , $(makeCases mapsOnly "mapAccumAscWithKey")
   , $(makeCases mapsOnly "mapAccumAscWithKey'")
   , $(makeCases mapsOnly "mapAccumDesc")
   , $(makeCases mapsOnly "mapAccumDesc'")
   , $(makeCases mapsOnly "mapAccumDescWithKey")
   , $(makeCases mapsOnly "mapAccumDescWithKey'")
   , $(makeCases mapsOnly "fromListWith")
   , $(makeCases mapsOnly "fromListWith'")
   , $(makeCases mapsOnly "fromListWithKey")
   , $(makeCases mapsOnly "fromListWithKey'")
   ]
