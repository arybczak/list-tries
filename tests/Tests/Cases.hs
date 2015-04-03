-- File created: 2009-01-16 18:54:26

{-# LANGUAGE TemplateHaskell #-}

module Tests.Cases (tests) where

import Control.Monad                  (join)
import Test.Framework                 (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (assert)

import qualified Data.ListTrie.Base.Map as Map
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

import Tests.TH

$(makeFunc allTries ["null","empty"] [d|
   nullEmpty null empty = null (empty :: TrieType)
 |])

-- "foo" is obviously not equal to "fo"
$(makeFunc setsOnly ["singleton"] [d|
   eq1_s singleton = singleton "foo" /= (singleton "fo" :: TrieType)
 |])
$(makeFunc mapsOnly ["singleton"] [d|
   eq1_m singleton = singleton "foo" 0 /= (singleton "fo" 0 :: TrieType)
 |])

-- eq1 via compare instead of ==
$(makeFunc setsOnly ["singleton"] [d|
   ord1_s singleton =
      compare (singleton "foo") (singleton "fo" :: TrieType) == GT
 |])
$(makeFunc mapsOnly ["singleton"] [d|
   ord1_m singleton =
      compare (singleton "foo" 0) (singleton "fo" 0 :: TrieType) == GT
 |])

-- Subset/map tests where the maps aren't identical or empty, couldn't think of
-- a good property for such cases
$(makeFunc setsOnly ["fromList","isSubsetOf"] [d|
   isSubsetOf1 fromList isSubsetOf =
      (fromList ["cameroon","camera"] :: TrieType)
      `isSubsetOf`
      fromList ["cameroon","camera","camel","camouflage","cat"]
 |])
$(makeFunc setsOnly ["fromList","isSubsetOf"] [d|
   isSubsetOf2 fromList isSubsetOf =
      not $
         (fromList ["cameroon","camera","came"] :: TrieType)
         `isSubsetOf`
         fromList ["cameroon","camera","camel","camouflage","cat"]
 |])
$(makeFunc mapsOnly ["fromList","isSubmapOf"] [d|
   isSubmapOf1 fromList isSubmapOf =
      not $
         (fromList (zip ["cameroon","camera","came"] [0..]) :: TrieType)
         `isSubmapOf`
         fromList (zip ["cameroon","camera","camel","camouflage","cat"] [0..])
 |])

-- Simple tests for alter to up the code coverage a bit
$(makeFunc mapsOnly ["fromList","alter"] [d|
   alter1 fromList alter =
      alter (\Nothing -> Just 42) "foo" (fromList [("foobar",0)] :: TrieType)
      == fromList [("foo",42),("foobar",0)]
 |])
$(makeFunc mapsOnly ["fromList","alter"] [d|
   alter2 fromList alter =
      let x = fromList [("xxx",0)] :: TrieType
       in alter id "x" x == x
 |])

-- Make sure insertWith applies the combining function in the right order
$(makeFunc mapsOnly ["singleton","insertWith"] [d|
   insertWith1 singleton insertWith =
      insertWith (-) [] 3 (singleton [] 1) == (singleton [] 2 :: TrieType)
 |])

-- And the same for fromListWith
$(makeFunc mapsOnly ["singleton","fromListWith"] [d|
   fromListWith1 singleton fromListWith =
      fromListWith (-) (zip (repeat []) [1..4]) ==
         (singleton [] 2 :: TrieType)
 |])

-- A couple of simple sanity tests for the *WithKey set operations since they
-- don't have properties at all
$(makeFunc mapsOnly ["fromList","unionWithKey"] [d|
   unionWithKey1 fromList unionWithKey =
      let al = ["tom","tome","tomatoes","fork"]
          bl = ["tom","tomb","tomes","tomato","fark"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in unionWithKey (\k vl vr -> vl - vr + length k) a b
          == fromList (("tom",3+1-length al) : zip (tail al ++ tail bl) [2..])
 |])
$(makeFunc mapsOnly ["fromList","differenceWithKey"] [d|
   differenceWithKey1 fromList differenceWithKey =
      let al = ["tom","tome","tomatoes","fork"]
          bl = ["tom","tomb","tomes","tomato","fark"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in differenceWithKey (\k vl vr -> Just $ vl - vr + length k) a b
          == fromList (("tom",3+1-length al) : zip (tail al) [2..])
 |])
$(makeFunc mapsOnly ["fromList","differenceWithKey"] [d|
   differenceWithKey2 fromList differenceWithKey =
      let al = ["shiner","shine"]
          bl = ["shiner","shin","shiners","shoe"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in differenceWithKey (\k vl vr -> Just $ vl - vr + length k) a b
          == fromList (("shiner",6+1-length al) : zip (tail al) [2..])
 |])
$(makeFunc mapsOnly ["fromList","differenceWithKey"] [d|
   differenceWithKey3 fromList differenceWithKey =
      let al = ["mar","marks","marksman","marksman's bow"]
          bl = ["mark","marksman's","marksman's bow"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in differenceWithKey (\_ _ _ -> Nothing) a b
          == fromList (zip (init al) [1..])
 |])
$(makeFunc mapsOnly ["fromList","intersectionWithKey"] [d|
   intersectionWithKey1 fromList intersectionWithKey =
      let al = ["cat","caterers","caterwauling","caterer"]
          bl = ["cat","caterers","c","caterwauler"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in intersectionWithKey (\k vl vr -> length k - vl + vr) a b
          == fromList (zip ["cat","caterers"] $
                zipWith3 (\a b c -> b + c - a)
                         [1..] [length al..] (map length al))
 |])
$(makeFunc mapsOnly ["fromList","intersectionWithKey"] [d|
   intersectionWithKey2 fromList intersectionWithKey =
      let al = ["wa","wart","wartortle"]
          bl = ["w","wartor","wartortles","wartortle army"]
          a = fromList $ zip al [1..]
          b = fromList $ zip bl [length al..]
       in intersectionWithKey undefined a b == (fromList [] :: TrieType)
 |])

-- children should return something nonempty even if there's only one path
-- through the trie
$(makeFunc setsOnly ["fromList","children"] [d|
   children1_s fromList children =
      children (fromList ["foo","foobar"] :: TrieType) ==
         Map.singleton 'b' (fromList ["ar"])
 |])
$(makeFunc mapsOnly ["fromList","children"] [d|
   children1_m fromList children =
      children (fromList [("foo",1),("foobar",2)] :: TrieType) ==
         Map.singleton 'b' (fromList [("ar",2)])
 |])

tests = testGroup "Individual cases"
   [ $(makeCases allTries "nullEmpty")
   , $(makeCases setsOnly "eq1_s")
   , $(makeCases mapsOnly "eq1_m")
   , $(makeCases setsOnly "ord1_s")
   , $(makeCases mapsOnly "ord1_m")
   , $(makeCases setsOnly "isSubsetOf1")
   , $(makeCases setsOnly "isSubsetOf2")
   , $(makeCases mapsOnly "isSubmapOf1")
   , $(makeCases mapsOnly "alter1")
   , $(makeCases mapsOnly "alter2")
   , $(makeCases mapsOnly "insertWith1")
   , $(makeCases mapsOnly "fromListWith1")
   , $(makeCases mapsOnly "unionWithKey1")
   , $(makeCases mapsOnly "differenceWithKey1")
   , $(makeCases mapsOnly "differenceWithKey2")
   , $(makeCases mapsOnly "differenceWithKey3")
   , $(makeCases mapsOnly "intersectionWithKey1")
   , $(makeCases mapsOnly "intersectionWithKey2")
   , $(makeCases setsOnly "children1_s")
   , $(makeCases mapsOnly "children1_m")
   ]
