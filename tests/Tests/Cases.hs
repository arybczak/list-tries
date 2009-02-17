-- File created: 2009-01-16 18:54:26

{-# LANGUAGE TemplateHaskell #-}

module Tests.Cases (tests) where

import Control.Monad                  (join)
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

import Data.Trie.Util

import Tests.TH

$(makeFunc allTries ["null","empty"] [d|
   nullEmpty null empty = null (empty :: TrieType)
 |])

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

-- A couple of simple sanity tests for the *WithKey set operations since they
-- don't have properties at all
$(makeFunc mapsOnly ["fromList","unionWithKey"] [d|
   unionWithKey1 fromList unionWithKey =
      let al = ["tom","tome","tomatoes","fork"]
          bl = ["tom","tomb","tomes","tomato","fark"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in unionWithKey (\k vl vr -> vl + vr + length k) a b
          == fromList (("tom",3+1+length al) : zip (tail al ++ tail bl) [2..])
 |])
$(makeFunc mapsOnly ["fromList","differenceWithKey"] [d|
   differenceWithKey1 fromList differenceWithKey =
      let al = ["tom","tome","tomatoes","fork"]
          bl = ["tom","tomb","tomes","tomato","fark"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in differenceWithKey (\k vl vr -> Just $ vl + vr + length k) a b
          == fromList (("tom",3+1+length al) : zip (tail al) [2..])
 |])
$(makeFunc mapsOnly ["fromList","differenceWithKey"] [d|
   differenceWithKey2 fromList differenceWithKey =
      let al = ["shiner","shine"]
          bl = ["shiner","shin","shiners","shoe"]
          a = fromList $ zip al [1..] :: TrieType
          b = fromList $ zip bl [length al..]
       in differenceWithKey (\k vl vr -> Just $ vl + vr + length k) a b
          == fromList (("shiner",6+1+length al) : zip (tail al) [2..])
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
       in intersectionWithKey (\k vl vr -> length k + vl + vr) a b
          == fromList (zip ["cat","caterers"] $
                zipWith3 (join (.:) (+)) [1..] [length al..] (map length al))
 |])
$(makeFunc mapsOnly ["fromList","intersectionWithKey"] [d|
   intersectionWithKey2 fromList intersectionWithKey =
      let al = ["wa","wart","wartortle"]
          bl = ["w","wartor","wartortles","wartortle army"]
          a = fromList $ zip al [1..]
          b = fromList $ zip bl [length al..]
       in intersectionWithKey undefined a b == (fromList [] :: TrieType)
 |])

tests = testGroup "Individual cases"
   [ $(makeCases allTries "nullEmpty")
   , $(makeCases setsOnly "isSubsetOf1")
   , $(makeCases setsOnly "isSubsetOf2")
   , $(makeCases mapsOnly "isSubmapOf1")
   , $(makeCases mapsOnly "alter1")
   , $(makeCases mapsOnly "alter2")
   , $(makeCases mapsOnly "unionWithKey1")
   , $(makeCases mapsOnly "differenceWithKey1")
   , $(makeCases mapsOnly "differenceWithKey2")
   , $(makeCases mapsOnly "differenceWithKey3")
   , $(makeCases mapsOnly "intersectionWithKey1")
   , $(makeCases mapsOnly "intersectionWithKey2")
   ]
