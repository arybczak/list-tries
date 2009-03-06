-- File created: 2009-01-06 13:01:36

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
           , FunctionalDependencies, FlexibleContexts #-}

module Tests.Base (Str(..), alpha, unArb, getKey) where

import Control.Arrow   (first)
import Test.QuickCheck (Arbitrary(arbitrary, shrink), sized, choose)

import Data.ListTrie.Base.Map (Map)
import qualified Data.ListTrie.Set          as  BS
import qualified Data.ListTrie.Map          as  BM
import qualified Data.ListTrie.Patricia.Set as PBS
import qualified Data.ListTrie.Patricia.Map as PBM

newtype Str = Str { unStr :: String } deriving Show

alpha = ('0','9')

instance Arbitrary Str where
   arbitrary = sized $ \size -> do
      s <- mapM (const $ choose alpha) [0..size `mod` 6]
      return (Str s)

   shrink (Str s) = map Str (shrink s)

instance Map map Char => Arbitrary ( BS.TrieSet map Char) where
   arbitrary = fmap ( BS.fromList . map unArb) arbitrary
instance Map map Char => Arbitrary (PBS.TrieSet map Char) where
   arbitrary = fmap (PBS.fromList . map unArb) arbitrary
instance (Map map Char, Arbitrary a) => Arbitrary ( BM.TrieMap map Char a)
 where
   arbitrary = fmap ( BM.fromList . map unArb) arbitrary
instance (Map map Char, Arbitrary a) => Arbitrary (PBM.TrieMap map Char a)
 where
   arbitrary = fmap (PBM.fromList . map unArb) arbitrary

--------- HACKS TO MAKE LIFE EASY

-- Some classes that allow us to convert between Str and String for both Sets
-- (where we're interested only in Str) and Maps (where it's (Str,value))
-- without having to write the functions twice, adding complexity either to
-- them or to TH.

class UnArbitrary a b | b -> a where unArb :: a -> b

instance UnArbitrary Str [Char] where unArb = unStr
instance UnArbitrary (Str,c) ([Char],c) where unArb = first unStr

class GetKey a where getKey :: a -> String
instance GetKey [Char] where getKey = id
instance GetKey ([Char],a) where getKey = fst
