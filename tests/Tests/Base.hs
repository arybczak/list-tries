-- File created: 2009-01-06 13:01:36

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
           , FunctionalDependencies, FlexibleContexts #-}

module Tests.Base (Str(..), alpha, unArb) where

import Control.Arrow   (first)
import Test.QuickCheck

import Data.Trie.Base.Map (Map)
import qualified Data.Trie.Set          as  BS
import qualified Data.Trie.Map          as  BM
import qualified Data.Trie.Patricia.Set as PBS
import qualified Data.Trie.Patricia.Map as PBM

newtype Str = Str { unStr :: String } deriving Show

alpha = ('a','z')

instance Arbitrary Str where
   arbitrary = sized $ \size -> do
      s <- mapM (const $ choose alpha) [0..size]
      return (Str s)

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

-- This is a bit of a hack to make life easy, so that we can use unArb always
-- instead of having to change it to first unArb for map types, which would be
-- a bit of a pain and add complexity to TH.
class UnArbitrary a b | b -> a where unArb :: a -> b

instance UnArbitrary Str [Char] where unArb = unStr
instance UnArbitrary (Str,c) ([Char],c) where unArb = first unStr
