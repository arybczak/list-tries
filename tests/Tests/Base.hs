-- File created: 2009-01-06 13:01:36

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances
           , FunctionalDependencies #-}

module Tests.Base (Str(..), alpha, unArb) where

import Control.Arrow
import Test.QuickCheck

newtype Str = Str { unStr :: String } deriving Show

alpha = ('a','z')

instance Arbitrary Str where
   arbitrary = sized $ \size -> do
      s <- mapM (const $ choose alpha) [0..size]
      return (Str s)

-- This is a bit of a hack to make life easy, so that we can use unArb always
-- instead of having to change it to first unArb for map types, which would be
-- a bit of a pain and add complexity to TH.
class UnArbitrary a b | b -> a where unArb :: a -> b

instance UnArbitrary Str [Char] where unArb = unStr
instance UnArbitrary (Str,c) ([Char],c) where unArb = first unStr
