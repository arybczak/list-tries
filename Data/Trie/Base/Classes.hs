-- File created: 2008-12-27 20:53:49

-- Various type classes to make both (Maybe a) and (Identity Bool) work
-- wherever we need them.

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleInstances #-}

module Data.Trie.Base.Classes where

import qualified Control.Applicative as A
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM2)
import Data.Maybe          (fromJust, isJust)

-- Funky instances for this type are marked with **FUNKY**
newtype Identity a = Id a

class Unwrappable  w where unwrap  :: w a -> a
class Boolable     b where toBool  :: b -> Bool

instance Unwrappable  Maybe    where unwrap = fromJust
instance Boolable    (Maybe a) where toBool = isJust

instance Unwrappable Identity       where unwrap (Id a) = a
instance Boolable   (Identity Bool) where toBool = unwrap

class Unionable v a where
   unionVals        :: (a -> a -> a)       -> v a -> v a -> v a
class Differentiable v a b where
   differenceVals   :: (a -> b -> Maybe a) -> v a -> v b -> v a
class Intersectable v a b c where
   intersectionVals :: (a -> b -> c)       -> v a -> v b -> v c

instance Unionable    Maybe a  where
   unionVals f (Just a) (Just b) = Just (f a b)
   unionVals _ Nothing  mb       = mb
   unionVals _ ma       _        = ma

instance Differentiable Maybe a b where
   differenceVals f (Just a) (Just b) = f a b
   differenceVals _ ma       _        = ma

instance Intersectable Maybe a b c where
   intersectionVals = liftM2

-- The other option with the following three would have been to just call f
-- (and, in the case of Differentiable, fromJust) and trust that it's correct.
-- I think this way is safer. Bottoms are passed to Base.unionWith etc.

-- **FUNKY**
instance Unionable Identity Bool where
   unionVals _ (Id a) (Id b) = Id$ a || b

-- **FUNKY**
instance Differentiable Identity Bool Bool where
   differenceVals _ (Id a) (Id b) = Id$ a && not b

-- **FUNKY**
instance Intersectable Identity Bool Bool Bool where
   intersectionVals _ (Id a) (Id b) = Id$ a && b

-- TODO: might be reasonably doable with plain Alternative: WrappedMonad for
-- Identity and a Monoid for Bool (monadLib has this stuff but if base doesn't
-- then screw it)
class Applicative a => Alt a x where
   altEmpty :: a x
   (<|>) :: a x -> a x -> a x

instance Functor Identity where
   fmap f (Id a) = Id (f a)

instance Applicative Identity where
   pure = Id
   Id f <*> Id a = Id (f a)

instance Alt Maybe a where
   altEmpty = A.empty
   (<|>) = (A.<|>)

instance Alt Identity Bool where
   altEmpty = Id False
   Id a <|> Id b = Id (a || b)
