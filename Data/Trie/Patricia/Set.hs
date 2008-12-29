-- File created: 2008-11-08 19:22:07

-- The base implementation of a Patricia trie representing a set of lists,
-- generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Trie.Patricia.Set where

import Control.Arrow  ((***))
import Data.Function  (on)
import Prelude hiding (map)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident), pfail)
#endif

import qualified Data.Trie.Base.Map      as Map
import qualified Data.Trie.Patricia.Base as Base
import Data.Trie.Base.Classes (Identity(..), Unwrappable(..))
import Data.Trie.Base.Map     (Map, OrdMap)
import Data.Trie.Util         ((.:), (.:.), both)

-- Invariant: any (Tr False _ _) has at least two children, all of which are
-- True or have a True descendant.
--
-- This Base stuff is needed just as in the non-Patricia version.
data TrieSetBase map a bool = Tr !bool ![a] !(CMapBase map a bool)

newtype TrieSet map a = TS { unTS :: TrieSetBase map a Bool }

inTS :: (TrieSetBase map a Bool -> TrieSetBase nap b Bool)
     -> (TrieSet map a -> TrieSet nap b)
inTS f = TS . f . unTS

type CMapBase map a bool = map a (TrieSetBase map a bool)
type CMap map a = CMapBase map a Bool

instance Map map k => Base.Trie TrieSetBase Identity map k where
   mkTrie = Tr . unwrap
   tParts (Tr b p m) = (Id b,p,m)

-- instances: Eq, Monoid, Foldable, Ord

instance (Map map a, Show a) => Show (TrieSet map a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map a, Read a) => Read (TrieSet map a) where
#if __GLASGOW_HASKELL__
   readPrec = parens $ prec 10 $ do
      text <- lexP
      if text == Ident "fromList"
         then fmap fromList readPrec
         else pfail
#else
   readsPrec p = readParen (p > 10) $ \r -> do
      (text, list) <- lex r
      if text == "fromList"
         then do
            (xs, rest) <- reads list
            [(fromList xs, rest)]
         else []
#endif

-- * Querying

-- O(1)
null :: Map map a => TrieSet map a -> Bool
null = Base.null . unTS

-- O(n). The number of elements in the set.
size :: Map map a => TrieSet map a -> Int
size = Base.size . unTS

-- O(m).
member :: Map map a => [a] -> TrieSet map a -> Bool
member = Base.member .:. unTS

-- O(?)
isSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isSubsetOf = Base.isSubmapOfBy (\_ _ -> Id True) `on` unTS

-- O(?)
isProperSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isProperSubsetOf = Base.isProperSubmapOfBy (\_ _ -> Id True) `on` unTS

-- * Construction

-- O(1)
empty :: Map map a => TrieSet map a
empty = TS Base.empty

-- O(1)
singleton :: Map map a => [a] -> TrieSet map a
singleton k = TS$ Base.singleton k True

-- O(m)
insert :: Map map a => [a] -> TrieSet map a -> TrieSet map a
insert k = inTS$ Base.insert k True

-- O(m)
delete :: Map map a => [a] -> TrieSet map a -> TrieSet map a
delete = inTS . Base.delete

-- * Combination

defaultUnion :: Bool -> Bool -> Bool
defaultUnion = error "TrieSet.union :: internal error"

union :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
union = TS .: Base.unionWith defaultUnion `on` unTS

unions :: Map map a => [TrieSet map a] -> TrieSet map a
unions = TS . Base.unionsWith defaultUnion . Prelude.map unTS

difference :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
difference = TS .: Base.differenceWith
                      (error "TrieSet.difference :: internal error")
                   `on` unTS

intersection :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
intersection = TS .: Base.intersectionWith
                        (error "TrieSet.intersection :: internal error")
                     `on` unTS

-- * Filtering

-- O(n)
filter :: Map map a => ([a] -> Bool) -> TrieSet map a -> TrieSet map a
filter p = inTS $ Base.filterWithKey (\k _ -> p k)

-- O(n)
partition :: Map map a
          => ([a] -> Bool) -> TrieSet map a -> (TrieSet map a, TrieSet map a)
partition p = both TS . Base.partitionWithKey (\k _ -> p k) . unTS

split :: OrdMap map a => [a] -> TrieSet map a -> (TrieSet map a, TrieSet map a)
split = both TS .: Base.split .:. unTS

splitMember :: OrdMap map a
            => [a] -> TrieSet map a -> (TrieSet map a, Bool, TrieSet map a)
splitMember = (\(l,b,g) -> (TS l,unwrap b,TS g)) .: Base.splitLookup .:. unTS

-- * Mapping

-- O(n)
map :: (Map map a, Map map b) => ([a] -> [b]) -> TrieSet map a -> TrieSet map b
map = inTS . Base.mapKeysWith Base.fromList

-- O(n)
-- needs a name!
map' :: (Map map a, Map map b) => (a -> b) -> TrieSet map a -> TrieSet map b
map' = inTS . Base.mapKeys'With defaultUnion

-- * Folding

-- O(n)
fold :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
fold f = Base.foldWithKey (\k _ -> f k) .:. unTS

-- O(n)
foldAsc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldAsc f = Base.foldAscWithKey (\k _ -> f k) .:. unTS

-- O(n)
foldDesc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldDesc f = Base.foldDescWithKey (\k _ -> f k) .:. unTS

-- * Conversion between lists

-- O(n)
toList :: Map map a => TrieSet map a -> [[a]]
toList = Prelude.map fst . Base.toList . unTS

-- O(n)
toAscList :: OrdMap map a => TrieSet map a -> [[a]]
toAscList = Prelude.map fst . Base.toAscList . unTS

-- O(n)
toDescList :: OrdMap map a => TrieSet map a -> [[a]]
toDescList = Prelude.map fst . Base.toDescList . unTS

-- O(n)
fromList :: Map map a => [[a]] -> TrieSet map a
fromList = TS . Base.fromList . Prelude.map (flip (,) True)

-- * Min/max

-- O(m log b)
findMin :: OrdMap map a => TrieSet map a -> Maybe [a]
findMin = fmap fst . Base.findMin . unTS

-- O(m log b)
findMax :: OrdMap map a => TrieSet map a -> Maybe [a]
findMax = fmap fst . Base.findMax . unTS

-- O(m log b)
deleteMin :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMin = inTS Base.deleteMin

-- O(m log b)
deleteMax :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMax = inTS Base.deleteMax

-- O(m log b)
minView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
minView = fmap (fst *** TS) . Base.minView . unTS

-- O(m log b)
maxView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
maxView = fmap (fst *** TS) . Base.maxView . unTS

-- O(m b)
findPredecessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
findPredecessor = fmap fst .: Base.findPredecessor . unTS

-- O(m b)
findSuccessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
findSuccessor = fmap fst .: Base.findSuccessor . unTS
