-- File created: 2008-11-11 11:24:30

-- The base implementation of a trie representing a map with list keys,
-- generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set and
-- @m@ to their maximum length. In addition, the trie's branching factor plays
-- a part in almost every operation, but the complexity depends on the
-- underlying Map. Thus, for instance, 'member' is actually O(m f(b)) where
-- f(b) is the complexity of a lookup operation on the Map used. Because this
-- complexity depends on the underlying operation, which is visible only in the
-- source code and thus can be changed whilst affecting the complexity only for
-- certain Map types, this "b factor" is not shown explicitly.
--
-- Disclaimer: the complexities have not been proven.
--
-- Strict versions of functions are provided for those who want to be certain
-- that their TrieMap doesn't contain values consisting of unevaluated thunks.
-- Note, however, that they do not evaluate the whole trie strictly, only the
-- values. And only to one level of depth: for instance, 'alter\'' and
-- 'differenceWith\'' do not 'seq' the value within the Maybe, only the Maybe
-- itself. The user should add the strictness in such cases himself, if he so
-- wishes.

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances
           , FlexibleContexts, UndecidableInstances #-}

#include "exports.h"

module Data.Trie.Map (MAP_EXPORTS) where

import Control.Applicative ((<*>),(<$>))
import Control.Arrow       ((***), second)
import qualified Data.DList as DL
import Data.Either         (partitionEithers)
import qualified Data.Foldable as F
import qualified Data.Maybe as Maybe
import Data.Monoid         (Monoid(..))
import Data.Traversable    (Traversable(traverse))
import Prelude hiding      (filter, foldr, lookup, map, null)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident))
#endif

import qualified Data.Trie.Base     as Base
import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Classes (fmap')
import Data.Trie.Base.Map     (Map, OrdMap)

-- Invariant: any (Tr Nothing _) has a Just descendant.
data TrieMap map k v = Tr (Maybe v) !(CMap map k v)

type CMap map k v = map k (TrieMap map k v)

instance Map map k => Base.Trie TrieMap Maybe map k where
   mkTrie = Tr
   tParts (Tr v m) = (v,m)

instance (Eq (CMap map k a), Eq a) => Eq (TrieMap map k a) where
   Tr v1 m1 == Tr v2 m2 = v1 == v2 && m1 == m2

instance (Ord (CMap map k a), Ord a) => Ord (TrieMap map k a) where
   compare (Tr v1 m1) (Tr v2 m2) =
      compare v1 v2 `mappend` compare m1 m2

instance Map map k => Monoid (TrieMap map k a) where
   mempty  = empty
   mappend = union
   mconcat = unions

instance Map map k => Functor (TrieMap map k) where
   fmap = map

instance Map map k => F.Foldable (TrieMap map k) where
   foldr = foldr

instance (Map map k, Traversable (map k)) => Traversable (TrieMap map k) where
   traverse f (Tr v m) = Tr <$> traverse f v <*> traverse (traverse f) m

instance (Map map k, Show k, Show a) => Show (TrieMap map k a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map k, Read k, Read a) => Read (TrieMap map k a) where
#if __GLASGOW_HASKELL__
   readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fmap fromList readPrec
#else
   readsPrec p = readParen (p > 10) $ \r -> do
      ("fromList", list) <- lex r
      (xs, rest) <- readsPrec (p+1) list
      [(fromList xs, rest)]
#endif

-- * Querying

-- O(1)
null :: Map map k => TrieMap map k a -> Bool
null = Base.null

-- O(n). The number of elements in the set.
size :: Map map k => TrieMap map k a -> Int
size = Base.size

-- O(m).
member :: Map map k => [k] -> TrieMap map k a -> Bool
member = Base.member

-- O(m).
notMember :: Map map k => [k] -> TrieMap map k a -> Bool
notMember = Base.notMember

-- O(m)
lookup :: Map map k => [k] -> TrieMap map k a -> Maybe a
lookup = Base.lookup

-- O(m)
lookupWithDefault :: Map map k => a -> [k] -> TrieMap map k a -> a
lookupWithDefault = Base.lookupWithDefault

-- O(min(n1,n2))
isSubmapOf :: (Map map k, Eq a) => TrieMap map k a -> TrieMap map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- O(min(n1,n2))
isSubmapOfBy :: Map map k
             => (a -> b -> Bool) -> TrieMap map k a -> TrieMap map k b -> Bool
isSubmapOfBy = Base.isSubmapOfBy

-- O(min(n1,n2))
isProperSubmapOf :: (Map map k, Eq a)
                 => TrieMap map k a -> TrieMap map k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- O(min(n1,n2))
isProperSubmapOfBy :: Map map k => (a -> b -> Bool)
                                -> TrieMap map k a
                                -> TrieMap map k b
                                -> Bool
isProperSubmapOfBy = Base.isProperSubmapOfBy

-- * Construction

-- O(1)
empty :: Map map k => TrieMap map k a
empty = Base.empty

-- O(m)
singleton :: Map map k => [k] -> a -> TrieMap map k a
singleton = Base.singleton

-- O(m)
insert :: Map map k => [k] -> a -> TrieMap map k a -> TrieMap map k a
insert = Base.insert

-- O(m)
insertWith :: Map map k
           => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith = Base.insertWith

-- O(m)
insertWith' :: Map map k
           => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith' = Base.insertWith'

-- O(m)
delete :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
delete = Base.delete

-- O(m)
adjust :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust = Base.adjust

-- O(m)
adjust' :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust' = Base.adjust'

-- O(m)
update :: Map map k
       => (a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
update f k = snd . updateLookup f k

-- O(m)
updateLookup :: Map map k => (a -> Maybe a)
                          -> [k]
                          -> TrieMap map k a
                          -> (Maybe a, TrieMap map k a)
updateLookup = Base.updateLookup

-- O(m)
alter :: Map map k
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter = Base.alter

-- O(m)
alter' :: Map map k
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter' = Base.alter'

-- * Combination

defaultUnion :: a -> a -> a
defaultUnion = const

-- O(min(n1,n2))
union :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union = unionWith defaultUnion

-- O(min(n1,n2))
union' :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union' = unionWith' defaultUnion

-- O(min(n1,n2))
unionWith :: Map map k => (a -> a -> a)
                       -> TrieMap map k a
                       -> TrieMap map k a
                       -> TrieMap map k a
unionWith = Base.unionWith

-- O(min(n1,n2))
unionWith' :: Map map k => (a -> a -> a)
                        -> TrieMap map k a
                        -> TrieMap map k a
                        -> TrieMap map k a
unionWith' = Base.unionWith'

-- O(min(n1,n2))
unionWithKey :: Map map k => ([k] -> a -> a -> a)
                          -> TrieMap map k a
                          -> TrieMap map k a
                          -> TrieMap map k a
unionWithKey = Base.unionWithKey

-- O(min(n1,n2))
unionWithKey' :: Map map k => ([k] -> a -> a -> a)
                           -> TrieMap map k a
                           -> TrieMap map k a
                           -> TrieMap map k a
unionWithKey' = Base.unionWithKey'

unions :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions = unionsWith defaultUnion

unions' :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions' = unionsWith' defaultUnion

unionsWith :: Map map k
           => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith = Base.unionsWith

unionsWith' :: Map map k
            => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith' = Base.unionsWith'

unionsWithKey :: Map map k
              => ([k] -> a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWithKey = Base.unionsWithKey

unionsWithKey' :: Map map k
               => ([k] -> a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWithKey' = Base.unionsWithKey'

-- O(min(n1,n2))
difference :: Map map k
           => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
difference = differenceWith (\_ _ -> Nothing)

-- O(min(n1,n2))
differenceWith :: Map map k => (a -> b -> Maybe a)
                            -> TrieMap map k a
                            -> TrieMap map k b
                            -> TrieMap map k a
differenceWith = Base.differenceWith

-- O(min(n1,n2))
differenceWithKey :: Map map k => ([k] -> a -> b -> Maybe a)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k a
differenceWithKey = Base.differenceWithKey

-- O(min(n1,n2))
intersection :: Map map k
             => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection = intersectionWith const

-- O(min(n1,n2))
intersection' :: Map map k
              => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection' = intersectionWith' const

-- O(min(n1,n2))
intersectionWith :: Map map k => (a -> b -> c)
                              -> TrieMap map k a
                              -> TrieMap map k b
                              -> TrieMap map k c
intersectionWith = Base.intersectionWith

-- O(min(n1,n2))
intersectionWith' :: Map map k => (a -> b -> c)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k c
intersectionWith' = Base.intersectionWith'

-- O(min(n1,n2))
intersectionWithKey :: Map map k => ([k] -> a -> b -> c)
                                 -> TrieMap map k a
                                 -> TrieMap map k b
                                 -> TrieMap map k c
intersectionWithKey = Base.intersectionWithKey

-- O(min(n1,n2))
intersectionWithKey' :: Map map k => ([k] -> a -> b -> c)
                                  -> TrieMap map k a
                                  -> TrieMap map k b
                                  -> TrieMap map k c
intersectionWithKey' = Base.intersectionWithKey'

-- * Filtering

-- O(n m)
filter :: Map map k => (a -> Bool) -> TrieMap map k a -> TrieMap map k a
filter = filterWithKey . const

-- O(n m)
filterWithKey :: Map map k
              => ([k] -> a -> Bool) -> TrieMap map k a -> TrieMap map k a
filterWithKey = Base.filterWithKey

-- O(n m)
partition :: Map map k => (a -> Bool)
                       -> TrieMap map k a
                       -> (TrieMap map k a, TrieMap map k a)
partition = partitionWithKey . const

-- O(n m)
partitionWithKey :: Map map k => ([k] -> a -> Bool)
                              -> TrieMap map k a
                              -> (TrieMap map k a, TrieMap map k a)
partitionWithKey = Base.partitionWithKey

-- O(m)
split :: OrdMap map k
      => [k] -> TrieMap map k a -> (TrieMap map k a, TrieMap map k a)
split = Base.split

-- O(m)
splitLookup :: OrdMap map k => [k]
                            -> TrieMap map k a
                            -> (TrieMap map k a, Maybe a, TrieMap map k a)
splitLookup = Base.splitLookup

-- O(n)
mapMaybe :: Map map k
         => (a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybe = mapMaybeWithKey . const

-- O(n)
mapMaybeWithKey :: Map map k
                => ([k] -> a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybeWithKey f =
   fromList . Maybe.mapMaybe (\(k,v) -> fmap ((,) k) (f k v)) . toList

-- O(n)
mapEither :: Map map k => (a -> Either b c)
                       -> TrieMap map k a
                       -> (TrieMap map k b, TrieMap map k c)
mapEither = mapEitherWithKey . const

-- O(n)
mapEitherWithKey :: Map map k => ([k] -> a -> Either b c)
                              -> TrieMap map k a
                              -> (TrieMap map k b, TrieMap map k c)
mapEitherWithKey f =
   (fromList *** fromList) . partitionEithers .
   Prelude.map (\(k,v) -> either (Left . (,) k) (Right . (,) k) (f k v)) .
   toList

-- * Mapping

-- O(n)
map :: Map map k => (a -> b) -> TrieMap map k a -> TrieMap map k b
map = genericMap fmap

-- O(n)
map' :: Map map k => (a -> b) -> TrieMap map k a -> TrieMap map k b
map' = genericMap fmap'

genericMap :: Map map k => ((a -> b) -> Maybe a -> Maybe b)
                        -> (a -> b) -> TrieMap map k a -> TrieMap map k b
genericMap myFmap f (Tr v m) = Tr (myFmap f v)
                                  (Map.map (genericMap myFmap f) m)

-- O(n)
mapWithKey :: Map map k
           => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey = genericMapWithKey fmap

-- O(n)
mapWithKey' :: Map map k
            => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey' = genericMapWithKey fmap'

genericMapWithKey :: Map map k
                  => ((a -> b) -> Maybe a -> Maybe b)
                  -> ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
genericMapWithKey = go DL.empty
 where
   go k myFmap f (Tr v m) =
      Tr (myFmap (f $ DL.toList k) v)
         (Map.mapWithKey (\x -> go (k `DL.snoc` x) myFmap f) m)

-- O(n)
mapAccum :: Map map k => (acc -> a -> (acc, b))
                      -> acc
                      -> TrieMap map k a
                      -> (acc, TrieMap map k b)
mapAccum = genericMapAccum Map.mapAccum (flip const)

-- O(n)
mapAccum' :: Map map k => (acc -> a -> (acc, b))
                       -> acc
                       -> TrieMap map k a
                       -> (acc, TrieMap map k b)
mapAccum' = genericMapAccum Map.mapAccum seq

-- O(n)
mapAccumWithKey :: Map map k => (acc -> [k] -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumWithKey = genericMapAccumWithKey Map.mapAccumWithKey (flip const)

-- O(n)
mapAccumWithKey' :: Map map k => (acc -> [k] -> a -> (acc, b))
                              -> acc
                              -> TrieMap map k a
                              -> (acc, TrieMap map k b)
mapAccumWithKey' = genericMapAccumWithKey Map.mapAccumWithKey seq

-- O(n)
mapAccumAsc :: OrdMap map k => (acc -> a -> (acc, b))
                            -> acc
                            -> TrieMap map k a
                            -> (acc, TrieMap map k b)
mapAccumAsc = genericMapAccum Map.mapAccumAsc (flip const)

-- O(n)
mapAccumAsc' :: OrdMap map k => (acc -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumAsc' = genericMapAccum Map.mapAccumAsc seq

-- O(n)
mapAccumAscWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                   -> acc
                                   -> TrieMap map k a
                                   -> (acc, TrieMap map k b)
mapAccumAscWithKey = genericMapAccumWithKey Map.mapAccumAscWithKey (flip const)

-- O(n)
mapAccumAscWithKey' :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                    -> acc
                                    -> TrieMap map k a
                                    -> (acc, TrieMap map k b)
mapAccumAscWithKey' = genericMapAccumWithKey Map.mapAccumAscWithKey seq

-- O(n)
mapAccumDesc :: OrdMap map k => (acc -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumDesc = genericMapAccum Map.mapAccumDesc (flip const)

-- O(n)
mapAccumDesc' :: OrdMap map k => (acc -> a -> (acc, b))
                              -> acc
                              -> TrieMap map k a
                              -> (acc, TrieMap map k b)
mapAccumDesc' = genericMapAccum Map.mapAccumDesc seq

-- O(n)
mapAccumDescWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                    -> acc
                                    -> TrieMap map k a
                                    -> (acc, TrieMap map k b)
mapAccumDescWithKey =
   genericMapAccumWithKey Map.mapAccumDescWithKey (flip const)

-- O(n)
mapAccumDescWithKey' :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                     -> acc
                                     -> TrieMap map k a
                                     -> (acc, TrieMap map k b)
mapAccumDescWithKey' = genericMapAccumWithKey Map.mapAccumDescWithKey seq

genericMapAccum :: Map map k
                => (  (acc -> TrieMap map k a -> (acc, TrieMap map k b))
                   -> acc
                   -> CMap map k a
                   -> (acc, CMap map k b)
                   )
                -> (b -> (acc, Maybe b) -> (acc, Maybe b))
                -> (acc -> a -> (acc, b))
                -> acc
                -> TrieMap map k a
                -> (acc, TrieMap map k b)
genericMapAccum subMapAccum seeq f acc (Tr mv m) =
   let (acc', mv') =
          case mv of
               Nothing -> (acc, Nothing)
               Just v  ->
                  let (acc'', v') = f acc v
                   in v' `seeq` (acc'', Just v')
    in second (Tr mv') $
          subMapAccum (genericMapAccum subMapAccum seeq f) acc' m

genericMapAccumWithKey :: Map map k
                       => (  (  acc
                             -> k
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
                             )
                          -> acc
                          -> CMap map k a
                          -> (acc, CMap map k b)
                          )
                       -> (b -> (acc, Maybe b) -> (acc, Maybe b))
                       -> (acc -> [k] -> a -> (acc, b))
                       -> acc
                       -> TrieMap map k a
                       -> (acc, TrieMap map k b)
genericMapAccumWithKey = go DL.empty
 where
   go k subMapAccum seeq f acc (Tr mv m) =
      let (acc', mv') =
             case mv of
                  Nothing -> (acc, Nothing)
                  Just v  ->
                     let (acc'', v') = f acc (DL.toList k) v
                      in v' `seeq` (acc'', Just v')
       in second (Tr mv') $
             subMapAccum (\a x -> go (k `DL.snoc` x) subMapAccum seeq f a)
                         acc' m

-- O(n m)
mapKeys :: (Map map k1, Map map k2)
        => ([k1] -> [k2]) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys = mapKeysWith const

-- O(n m)
mapKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                        -> ([k1] -> [k2])
                                        -> TrieMap map k1 a
                                        -> TrieMap map k2 a
mapKeysWith = Base.mapKeysWith . fromListWith

-- O(n)
mapInKeys :: (Map map k1, Map map k2)
          => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapInKeys = mapInKeysWith defaultUnion

-- O(n)
mapInKeys' :: (Map map k1, Map map k2)
           => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapInKeys' = mapInKeysWith' defaultUnion

-- O(n)
mapInKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                          -> (k1 -> k2)
                                          -> TrieMap map k1 a
                                          -> TrieMap map k2 a
mapInKeysWith = Base.mapInKeysWith

-- O(n)
mapInKeysWith' :: (Map map k1, Map map k2) => (a -> a -> a)
                                           -> (k1 -> k2)
                                           -> TrieMap map k1 a
                                           -> TrieMap map k2 a
mapInKeysWith' = Base.mapInKeysWith'

-- * Folding

-- O(n)
foldr :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldr = foldrWithKey . const

-- O(n)
foldrWithKey :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrWithKey = Base.foldrWithKey

-- O(n)
foldrAsc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldrAsc = foldrAscWithKey . const

-- O(n)
foldrAscWithKey :: OrdMap map k
                => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrAscWithKey = Base.foldrAscWithKey

-- O(n)
foldrDesc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldrDesc = foldrDescWithKey . const

-- O(n)
foldrDescWithKey :: OrdMap map k
                 => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrDescWithKey = Base.foldrDescWithKey

-- O(n)
foldl' :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldl' = foldlWithKey' . const

-- O(n)
foldlWithKey' :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlWithKey' = Base.foldlWithKey'

-- O(n)
foldlAsc' :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlAsc' = foldlAscWithKey' . const

-- O(n)
foldlAscWithKey' :: OrdMap map k
                 => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlAscWithKey' = Base.foldlAscWithKey'

-- O(n)
foldlDesc' :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlDesc' = foldlDescWithKey' . const

-- O(n)
foldlDescWithKey' :: OrdMap map k
                  => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlDescWithKey' = Base.foldlDescWithKey'

-- * Conversion between lists

-- O(n)
toList :: Map map k => TrieMap map k a -> [([k],a)]
toList = Base.toList

-- O(n)
toAscList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toAscList = Base.toAscList

-- O(n)
toDescList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toDescList = Base.toDescList

-- O(n m)
fromList :: Map map k => [([k],a)] -> TrieMap map k a
fromList = Base.fromList

-- O(n m)
fromListWith :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith = Base.fromListWith

-- O(n m)
fromListWith' :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith' = Base.fromListWith'

-- O(n m)
fromListWithKey :: Map map k
                => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey = Base.fromListWithKey

-- O(n m)
fromListWithKey' :: Map map k
                 => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey' = Base.fromListWithKey'

-- * Min/max

-- O(m)
findMin :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMin = Base.findMin

-- O(m)
findMax :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMax = Base.findMax

-- O(m)
deleteMin :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMin = Base.deleteMin

-- O(m)
deleteMax :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMax = Base.deleteMax

-- O(m)
minView :: OrdMap map k => TrieMap map k a -> (Maybe ([k], a), TrieMap map k a)
minView = Base.minView

-- O(m)
maxView :: OrdMap map k => TrieMap map k a -> (Maybe ([k], a), TrieMap map k a)
maxView = Base.maxView

-- O(m)
findPredecessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findPredecessor = Base.findPredecessor

-- O(m)
findSuccessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findSuccessor = Base.findSuccessor

-- * Trie-only operations

-- O(s) where s is the input
addPrefix :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
addPrefix = Base.addPrefix

-- O(m)
splitPrefix :: Map map k => TrieMap map k a -> ([k], Maybe a, TrieMap map k a)
splitPrefix = Base.splitPrefix

-- O(m)
lookupPrefix :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
lookupPrefix = Base.lookupPrefix

-- O(m)
children :: Map map k => TrieMap map k a -> [(k, TrieMap map k a)]
children = Base.children

-- * Visualization

showTrie :: (Show k, Show a, Map map k) => TrieMap map k a -> ShowS
showTrie = Base.showTrieWith $ \mv -> case mv of
                                           Nothing -> showChar ' '
                                           Just v  -> showsPrec 11 v

showTrieWith :: (Show k, Map map k)
             => (Maybe a -> ShowS) -> TrieMap map k a -> ShowS
showTrieWith = Base.showTrieWith
