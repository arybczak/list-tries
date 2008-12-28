-- File created: 2008-11-11 11:24:30

-- The base implementation of a trie representing a map with list keys,
-- generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Trie.Map where

import Control.Applicative ((<|>))
import Control.Arrow       ((***), second)
import Control.Monad       (liftM2)
import qualified Data.DList as DL
import Data.Either         (partitionEithers)
import Data.Maybe          (isJust)
import qualified Data.Maybe as Maybe
import Prelude hiding      (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident), pfail)
#endif

import qualified Data.Trie.Base     as Base
import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Map (Map, OrdMap)

-- Invariant: any (Tr Nothing _) has a Just descendant.
data TrieMap map k v = Tr !(Maybe v) !(CMap map k v)

type CMap map k v = map k (TrieMap map k v)

instance Map map k => Base.Trie TrieMap Maybe map k where
   mkTrie = Tr
   tParts (Tr v m) = (v,m)

-- instances: Eq, Monoid, Foldable, Ord

instance (Map map k, Show k, Show a) => Show (TrieMap map k a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map k, Read k, Read a) => Read (TrieMap map k a) where
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
null :: Map map k => TrieMap map k a -> Bool
null = Base.null

-- O(n). The number of elements in the set.
size :: Map map k => TrieMap map k a -> Int
size = Base.size

-- O(m).
member :: Map map k => [k] -> TrieMap map k a -> Bool
member k m = isJust (lookup k m)

-- O(m)
lookup :: Map map k => [k] -> TrieMap map k a -> Maybe a
lookup = Base.lookup

-- O(?)
isSubmapOf :: (Map map k, Eq a) => TrieMap map k a -> TrieMap map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- O(?)
isSubmapOfBy :: Map map k
             => (a -> b -> Bool) -> TrieMap map k a -> TrieMap map k b -> Bool
isSubmapOfBy f = Base.isSubmapOfBy (liftM2 f)

-- O(?)
isProperSubmapOf :: (Map map k, Eq a)
                 => TrieMap map k a -> TrieMap map k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- O(?)
isProperSubmapOfBy :: Map map k => (a -> b -> Bool)
                                -> TrieMap map k a
                                -> TrieMap map k b
                                -> Bool
isProperSubmapOfBy f = Base.isProperSubmapOfBy (liftM2 f)

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
insertWithKey :: Map map k => ([k] -> a -> a -> a)
                           -> [k] -> a
                           -> TrieMap map k a
                           -> TrieMap map k a
insertWithKey = Base.insertWithKey

-- O(m)
delete :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
delete = Base.delete

-- O(m)
adjust :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust = Base.adjust

-- O(m)
update :: Map map k
       => (a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
update f k t = snd (updateLookup f k t)

-- O(m)
updateLookup :: Map map k => (a -> Maybe a)
                          -> [k]
                          -> TrieMap map k a
                          -> (Maybe a, TrieMap map k a)
updateLookup f = Base.updateLookup (\v -> let v' = v >>= f in (v' <|> v, v'))

-- O(m)
alter :: Map map k
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter = Base.alter

-- * Combination

defaultUnion :: a -> a -> a
defaultUnion = const

-- O(n1+n2)
union :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union = unionWith defaultUnion

-- O(n1+n2)
unionWith :: Map map k => (a -> a -> a)
                       -> TrieMap map k a
                       -> TrieMap map k a
                       -> TrieMap map k a
unionWith = Base.unionWith

-- O(n1+n2)
unionWithKey :: Map map k => ([k] -> a -> a -> a)
                          -> TrieMap map k a
                          -> TrieMap map k a
                          -> TrieMap map k a
unionWithKey = Base.unionWithKey

unions :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions = unionsWith defaultUnion

unionsWith :: Map map k
           => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith = Base.unionsWith

-- O(n1+n2)
difference :: Map map k
           => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
difference = differenceWith (\_ _ -> Nothing)

-- O(n1+n2)
differenceWith :: Map map k => (a -> b -> Maybe a)
                            -> TrieMap map k a
                            -> TrieMap map k b
                            -> TrieMap map k a
differenceWith = Base.differenceWith

-- O(n1+n2)
differenceWithKey :: Map map k => ([k] -> a -> b -> Maybe a)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k a
differenceWithKey = Base.differenceWithKey

-- O(n1+n2)
intersection :: Map map k
             => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection = intersectionWith const

-- O(n1+n2)
intersectionWith :: Map map k => (a -> b -> c)
                              -> TrieMap map k a
                              -> TrieMap map k b
                              -> TrieMap map k c
intersectionWith = Base.intersectionWith

-- O(n1+n2)
intersectionWithKey :: Map map k => ([k] -> a -> b -> c)
                                 -> TrieMap map k a
                                 -> TrieMap map k b
                                 -> TrieMap map k c
intersectionWithKey = Base.intersectionWithKey

-- * Filtering

-- O(n)
filter :: Map map k => (a -> Bool) -> TrieMap map k a -> TrieMap map k a
filter p = filterWithKey (const p)

-- O(n)
filterWithKey :: Map map k
              => ([k] -> a -> Bool) -> TrieMap map k a -> TrieMap map k a
filterWithKey = Base.filterWithKey

-- O(n)
partition :: Map map k => (a -> Bool)
                       -> TrieMap map k a
                       -> (TrieMap map k a, TrieMap map k a)
partition p = partitionWithKey (const p)

-- O(n)
partitionWithKey :: Map map k => ([k] -> a -> Bool)
                              -> TrieMap map k a
                              -> (TrieMap map k a, TrieMap map k a)
partitionWithKey = Base.partitionWithKey

split :: OrdMap map k
      => [k] -> TrieMap map k a -> (TrieMap map k a, TrieMap map k a)
split = Base.split

splitLookup :: OrdMap map k => [k]
                            -> TrieMap map k a
                            -> (TrieMap map k a, Maybe a, TrieMap map k a)
splitLookup = Base.splitLookup

-- O(n)
mapMaybe :: Map map k
         => (a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybe f = mapMaybeWithKey (const f)

-- O(n)
mapMaybeWithKey :: Map map k
                => ([k] -> a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybeWithKey f =
   fromList . Maybe.mapMaybe (\(k,v) -> fmap ((,) k) (f k v)) . toList

-- O(n)
mapEither :: Map map k => (a -> Either b c)
                       -> TrieMap map k a
                       -> (TrieMap map k b, TrieMap map k c)
mapEither f = mapEitherWithKey (const f)

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
map f (Tr v m) = Tr (fmap f v) (Map.map (map f) m)

-- O(n)
mapWithKey :: Map map k
           => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey = go DL.empty
 where
   go k f (Tr v m) = Tr (fmap (f $ DL.toList k) v)
                        (Map.mapWithKey (\x -> go (k `DL.snoc` x) f) m)

-- O(n)
mapAccum :: Map map k => (acc -> a -> (acc, b))
                      -> acc
                      -> TrieMap map k a
                      -> (acc, TrieMap map k b)
mapAccum = genericMapAccum Map.mapAccum

-- O(n)
mapAccumWithKey :: Map map k => (acc -> [k] -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumWithKey = genericMapAccumWithKey Map.mapAccumWithKey

-- O(n)
mapAccumAsc :: OrdMap map k => (acc -> a -> (acc, b))
                            -> acc
                            -> TrieMap map k a
                            -> (acc, TrieMap map k b)
mapAccumAsc = genericMapAccum Map.mapAccumAsc

-- O(n)
mapAccumAscWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                   -> acc
                                   -> TrieMap map k a
                                   -> (acc, TrieMap map k b)
mapAccumAscWithKey = genericMapAccumWithKey Map.mapAccumAscWithKey

-- O(n)
mapAccumDesc :: OrdMap map k => (acc -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumDesc = genericMapAccum Map.mapAccumDesc

-- O(n)
mapAccumDescWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                    -> acc
                                    -> TrieMap map k a
                                    -> (acc, TrieMap map k b)
mapAccumDescWithKey = genericMapAccumWithKey Map.mapAccumDescWithKey

genericMapAccum :: Map map k
                => (  (acc -> TrieMap map k a -> (acc, TrieMap map k b))
                   -> acc
                   -> CMap map k a
                   -> (acc, CMap map k b)
                   )
                -> (acc -> a -> (acc, b))
                -> acc
                -> TrieMap map k a
                -> (acc, TrieMap map k b)
genericMapAccum subMapAccum f acc (Tr mv m) =
   let (acc', v') =
          case mv of
               Nothing -> (acc, Nothing)
               Just v  -> second Just (f acc v)
    in second (Tr v') $ subMapAccum (genericMapAccum subMapAccum f) acc' m

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
                       -> (acc -> [k] -> a -> (acc, b))
                       -> acc
                       -> TrieMap map k a
                       -> (acc, TrieMap map k b)
genericMapAccumWithKey = go DL.empty
 where
   go k subMapAccum f acc (Tr mv m) =
      let (acc', v') =
             case mv of
                  Nothing -> (acc, Nothing)
                  Just v  -> second Just (f acc (DL.toList k) v)
       in second (Tr v') $
             subMapAccum (\a x -> go (k `DL.snoc` x) subMapAccum f a) acc' m

-- O(n)
mapKeys :: (Map map k1, Map map k2)
        => ([k1] -> [k2]) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys = mapKeysWith const

-- O(n)
mapKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                        -> ([k1] -> [k2])
                                        -> TrieMap map k1 a
                                        -> TrieMap map k2 a
mapKeysWith j = Base.mapKeysWith (fromListWith j)

-- O(n)
-- TODO: needs a name!
mapKeys' :: (Map map k1, Map map k2)
         => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys' = mapKeys'With defaultUnion

-- O(n)
-- TODO: needs a name!
mapKeys'With :: (Map map k1, Map map k2)
             => (a -> a -> a)
             -> (k1 -> k2)
             -> TrieMap map k1 a
             -> TrieMap map k2 a
mapKeys'With = Base.mapKeys'With

-- * Folding

-- O(n)
fold :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
fold f = foldWithKey (const f)

-- O(n)
foldWithKey :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldWithKey = Base.foldWithKey

-- O(n)
foldAsc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldAsc f = foldAscWithKey (const f)

-- O(n)
foldAscWithKey :: OrdMap map k
               => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldAscWithKey = Base.foldAscWithKey

-- O(n)
foldDesc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldDesc f = foldDescWithKey (const f)

-- O(n)
foldDescWithKey :: OrdMap map k
                => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldDescWithKey = Base.foldDescWithKey

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

-- O(n*m)
fromList :: Map map k => [([k],a)] -> TrieMap map k a
fromList = Base.fromList

-- O(n*m)
fromListWith :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith = Base.fromListWith

-- O(n*m)
fromListWithKey :: Map map k
                => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey = Base.fromListWithKey

-- * Min/max

-- O(m log b)
findMin :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMin = Base.findMin

-- O(m log b)
findMax :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMax = Base.findMax

-- O(m log b)
deleteMin :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMin = Base.deleteMin

-- O(m log b)
deleteMax :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMax = Base.deleteMax

-- O(m log b)
minView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
minView = Base.minView

-- O(m log b)
maxView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
maxView = Base.maxView

-- O(m b)
findPredecessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findPredecessor = Base.findPredecessor

-- O(m b)
findSuccessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findSuccessor = Base.findSuccessor
