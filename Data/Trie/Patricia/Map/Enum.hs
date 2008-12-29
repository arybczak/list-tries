-- File created: 2008-12-29 12:42:12

-- A map from lists of enumerable elements, based on a Patricia trie.
--
-- Note that those operations which require an ordering, such as 'toAscList',
-- do not compare the elements themselves, but rather their Int representation
-- after 'fromEnum'.
--
-- Complexities are given; @n@ refers to the number of elements in the map, @m@
-- to their maximum length, @b@ to the trie's branching factor.

module Data.Trie.Patricia.Map.Enum where

import qualified Data.Trie.Base.Map     as Map
import qualified Data.Trie.Patricia.Map as Base

type TrieMap = Base.TrieMap Map.IMap

-- * Querying

-- O(1)
null :: Enum k => TrieMap k a -> Bool
null = Base.null

-- O(n). The number of elements in the map.
size :: Enum k => TrieMap k a -> Int
size = Base.size

-- O(m).
member :: Enum k => [k] -> TrieMap k a -> Bool
member = Base.member

-- O(m)
lookup :: Enum k => [k] -> TrieMap k a -> Maybe a
lookup = Base.lookup

-- O(?)
isSubmapOf :: (Enum k, Eq a) => TrieMap k a -> TrieMap k a -> Bool
isSubmapOf = Base.isSubmapOf

-- O(?)
isSubmapOfBy :: Enum k
             => (a -> b -> Bool) -> TrieMap k a -> TrieMap k b -> Bool
isSubmapOfBy = Base.isSubmapOfBy

-- O(?)
isProperSubmapOf :: (Enum k, Eq a) => TrieMap k a -> TrieMap k a -> Bool
isProperSubmapOf = Base.isProperSubmapOf

-- O(?)
isProperSubmapOfBy :: Enum k
                   => (a -> b -> Bool) -> TrieMap k a -> TrieMap k b -> Bool
isProperSubmapOfBy = Base.isProperSubmapOfBy

-- * Construction

-- O(1)
empty :: Enum k => TrieMap k a
empty = Base.empty

-- O(1)
singleton :: Enum k => [k] -> a -> TrieMap k a
singleton = Base.singleton

-- O(m)
insert :: Enum k => [k] -> a -> TrieMap k a -> TrieMap k a
insert = Base.insert

-- O(m)
insertWith :: Enum k => (a -> a -> a) -> [k] -> a -> TrieMap k a -> TrieMap k a
insertWith = Base.insertWith

-- O(m)
insertWithKey :: Enum k
              => ([k] -> a -> a -> a) -> [k] -> a -> TrieMap k a -> TrieMap k a
insertWithKey = Base.insertWithKey

-- O(m)
delete :: Enum k => [k] -> TrieMap k a -> TrieMap k a
delete = Base.delete

-- O(m)
adjust :: Enum k => (a -> a) -> [k] -> TrieMap k a -> TrieMap k a
adjust = Base.adjust

-- O(m)
update :: Enum k
       => (a -> Maybe a) -> [k] -> TrieMap k a -> TrieMap k a
update = Base.update

-- O(m)
updateLookup :: Enum k
             => (a -> Maybe a) -> [k] -> TrieMap k a -> (Maybe a, TrieMap k a)
updateLookup = Base.updateLookup

-- * Combination

union :: Enum k => TrieMap k a -> TrieMap k a -> TrieMap k a
union = Base.union

unionWith :: Enum k
          => (a -> a -> a) -> TrieMap k a -> TrieMap k a -> TrieMap k a
unionWith = Base.unionWith

unionWithKey :: Enum k => ([k] -> a -> a -> a)
                       -> TrieMap k a
                       -> TrieMap k a
                       -> TrieMap k a
unionWithKey = Base.unionWithKey

unions :: Enum k => [TrieMap k a] -> TrieMap k a
unions = Base.unions

unionsWith :: Enum k
           => (a -> a -> a) -> [TrieMap k a] -> TrieMap k a
unionsWith = Base.unionsWith

difference :: Enum k
           => TrieMap k a -> TrieMap k a -> TrieMap k a
difference = Base.difference

differenceWith :: Enum k => (a -> b -> Maybe a)
                         -> TrieMap k a
                         -> TrieMap k b
                         -> TrieMap k a
differenceWith = Base.differenceWith

differenceWithKey :: Enum k => ([k] -> a -> b -> Maybe a)
                            -> TrieMap k a
                            -> TrieMap k b
                            -> TrieMap k a
differenceWithKey = Base.differenceWithKey

intersection :: Enum k
             => TrieMap k a -> TrieMap k a -> TrieMap k a
intersection = Base.intersection

intersectionWith :: Enum k => (a -> b -> c)
                           -> TrieMap k a
                           -> TrieMap k b
                           -> TrieMap k c
intersectionWith = Base.intersectionWith

intersectionWithKey :: Enum k => ([k] -> a -> b -> c)
                              -> TrieMap k a
                              -> TrieMap k b
                              -> TrieMap k c
intersectionWithKey = Base.intersectionWithKey

-- * Filtering

-- O(n)
filter :: Enum k => (a -> Bool) -> TrieMap k a -> TrieMap k a
filter = Base.filter

-- O(n)
filterWithKey :: Enum k
              => ([k] -> a -> Bool) -> TrieMap k a -> TrieMap k a
filterWithKey = Base.filterWithKey

-- O(n)
partition :: Enum k
          => (a -> Bool) -> TrieMap k a -> (TrieMap k a, TrieMap k a)
partition = Base.partition

-- O(n)
partitionWithKey :: Enum k => ([k] -> a -> Bool)
                           -> TrieMap k a
                           -> (TrieMap k a, TrieMap k a)
partitionWithKey = Base.partitionWithKey

split :: Enum k => [k] -> TrieMap k a -> (TrieMap k a, TrieMap k a)
split = Base.split

splitLookup :: Enum k
            => [k] -> TrieMap k a -> (TrieMap k a, Maybe a, TrieMap k a)
splitLookup = Base.splitLookup

-- O(n)
mapMaybe :: Enum k => (a -> Maybe b) -> TrieMap k a -> TrieMap k b
mapMaybe = Base.mapMaybe

-- O(n)
mapMaybeWithKey :: Enum k
                => ([k] -> a -> Maybe b) -> TrieMap k a -> TrieMap k b
mapMaybeWithKey = Base.mapMaybeWithKey

-- O(n)
mapEither :: Enum k
          => (a -> Either b c) -> TrieMap k a -> (TrieMap k b, TrieMap k c)
mapEither = Base.mapEither

-- O(n)
mapEitherWithKey :: Enum k => ([k] -> a -> Either b c)
                           -> TrieMap k a
                           -> (TrieMap k b, TrieMap k c)
mapEitherWithKey = Base.mapEitherWithKey

-- * Mapping

-- O(n)
map :: Enum k => (a -> b) -> TrieMap k a -> TrieMap k b
map = Base.map

-- O(n)
mapWithKey :: (Enum k)
           => ([k] -> a -> b) -> TrieMap k a -> TrieMap k b
mapWithKey = Base.mapWithKey

-- O(n)
mapAccum :: Enum k => (acc -> a -> (acc, b))
                   -> acc
                   -> TrieMap k a
                   -> (acc, TrieMap k b)
mapAccum = Base.mapAccum

-- O(n)
mapAccumWithKey :: Enum k => (acc -> [k] -> a -> (acc, b))
                          -> acc
                          -> TrieMap k a
                          -> (acc, TrieMap k b)
mapAccumWithKey = Base.mapAccumWithKey

-- O(n)
mapAccumAsc :: Enum k => (acc -> a -> (acc, b))
                      -> acc
                      -> TrieMap k a
                      -> (acc, TrieMap k b)
mapAccumAsc = Base.mapAccumAsc

-- O(n)
mapAccumAscWithKey :: Enum k => (acc -> [k] -> a -> (acc, b))
                             -> acc
                             -> TrieMap k a
                             -> (acc, TrieMap k b)
mapAccumAscWithKey = Base.mapAccumAscWithKey

-- O(n)
mapAccumDesc :: Enum k => (acc -> a -> (acc, b))
                       -> acc
                       -> TrieMap k a
                       -> (acc, TrieMap k b)
mapAccumDesc = Base.mapAccumDesc

-- O(n)
mapAccumDescWithKey :: Enum k => (acc -> [k] -> a -> (acc, b))
                              -> acc
                              -> TrieMap k a
                              -> (acc, TrieMap k b)
mapAccumDescWithKey = Base.mapAccumDescWithKey

-- O(n)
mapKeys :: (Enum k1, Enum k2)
        => ([k1] -> [k2]) -> TrieMap k1 a -> TrieMap k2 a
mapKeys = Base.mapKeys

-- O(n)
mapKeysWith :: (Enum k1, Enum k2)
            => (a -> a -> a) -> ([k1] -> [k2]) -> TrieMap k1 a -> TrieMap k2 a
mapKeysWith = Base.mapKeysWith

-- O(n)
-- needs a name!
mapKeys' :: (Enum k1, Enum k2)
         => (k1 -> k2) -> TrieMap k1 a -> TrieMap k2 a
mapKeys' = Base.mapKeys'

-- O(n)
-- TODO: needs a name!
mapKeys'With :: (Enum k1, Enum k2)
             => (a -> a -> a) -> (k1 -> k2) -> TrieMap k1 a -> TrieMap k2 a
mapKeys'With = Base.mapKeys'With

-- * Folding

-- O(n)
fold :: Enum k => (a -> b -> b) -> b -> TrieMap k a -> b
fold = Base.fold

-- O(n)
foldWithKey :: Enum k => ([k] -> a -> b -> b) -> b -> TrieMap k a -> b
foldWithKey = Base.foldWithKey

-- O(n)
foldAsc :: Enum k => (a -> b -> b) -> b -> TrieMap k a -> b
foldAsc = Base.foldAsc

-- O(n)
foldAscWithKey :: Enum k
               => ([k] -> a -> b -> b) -> b -> TrieMap k a -> b
foldAscWithKey = Base.foldAscWithKey

-- O(n)
foldDesc :: Enum k => (a -> b -> b) -> b -> TrieMap k a -> b
foldDesc = Base.foldDesc

-- O(n)
foldDescWithKey :: Enum k
                => ([k] -> a -> b -> b) -> b -> TrieMap k a -> b
foldDescWithKey = Base.foldDescWithKey

-- * Conversion between lists

-- O(n)
toList :: Enum k => TrieMap k a -> [([k],a)]
toList = Base.toList

-- O(n)
toAscList :: Enum k => TrieMap k a -> [([k],a)]
toAscList = Base.toAscList

-- O(n)
toDescList :: Enum k => TrieMap k a -> [([k],a)]
toDescList = Base.toDescList

-- O(n)
fromList :: Enum k => [([k],a)] -> TrieMap k a
fromList = Base.fromList

-- * Min/max

-- O(m log b)
findMin :: Enum k => TrieMap k a -> Maybe ([k], a)
findMin = Base.findMin

-- O(m log b)
findMax :: Enum k => TrieMap k a -> Maybe ([k], a)
findMax = Base.findMax

-- O(m log b)
deleteMin :: Enum k => TrieMap k a -> TrieMap k a
deleteMin = Base.deleteMin

-- O(m log b)
deleteMax :: Enum k => TrieMap k a -> TrieMap k a
deleteMax = Base.deleteMax

-- O(m log b)
minView :: Enum k => TrieMap k a -> Maybe (([k], a), TrieMap k a)
minView = Base.minView

-- O(m log b)
maxView :: Enum k => TrieMap k a -> Maybe (([k], a), TrieMap k a)
maxView = Base.maxView

-- O(m b)
findPredecessor :: Enum k => TrieMap k a -> [k] -> Maybe ([k], a)
findPredecessor = Base.findPredecessor

-- O(m b)
findSuccessor :: Enum k => TrieMap k a -> [k] -> Maybe ([k], a)
findSuccessor = Base.findSuccessor
