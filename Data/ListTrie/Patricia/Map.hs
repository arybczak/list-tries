-- File created: 2008-11-12 14:16:48

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances
           , FlexibleContexts, UndecidableInstances #-}

#include "exports.h"

-- | The base implementation of a Patricia trie representing a map with list
-- keys, generalized over any type of map from element values to tries.
--
-- Worst-case complexities are given in terms of @n@, @m@, and @s@. @n@ refers
-- to the number of keys in the map and @m@ to their maximum length. @s@ refers
-- to the length of a key given to the function, not any property of the map.
--
-- In addition, the trie's branching factor plays a part in almost every
-- operation, but the complexity depends on the underlying 'Map'. Thus, for
-- instance, 'member' is actually @O(m f(b))@ where @f(b)@ is the complexity of
-- a lookup operation on the 'Map' used. This complexity depends on the
-- underlying operation, which is not part of the specification of the visible
-- function. Thus it could change whilst affecting the complexity only for
-- certain Map types: hence this \"b factor\" is not shown explicitly.
--
-- Disclaimer: the complexities have not been proven.
--
-- Strict versions of functions are provided for those who want to be certain
-- that their 'TrieMap' doesn't contain values consisting of unevaluated
-- thunks. Note, however, that they do not evaluate the whole trie strictly,
-- only the values. And only to one level of depth: for instance, 'alter'' does
-- not 'seq' the value within the 'Maybe', only the 'Maybe' itself. The user
-- should add the strictness in such cases himself, if he so wishes.
--
-- Many functions come in both ordinary and @WithKey@ forms, where the former
-- takes a function of type @a -> b@ and the latter of type @[k] -> a -> b@,
-- where @[k]@ is the key associated with the value @a@. For most of these
-- functions, there is additional overhead involved in keeping track of the
-- key: don't use the latter form of the function unless you need it.
module Data.ListTrie.Patricia.Map (MAP_EXPORTS) where

import Control.Arrow       ((***), second)
import Control.Monad       (liftM3)
import Data.Binary         (Binary,get,put)
import qualified Data.DList as DL
import Data.Either         (partitionEithers)
import Data.Function       (on)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import Data.Semigroup      (Semigroup(..), stimesIdempotent)
import Prelude hiding      (filter, foldl, foldl', foldr, lookup, map, null)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident))
#endif

import qualified Data.ListTrie.Base.Map      as Map
import qualified Data.ListTrie.Patricia.Base as Base
import Data.ListTrie.Base.Classes (fmap')
import Data.ListTrie.Base.Map     (Map, OrdMap)

#include "docs.h"

-- Invariant: any (Tr Nothing _ _) has at least two children, all of which are
-- Just or have a Just descendant.
--
-- In order to avoid a lot of special casing it has to be the case that there's
-- only one way to represent a given trie. The above property makes sure of
-- that, so that, for instance, 'fromList [("foo",1)]' can only be 'Tr (Just 1)
-- "foo" Map.empty', and not 'Tr Nothing "fo" (Map.fromList [('o',Tr (Just 1)
-- "" Map.empty)])'. Base.tryCompress is a function which takes care of this.
--
-- | The data structure itself: a map from keys of type @[k]@ to values of type
-- @v@ implemented as a trie, using @map@ to map keys of type @k@ to sub-tries.
--
-- Regarding the instances:
--
-- - The @Trie@ class is internal, ignore it.
--
-- - The 'Eq' constraint for the 'Ord' instance is misleading: it is needed
--   only because 'Eq' is a superclass of 'Ord'.
--
-- - The 'Foldable' and 'Traversable' instances allow folding over and
--   traversing only the values, not the keys.
--
-- - The 'Monoid' instance defines 'mappend' as 'union' and 'mempty' as
--   'empty'.
data TrieMap map k v = Tr (Maybe v) ![k] !(CMap map k v)

type CMap map k v = map k (TrieMap map k v)

instance Map map k => Base.Trie TrieMap Maybe map k where
   mkTrie = Tr
   tParts (Tr v p m) = (v,p,m)

-- Don't use CMap in these instances since Haddock won't expand it
instance (Map map k, Eq (map k (TrieMap map k a)), Eq a)
      => Eq (TrieMap map k a)
 where
   Tr v1 p1 m1 == Tr v2 p2 m2 =
      v1 == v2 && Base.eqComparePrefixes (Map.eqCmp m1) p1 p2
               && m1 == m2

-- Eq constraint only needed because of superclassness... sigh
instance (Eq (map k (TrieMap map k a)), OrdMap map k, Ord k, Ord a)
      => Ord (TrieMap map k a)
 where
   compare = compare `on` toAscList

instance Map map k => Semigroup (TrieMap map k a) where
   (<>) = union
   sconcat = unions . NE.toList
   stimes = stimesIdempotent

instance Map map k => Monoid (TrieMap map k a) where
   mempty  = empty
   mappend = (<>)
   mconcat = unions

instance Map map k => Functor (TrieMap map k) where
   fmap = map

instance Map map k => F.Foldable (TrieMap map k) where
   foldl = foldl . flip
   foldr = foldr

instance (Map map k, Traversable (map k)) => Traversable (TrieMap map k) where
   traverse f (Tr v p m) =
      flip Tr p <$> traverse f v <*> traverse (traverse f) m

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

instance (Map map k, Binary k, Binary a) => Binary (TrieMap map k a) where
   put (Tr v k m) = put v >> put k >> (put . Map.serializeToList $ m)
   get = liftM3 Tr get get (get >>= return . Map.deserializeFromList)

-- * Construction

-- | @O(1)@. The empty map.
empty :: Map map k => TrieMap map k a
empty = Base.empty

-- | @O(1)@. The singleton map containing only the given key-value pair.
singleton :: Map map k => [k] -> a -> TrieMap map k a
singleton = Base.singleton

-- * Modification

-- | @O(min(m,s))@. Inserts the key-value pair into the map. If the key is
-- already a member of the map, the given value replaces the old one.
--
-- > insert = insertWith const
insert :: Map map k => [k] -> a -> TrieMap map k a -> TrieMap map k a
insert = Base.insert

-- | @O(min(m,s))@. Like 'insert', but the new value is reduced to weak head
-- normal form before being placed into the map.
--
-- > insert' = insertWith' const
insert' :: Map map k => [k] -> a -> TrieMap map k a -> TrieMap map k a
insert' = Base.insert'

-- | @O(min(m,s))@. Inserts the key-value pair into the map. If the key is
-- already a member of the map, the old value is replaced by @f givenValue
-- oldValue@ where @f@ is the given function.
insertWith :: Map map k
           => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith = Base.insertWith

-- | @O(min(m,s))@. Like 'insertWith', but the new value is reduced to weak
-- head normal form before being placed into the map, whether it is the given
-- value or a result of the combining function.
insertWith' :: Map map k
            => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith' = Base.insertWith'

-- | @O(min(m,s))@. Removes the key from the map along with its associated
-- value. If the key is not a member of the map, the map is unchanged.
delete :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
delete = Base.delete

-- | @O(min(m,s))@. Adjusts the value at the given key by calling the given
-- function on it. If the key is not a member of the map, the map is unchanged.
adjust :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust = Base.adjust

-- | @O(min(m,s))@. Like 'adjust', but the function is applied strictly.
adjust' :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust' = Base.adjust'

-- | @O(min(m,s))@. Updates the value at the given key: if the given
-- function returns 'Nothing', the value and its associated key are removed; if
-- 'Just'@ a@ is returned, the old value is replaced with @a@. If the key is
-- not a member of the map, the map is unchanged.
update :: Map map k
       => (a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
update f k = snd . updateLookup f k

-- | @O(min(m,s))@. Like 'update', but also returns 'Just' the original value,
-- or 'Nothing' if the key is not a member of the map.
updateLookup :: Map map k => (a -> Maybe a)
                          -> [k]
                          -> TrieMap map k a
                          -> (Maybe a, TrieMap map k a)
updateLookup = Base.updateLookup

-- | @O(min(m,s))@. The most general modification function, allowing you to
-- modify the value at the given key, whether or not it is a member of the map.
-- In short: the given function is passed 'Just' the value at the key if it is
-- present, or 'Nothing' otherwise; if the function returns 'Just' a value, the
-- new value is inserted into the map, otherwise the old value is removed. More
-- precisely, for @alter f k m@:
--
-- If @k@ is a member of @m@, @f (@'Just'@ oldValue)@ is called. Now:
--
-- - If @f@ returned 'Just'@ newValue@, @oldValue@ is replaced with @newValue@.
--
-- - If @f@ returned 'Nothing', @k@ and @oldValue@ are removed from the map.
--
-- If, instead, @k@ is not a member of @m@, @f @'Nothing' is called, and:
--
-- - If @f@ returned 'Just'@ value@, @value@ is inserted into the map, at @k@.
--
-- - If @f@ returned 'Nothing', the map is unchanged.
--
-- The function is applied lazily only if the given key is a prefix of more
-- than one key in the map.
alter :: Map map k
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter = Base.alter

-- | @O(min(m,s))@. Like 'alter', but the function is always applied strictly.
alter' :: Map map k
       => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter' = Base.alter'

-- * Querying

-- | @O(1)@. 'True' iff the map is empty.
null :: Map map k => TrieMap map k a -> Bool
null = Base.null

-- | @O(n m)@. The number of elements in the map. The value is built up lazily,
-- allowing for delivery of partial results without traversing the whole map.
size :: (Map map k, Num n) => TrieMap map k a -> n
size = Base.size

-- | @O(n m)@. The number of elements in the map. The value is built strictly:
-- no value is returned until the map has been fully traversed.
size' :: (Map map k, Num n) => TrieMap map k a -> n
size' = Base.size'

-- | @O(min(m,s))@. 'True' iff the given key is associated with a value in the
-- map.
member :: Map map k => [k] -> TrieMap map k a -> Bool
member = Base.member

-- | @O(min(m,s))@. 'False' iff the given key is associated with a value in the
-- map.
notMember :: Map map k => [k] -> TrieMap map k a -> Bool
notMember = Base.notMember

-- | @O(min(m,s))@. 'Just' the value in the map associated with the given key,
-- or 'Nothing' if the key is not a member of the map.
lookup :: Map map k => [k] -> TrieMap map k a -> Maybe a
lookup = Base.lookup

-- | @O(min(m,s))@. Like 'lookup', but returns the given value when the key is
-- not a member of the map.
lookupWithDefault :: Map map k => a -> [k] -> TrieMap map k a -> a
lookupWithDefault = Base.lookupWithDefault

-- | @O(min(n1 m1,n2 m2))@. 'True' iff the first map is a submap of the second,
-- i.e. all keys that are members of the first map are also members of the
-- second map, and their associated values are the same.
--
-- > isSubmapOf = isSubmapOfBy (==)
isSubmapOf :: (Map map k, Eq a) => TrieMap map k a -> TrieMap map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- | @O(min(n1 m1,n2 m2))@. Like 'isSubmapOf', but one can specify the equality
-- relation applied to the values.
--
-- 'True' iff all keys that are members of the first map are also members of
-- the second map, and the given function @f@ returns 'True' for all @f
-- firstMapValue secondMapValue@ where @firstMapValue@ and @secondMapValue@ are
-- associated with the same key.
isSubmapOfBy :: Map map k
             => (a -> b -> Bool) -> TrieMap map k a -> TrieMap map k b -> Bool
isSubmapOfBy = Base.isSubmapOfBy

-- | @O(min(n1 m1,n2 m2))@. 'True' iff the first map is a proper submap of the
-- second, i.e. all keys that are members of the first map are also members of
-- the second map, and their associated values are the same, but the maps are
-- not equal. That is, at least one key was a member of the second map but not
-- the first.
--
-- > isProperSubmapOf = isProperSubmapOfBy (==)
isProperSubmapOf :: (Map map k, Eq a)
                 => TrieMap map k a -> TrieMap map k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- | @O(min(n1 m1,n2 m2))@. Like 'isProperSubmapOf', but one can specify the
-- equality relation applied to the values.
--
-- 'True' iff all keys that are members of the first map are also members of
-- the second map, and the given function @f@ returns 'True' for all @f
-- firstMapValue secondMapValue@ where @firstMapValue@ and @secondMapValue@ are
-- associated with the same key, and at least one key in the second map is not
-- a member of the first.
isProperSubmapOfBy :: Map map k => (a -> b -> Bool)
                                -> TrieMap map k a
                                -> TrieMap map k b
                                -> Bool
isProperSubmapOfBy = Base.isProperSubmapOfBy

-- * Combination

defaultUnion :: a -> a -> a
defaultUnion = const

-- | @O(min(n1 m1,n2 m2))@. The union of the two maps: the map which contains
-- all keys that are members of either map. This union is left-biased: if a key
-- is a member of both maps, the value from the first map is chosen.
--
-- The worst-case performance occurs when the two maps are identical.
--
-- > union = unionWith const
union :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union = unionWith defaultUnion

-- | @O(min(n1 m1,n2 m2))@. Like 'union', but the combining function ('const')
-- is applied strictly.
--
-- > union' = unionWith' const
union' :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union' = unionWith' defaultUnion

-- | @O(min(n1 m1,n2 m2))@. Like 'union', but the given function is used to
-- determine the new value if a key is a member of both given maps. For a
-- function @f@, the new value is @f firstMapValue secondMapValue@.
unionWith :: Map map k => (a -> a -> a)
                       -> TrieMap map k a
                       -> TrieMap map k a
                       -> TrieMap map k a
unionWith = Base.unionWith

-- | @O(min(n1 m1,n2 m2))@. Like 'unionWith', but the combining function is
-- applied strictly.
unionWith' :: Map map k => (a -> a -> a)
                        -> TrieMap map k a
                        -> TrieMap map k a
                        -> TrieMap map k a
unionWith' = Base.unionWith'

-- | @O(min(n1 m1,n2 m2))@. Like 'unionWith', but in addition to the two
-- values, the key is passed to the combining function.
unionWithKey :: Map map k => ([k] -> a -> a -> a)
                          -> TrieMap map k a
                          -> TrieMap map k a
                          -> TrieMap map k a
unionWithKey = Base.unionWithKey

-- | @O(min(n1 m1,n2 m2))@. Like 'unionWithKey', but the combining function is
-- applied strictly.
unionWithKey' :: Map map k => ([k] -> a -> a -> a)
                           -> TrieMap map k a
                           -> TrieMap map k a
                           -> TrieMap map k a
unionWithKey' = Base.unionWithKey'

-- | @O(sum(n))@. The union of all the maps: the map which contains all keys
-- that are members of any of the maps. If a key is a member of multiple maps,
-- the value that occurs in the earliest of the maps (according to the order of
-- the given list) is chosen.
--
-- The worst-case performance occurs when all the maps are identical.
--
-- > unions = unionsWith const
unions :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions = unionsWith defaultUnion

-- | @O(sum(n))@. Like 'unions', but the combining function ('const') is
-- applied strictly.
--
-- > unions' = unionsWith' const
unions' :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions' = unionsWith' defaultUnion

-- | @O(sum(n))@. Like 'unions', but the given function determines the final
-- value if a key is a member of more than one map. The function is applied as
-- a left fold over the values in the given list's order. For example:
--
-- > unionsWith (-) [fromList [("a",1)],fromList [("a",2)],fromList [("a",3)]]
-- >    == fromList [("a",(1-2)-3)]
-- >    == fromList [("a",-4)]
unionsWith :: Map map k
           => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith = Base.unionsWith

-- | @O(sum(n))@. Like 'unionsWith', but the combining function is applied
-- strictly.
unionsWith' :: Map map k
            => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith' = Base.unionsWith'

-- | @O(sum(n))@. Like 'unionsWith', but in addition to the two values under
-- consideration, the key is passed to the combining function.
unionsWithKey :: Map map k
              => ([k] -> a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWithKey = Base.unionsWithKey

-- | @O(sum(n))@. Like 'unionsWithKey', but the combining function is applied
-- strictly.
unionsWithKey' :: Map map k
               => ([k] -> a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWithKey' = Base.unionsWithKey'

-- | @O(min(n1 m1,n2 m2))@. The difference of the two maps: the map which
-- contains all keys that are members of the first map and not of the second.
--
-- The worst-case performance occurs when the two maps are identical.
--
-- > difference = differenceWith (\_ _ -> Nothing)
difference :: Map map k
           => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
difference = differenceWith (\_ _ -> Nothing)

-- | @O(min(n1 m1,n2 m2))@. Like 'difference', but the given function
-- determines what to do when a key is a member of both maps. If the function
-- returns 'Nothing', the key is removed; if it returns 'Just' a new value,
-- that value replaces the old one in the first map.
differenceWith :: Map map k => (a -> b -> Maybe a)
                            -> TrieMap map k a
                            -> TrieMap map k b
                            -> TrieMap map k a
differenceWith = Base.differenceWith

-- | @O(min(n1 m1,n2 m2))@. Like 'differenceWith', but in addition to the two
-- values, the key they are associated with is passed to the combining
-- function.
differenceWithKey :: Map map k => ([k] -> a -> b -> Maybe a)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k a
differenceWithKey = Base.differenceWithKey

-- | @O(min(n1 m1,n2 m2))@. The intersection of the two maps: the map which
-- contains all keys that are members of both maps.
--
-- The worst-case performance occurs when the two maps are identical.
--
-- > intersection = intersectionWith const
intersection :: Map map k
             => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection = intersectionWith const

-- | @O(min(n1 m1,n2 m2))@. Like 'intersection', but the combining function is
-- applied strictly.
--
-- > intersection' = intersectionWith' const
intersection' :: Map map k
              => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection' = intersectionWith' const

-- | @O(min(n1 m1,n2 m2))@. Like 'intersection', but the given function
-- determines the new values.
intersectionWith :: Map map k => (a -> b -> c)
                              -> TrieMap map k a
                              -> TrieMap map k b
                              -> TrieMap map k c
intersectionWith = Base.intersectionWith

-- | @O(min(n1 m1,n2 m2))@. Like 'intersectionWith', but the combining function
-- is applied strictly.
intersectionWith' :: Map map k => (a -> b -> c)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k c
intersectionWith' = Base.intersectionWith'

-- | @O(min(n1 m1,n2 m2))@. Like 'intersectionWith', but in addition to the two
-- values, the key they are associated with is passed to the combining
-- function.
intersectionWithKey :: Map map k => ([k] -> a -> b -> c)
                                 -> TrieMap map k a
                                 -> TrieMap map k b
                                 -> TrieMap map k c
intersectionWithKey = Base.intersectionWithKey

-- | @O(min(n1 m1,n2 m2))@. Like 'intersectionWithKey', but the combining
-- function is applied strictly.
intersectionWithKey' :: Map map k => ([k] -> a -> b -> c)
                                  -> TrieMap map k a
                                  -> TrieMap map k b
                                  -> TrieMap map k c
intersectionWithKey' = Base.intersectionWithKey'

-- * Filtering

-- | @O(n m)@. Apply the given function to the elements in the map, discarding
-- those for which the function returns 'False'.
filter :: Map map k => (a -> Bool) -> TrieMap map k a -> TrieMap map k a
filter = filterWithKey . const

-- | @O(n m)@. Like 'filter', but the key associated with the element is also
-- passed to the given predicate.
filterWithKey :: Map map k
              => ([k] -> a -> Bool) -> TrieMap map k a -> TrieMap map k a
filterWithKey = Base.filterWithKey

-- | @O(n m)@. A pair of maps: the first element contains those values for
-- which the given predicate returns 'True', and the second contains those for
-- which it was 'False'.
partition :: Map map k => (a -> Bool)
                       -> TrieMap map k a
                       -> (TrieMap map k a, TrieMap map k a)
partition = partitionWithKey . const

-- | @O(n m)@. Like 'partition', but the key associated with the element is
-- also passed to the given predicate.
partitionWithKey :: Map map k => ([k] -> a -> Bool)
                              -> TrieMap map k a
                              -> (TrieMap map k a, TrieMap map k a)
partitionWithKey = Base.partitionWithKey

-- | @O(n m)@. Apply the given function to the elements in the map, preserving
-- only the 'Just' results.
mapMaybe :: Map map k
         => (a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybe = mapMaybeWithKey . const

-- | @O(n m)@. Like 'mapMaybe', but the key associated with the element is also
-- passed to the given function.
mapMaybeWithKey :: Map map k
                => ([k] -> a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybeWithKey f =
   fromList . Maybe.mapMaybe (\(k,v) -> fmap ((,) k) (f k v)) . toList

-- | @O(n m)@. Apply the given function to the elements in the map, separating
-- the 'Left' results from the 'Right'. The first element of the pair contains
-- the former results, and the second the latter.
mapEither :: Map map k => (a -> Either b c)
                       -> TrieMap map k a
                       -> (TrieMap map k b, TrieMap map k c)
mapEither = mapEitherWithKey . const

-- | @O(n m)@. Like 'mapEither', but the key associated with the element is
-- also passed to the given function.
mapEitherWithKey :: Map map k => ([k] -> a -> Either b c)
                              -> TrieMap map k a
                              -> (TrieMap map k b, TrieMap map k c)
mapEitherWithKey f =
   (fromList *** fromList) . partitionEithers .
   Prelude.map (\(k,v) -> either (Left . (,) k) (Right . (,) k) (f k v)) .
   toList

-- * Mapping

-- | @O(n m)@. Apply the given function to all the elements in the map.
map :: Map map k => (a -> b) -> TrieMap map k a -> TrieMap map k b
map = genericMap fmap

-- | @O(n m)@. Like 'map', but apply the function strictly.
map' :: Map map k => (a -> b) -> TrieMap map k a -> TrieMap map k b
map' = genericMap fmap'

genericMap :: Map map k => ((a -> b) -> Maybe a -> Maybe b)
                        -> (a -> b) -> TrieMap map k a -> TrieMap map k b
genericMap myFmap f (Tr v p m) = Tr (myFmap f v) p
                                    (Map.map (genericMap myFmap f) m)

-- | @O(n m)@. Like 'map', but also pass the key associated with the element to
-- the given function.
mapWithKey :: Map map k
           => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey = genericMapWithKey fmap

-- | @O(n m)@. Like 'mapWithKey', but apply the function strictly.
mapWithKey' :: Map map k
            => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey' = genericMapWithKey fmap'

genericMapWithKey :: Map map k
                  => ((a -> b) -> Maybe a -> Maybe b)
                  -> ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
genericMapWithKey = go DL.empty
 where
   go k myFmap f (Tr v p m) =
      let k' = k `DL.append` DL.fromList p
       in Tr (myFmap (f $ DL.toList k') v)
             p
             (Map.mapWithKey (\x -> go (k' `DL.snoc` x) myFmap f) m)

-- | @O(n m)@. Apply the given function to all the keys in a map.
--
-- > mapKeys = mapKeysWith const
mapKeys :: (Map map k1, Map map k2)
        => ([k1] -> [k2]) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys = mapKeysWith const

-- | @O(n m)@. Like 'mapKeys', but use the first given function to combine
-- elements if the second function gives two keys the same value.
mapKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                        -> ([k1] -> [k2])
                                        -> TrieMap map k1 a
                                        -> TrieMap map k2 a
mapKeysWith = Base.mapKeysWith . fromListWith

-- | @O(n m)@. Apply the given function to the contents of all the keys in the
-- map.
--
-- > mapInKeys = mapInKeysWith const
mapInKeys :: (Map map k1, Map map k2)
          => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapInKeys = mapInKeysWith defaultUnion

-- | @O(n m)@. Like 'mapInKeys', but combine identical keys strictly.
--
-- > mapInKeys' = mapInKeysWith' const
mapInKeys' :: (Map map k1, Map map k2)
           => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapInKeys' = mapInKeysWith' defaultUnion

-- | @O(n m)@. Like 'mapInKeys', but use the first given function to combine
-- elements if the second function gives two keys the same value.
mapInKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                          -> (k1 -> k2)
                                          -> TrieMap map k1 a
                                          -> TrieMap map k2 a
mapInKeysWith = Base.mapInKeysWith

-- | @O(n m)@. Like 'mapInKeysWith', but apply the combining function strictly.
mapInKeysWith' :: (Map map k1, Map map k2) => (a -> a -> a)
                                           -> (k1 -> k2)
                                           -> TrieMap map k1 a
                                           -> TrieMap map k2 a
mapInKeysWith' = Base.mapInKeysWith'

-- | @O(n m)@. Like "Data.List".@mapAccumL@ on the 'toList' representation.
--
-- Essentially a combination of 'map' and 'foldl': the given
-- function is applied to each element of the map, resulting in a new value for
-- the accumulator and a replacement element for the map.
mapAccum :: Map map k => (acc -> a -> (acc, b))
                      -> acc
                      -> TrieMap map k a
                      -> (acc, TrieMap map k b)
mapAccum = genericMapAccum Map.mapAccum (flip const)

-- | @O(n m)@. Like 'mapAccum', but the function is applied strictly.
mapAccum' :: Map map k => (acc -> a -> (acc, b))
                       -> acc
                       -> TrieMap map k a
                       -> (acc, TrieMap map k b)
mapAccum' = genericMapAccum Map.mapAccum seq

-- | @O(n m)@. Like 'mapAccum', but the function receives the key in addition
-- to the value associated with it.
mapAccumWithKey :: Map map k => (acc -> [k] -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumWithKey = genericMapAccumWithKey Map.mapAccumWithKey (flip const)

-- | @O(n m)@. Like 'mapAccumWithKey', but the function is applied strictly.
mapAccumWithKey' :: Map map k => (acc -> [k] -> a -> (acc, b))
                              -> acc
                              -> TrieMap map k a
                              -> (acc, TrieMap map k b)
mapAccumWithKey' = genericMapAccumWithKey Map.mapAccumWithKey seq

-- | @O(n m)@. Like 'mapAccum', but in ascending order, as though operating on
-- the 'toAscList' representation.
mapAccumAsc :: OrdMap map k => (acc -> a -> (acc, b))
                            -> acc
                            -> TrieMap map k a
                            -> (acc, TrieMap map k b)
mapAccumAsc = genericMapAccum Map.mapAccumAsc (flip const)

-- | @O(n m)@. Like 'mapAccumAsc', but the function is applied strictly.
mapAccumAsc' :: OrdMap map k => (acc -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumAsc' = genericMapAccum Map.mapAccumAsc seq

-- | @O(n m)@. Like 'mapAccumAsc', but the function receives the key in
-- addition to the value associated with it.
mapAccumAscWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                   -> acc
                                   -> TrieMap map k a
                                   -> (acc, TrieMap map k b)
mapAccumAscWithKey = genericMapAccumWithKey Map.mapAccumAscWithKey (flip const)

-- | @O(n m)@. Like 'mapAccumAscWithKey', but the function is applied strictly.
mapAccumAscWithKey' :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                    -> acc
                                    -> TrieMap map k a
                                    -> (acc, TrieMap map k b)
mapAccumAscWithKey' = genericMapAccumWithKey Map.mapAccumAscWithKey seq

-- | @O(n m)@. Like 'mapAccum', but in descending order, as though operating on
-- the 'toDescList' representation.
mapAccumDesc :: OrdMap map k => (acc -> a -> (acc, b))
                             -> acc
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
mapAccumDesc = genericMapAccum Map.mapAccumDesc (flip const)

-- | @O(n m)@. Like 'mapAccumDesc', but the function is applied strictly.
mapAccumDesc' :: OrdMap map k => (acc -> a -> (acc, b))
                              -> acc
                              -> TrieMap map k a
                              -> (acc, TrieMap map k b)
mapAccumDesc' = genericMapAccum Map.mapAccumDesc seq

-- | @O(n m)@. Like 'mapAccumDesc', but the function receives the key in
-- addition to the value associated with it.
mapAccumDescWithKey :: OrdMap map k => (acc -> [k] -> a -> (acc, b))
                                    -> acc
                                    -> TrieMap map k a
                                    -> (acc, TrieMap map k b)
mapAccumDescWithKey =
   genericMapAccumWithKey Map.mapAccumDescWithKey (flip const)

-- | @O(n m)@. Like 'mapAccumDescWithKey', but the function is applied
-- strictly.
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
genericMapAccum subMapAccum seeq f acc (Tr mv p m) =
   let (acc', mv') =
          case mv of
               Nothing -> (acc, Nothing)
               Just v  ->
                  let (acc'', v') = f acc v
                   in v' `seeq` (acc'', Just v')
    in second (Tr mv' p) $
         subMapAccum (genericMapAccum subMapAccum seeq f) acc' m

genericMapAccumWithKey :: Map map k => (  (  acc
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
   go k subMapAccum seeq f acc (Tr mv p m) =
      let k'         = k `DL.append` DL.fromList p
          (acc', mv') =
             case mv of
                  Nothing -> (acc, Nothing)
                  Just v  ->
                     let (acc'', v') = f acc (DL.toList k') v
                      in v' `seeq` (acc'', Just v')
       in second (Tr mv' p) $
             subMapAccum (\a x -> go (k' `DL.snoc` x) subMapAccum seeq f a)
                         acc' m

-- * Folding

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toList' representation,
-- folding only over the elements.
foldr :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldr = foldrWithKey . const

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toList' representation,
-- folding over both the keys and the elements.
foldrWithKey :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrWithKey = Base.foldrWithKey

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toAscList' representation.
foldrAsc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldrAsc = foldrAscWithKey . const

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toAscList' representation,
-- folding over both the keys and the elements.
foldrAscWithKey :: OrdMap map k
                => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrAscWithKey = Base.foldrAscWithKey

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toDescList' representation.
foldrDesc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldrDesc = foldrDescWithKey . const

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toDescList' representation,
-- folding over both the keys and the elements.
foldrDescWithKey :: OrdMap map k
                 => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldrDescWithKey = Base.foldrDescWithKey

-- | @O(n m)@. Equivalent to a list @foldl@ on the toList representation.
foldl :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldl = foldlWithKey . const

-- | @O(n m)@. Equivalent to a list @foldl@ on the toList representation,
-- folding over both the keys and the elements.
foldlWithKey :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlWithKey = Base.foldlWithKey

-- | @O(n m)@. Equivalent to a list @foldl@ on the toAscList representation.
foldlAsc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlAsc = foldlAscWithKey . const

-- | @O(n m)@. Equivalent to a list @foldl@ on the toAscList representation,
-- folding over both the keys and the elements.
foldlAscWithKey :: OrdMap map k
                => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlAscWithKey = Base.foldlAscWithKey

-- | @O(n m)@. Equivalent to a list @foldl@ on the toDescList representation.
foldlDesc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlDesc = foldlDescWithKey . const

-- | @O(n m)@. Equivalent to a list @foldl@ on the toDescList representation,
-- folding over both the keys and the elements.
foldlDescWithKey :: OrdMap map k
                 => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlDescWithKey = Base.foldlDescWithKey

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toList' representation.
foldl' :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldl' = foldlWithKey' . const

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toList' representation,
-- folding over both the keys and the elements.
foldlWithKey' :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlWithKey' = Base.foldlWithKey'

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toAscList' representation.
foldlAsc' :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlAsc' = foldlAscWithKey' . const

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toAscList' representation,
-- folding over both the keys and the elements.
foldlAscWithKey' :: OrdMap map k
                 => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlAscWithKey' = Base.foldlAscWithKey'

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toDescList'
-- representation.
foldlDesc' :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldlDesc' = foldlDescWithKey' . const

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toDescList'
-- representation, folding over both the keys and the elements.
foldlDescWithKey' :: OrdMap map k
                  => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldlDescWithKey' = Base.foldlDescWithKey'

-- * Conversion between lists

-- | @O(n m)@. Converts the map to a list of the key-value pairs contained
-- within, in undefined order.
toList :: Map map k => TrieMap map k a -> [([k],a)]
toList = Base.toList

-- | @O(n m)@. Converts the map to a list of the key-value pairs contained
-- within, in ascending order.
toAscList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toAscList = Base.toAscList

-- | @O(n m)@. Converts the map to a list of the key-value pairs contained
-- within, in descending order.
toDescList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toDescList = Base.toDescList

-- | @O(n m)@. Creates a map from a list of key-value pairs. If a key occurs
-- more than once, the value from the last pair (according to the list's order)
-- is the one which ends up in the map.
--
-- > fromList = fromListWith const
fromList :: Map map k => [([k],a)] -> TrieMap map k a
fromList = Base.fromList

-- | @O(n m)@. Like 'fromList', but the given function is used to determine the
-- final value if a key occurs more than once. The function is applied as
-- though it were flipped and then applied as a left fold over the values in
-- the given list's order. Or, equivalently (except as far as performance is
-- concerned), as though the function were applied as a right fold over the
-- values in the reverse of the given list's order. For example:
--
-- > fromListWith (-) [("a",1),("a",2),("a",3),("a",4)]
-- >    == fromList [("a",4-(3-(2-1)))]
-- >    == fromList [("a",2)]
fromListWith :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith = Base.fromListWith

-- | @O(n m)@. Like 'fromListWith', but the combining function is applied
-- strictly.
fromListWith' :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith' = Base.fromListWith'

-- | @O(n m)@. Like 'fromListWith', but the key, in addition to the values to
-- be combined, is passed to the combining function.
fromListWithKey :: Map map k
                => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey = Base.fromListWithKey

-- | @O(n m)@. Like 'fromListWithKey', but the combining function is applied
-- strictly.
fromListWithKey' :: Map map k
                 => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey' = Base.fromListWithKey'

-- * Ordering ops

-- | @O(m)@. Removes and returns the minimal key in the map, along with the
-- value associated with it. If the map is empty, 'Nothing' and the original
-- map are returned.
minView :: OrdMap map k => TrieMap map k a -> (Maybe ([k], a), TrieMap map k a)
minView = Base.minView

-- | @O(m)@. Removes and returns the maximal key in the map, along with the
-- value associated with it. If the map is empty, 'Nothing' and the original
-- map are returned.
maxView :: OrdMap map k => TrieMap map k a -> (Maybe ([k], a), TrieMap map k a)
maxView = Base.maxView

-- | @O(m)@. Like 'fst' composed with 'minView'. 'Just' the minimal key in the
-- map and its associated value, or 'Nothing' if the map is empty.
findMin :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMin = Base.findMin

-- | @O(m)@. Like 'fst' composed with 'maxView'. 'Just' the minimal key in the
-- map and its associated value, or 'Nothing' if the map is empty.
findMax :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMax = Base.findMax

-- | @O(m)@. Like 'snd' composed with 'minView'. The map without its minimal
-- key, or the unchanged original map if it was empty.
deleteMin :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMin = Base.deleteMin

-- | @O(m)@. Like 'snd' composed with 'maxView'. The map without its maximal
-- key, or the unchanged original map if it was empty.
deleteMax :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMax = Base.deleteMax

-- | @O(min(m,s))@. Splits the map in two about the given key. The first
-- element of the resulting pair is a map containing the keys lesser than the
-- given key; the second contains those keys that are greater.
split :: OrdMap map k
      => [k] -> TrieMap map k a -> (TrieMap map k a, TrieMap map k a)
split = Base.split

-- | @O(min(m,s))@. Like 'split', but also returns the value associated with
-- the given key, if any.
splitLookup :: OrdMap map k => [k]
                            -> TrieMap map k a
                            -> (TrieMap map k a, Maybe a, TrieMap map k a)
splitLookup = Base.splitLookup

-- | @O(m)@. 'Just' the key of the map which precedes the given key in order,
-- along with its associated value, or 'Nothing' if the map is empty.
findPredecessor :: OrdMap map k => [k] -> TrieMap map k a -> Maybe ([k], a)
findPredecessor = Base.findPredecessor

-- | @O(m)@. 'Just' the key of the map which succeeds the given key in order,
-- along with its associated value, or 'Nothing' if the map is empty.
findSuccessor :: OrdMap map k => [k] -> TrieMap map k a -> Maybe ([k], a)
findSuccessor = Base.findSuccessor

-- * Trie-only operations

-- | @O(s)@. The map which contains all keys of which the given key is a
-- prefix. For example:
--
-- > lookupPrefix "ab" (fromList [("a",1),("ab",2),("ac",3),("abc",4)])
-- >    == fromList [("ab",2),("abc",4)]
lookupPrefix :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
lookupPrefix = Base.lookupPrefix

-- | @O(s)@. Prepends the given key to all the keys of the map. For example:
--
-- > addPrefix "xa" (fromList [("a",1),("b",2)])
-- >    == fromList [("xaa",1),("xab",2)]
addPrefix :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
addPrefix = Base.addPrefix

-- | @O(s)@. The map which contains all keys of which the given key is a
-- prefix, with the prefix removed from each key. If the given key is not a
-- prefix of any key in the map, an empty map is returned. For example:
--
-- > deletePrefix "a" (fromList [("a",1),("ab",2),("ac",3)])
-- >    == fromList [("",1),("b",2),("c",3)]
--
-- This function can be used, for instance, to reduce potentially expensive I/O
-- operations: if you need to find the value in a map associated with a string,
-- but you only have a prefix of it and retrieving the rest is an expensive
-- operation, calling 'deletePrefix' with what you have might allow you to
-- avoid the operation: if the resulting map is empty, the entire string cannot
-- be a member of the map.
deletePrefix :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
deletePrefix = Base.deletePrefix

-- | @O(s)@. Deletes all keys which are suffixes of the given key. For example:
--
-- > deleteSuffixes "ab" (fromList $ zip ["a","ab","ac","b","abc"] [1..])
-- >    == fromList [("a",1),("ac",3),("b",4)]
deleteSuffixes :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
deleteSuffixes = Base.deleteSuffixes

-- | @O(1)@. A triple containing the longest common prefix of all keys in the
-- map, the value associated with that prefix, if any, and the map with that
-- prefix removed from all the keys as well as the map itself. Examples:
--
-- > splitPrefix (fromList [("a",1),("b",2)])
-- >    == ("", Nothing, fromList [("a",1),("b",2)])
-- > splitPrefix (fromList [("a",1),("ab",2),("ac",3)])
-- >    == ("a", Just 1, fromList [("b",2),("c",3)])
splitPrefix :: Map map k => TrieMap map k a -> ([k], Maybe a, TrieMap map k a)
splitPrefix = Base.splitPrefix

-- | @O(1)@. The children of the longest common prefix in the trie as maps,
-- associated with their distinguishing key value. If the map contains less
-- than two keys, this function will return an empty map. Examples;
--
-- > children (fromList [("a",1),("abc",2),("abcd",3)])
-- >    == Map.fromList [('b',fromList [("c",2),("cd",3)])]
-- > children (fromList [("b",1),("c",2)])
-- >    == Map.fromList [('b',fromList [("",1)]),('c',fromList [("",2)])]
children :: Map map k => TrieMap map k a -> map k (TrieMap map k a)
children = Base.children

-- | @O(1)@. The children of the first element of the longest common prefix in
-- the trie as maps, associated with their distinguishing key value. If the map
-- contains less than two keys, this function will return an empty map.
--
-- If the longest common prefix of all keys in the trie is the empty list, this
-- function is equivalent to 'children'.
--
-- Examples:
--
-- > children1 (fromList [("abc",1),("abcd",2)])
-- >    == Map.fromList [('a',fromList [("bc",1),("bcd",2)])]
-- > children1 (fromList [("b",1),("c",2)])
-- >    == Map.fromList [('b',fromList [("",1)]),('c',fromList [("",2)])]
children1 :: Map map k => TrieMap map k a -> map k (TrieMap map k a)
children1 = Base.children1

-- * Visualization

-- | @O(n m)@. Displays the map's internal structure in an undefined way. That
-- is to say, no program should depend on the function's results.
showTrie :: (Show k, Show a, Map map k) => TrieMap map k a -> ShowS
showTrie = Base.showTrieWith $ \mv -> case mv of
                                           Nothing -> showChar ' '
                                           Just v  -> showsPrec 11 v

-- | @O(n m)@. Like 'showTrie', but uses the given function to display the
-- elements of the map. Still undefined.
showTrieWith :: (Show k, Map map k)
             => (Maybe a -> ShowS) -> TrieMap map k a -> ShowS
showTrieWith = Base.showTrieWith
