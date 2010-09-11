-- File created: 2008-11-08 15:52:33

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances
           , FlexibleContexts, UndecidableInstances #-}

#include "exports.h"

-- | The base implementation of a trie representing a set of lists, generalized
-- over any type of map from key values to tries.
--
-- Worst-case complexities are given in terms of @n@, @m@, and @k@. @n@ refers
-- to the number of keys in the set and @m@ to their maximum length. @k@ refers
-- to the length of a key given to the function, not any property of the set.
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
module Data.ListTrie.Set (SET_EXPORTS) where

import Control.Arrow  ((***))
import Control.Monad  (liftM2)
import Data.Binary    (Binary,get,put)
import Data.Function  (on)
import Data.Monoid    (Monoid(..))
import Prelude hiding (filter, foldl, foldr, map, null)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident))
#endif

import qualified Data.ListTrie.Base     as Base
import qualified Data.ListTrie.Base.Map as Map
import Data.ListTrie.Base.Classes (Identity(..), Unwrappable(..))
import Data.ListTrie.Base.Map     (Map, OrdMap)
import Data.ListTrie.Util         ((.:), (.:.), both)

#include "docs.h"

-- Invariant: any (Tr False _) has a True descendant.
--
-- We need this 'bool' and Base stuff in order to satisfy the Base.Trie type
-- class.
data TrieSetBase map a bool = Tr !bool !(CMap map a bool)
type CMap map a bool = map a (TrieSetBase map a bool)

-- That makes TrieSet a newtype, which means some unfortunate wrapping and
-- unwrapping in the function definitions below.
--
-- | The data structure itself: a set of keys of type @[a]@ implemented as a
-- trie, using @map@ to map keys of type @a@ to sub-tries.
--
-- Regarding the instances:
--
-- - The @CMap@ type is internal, ignore it. For 'Eq' and 'Ord' an 'Eq'
--   instance is required: what this means is that @map a v@ is expected to be
--   an instance of 'Eq', given 'Eq'@ v@.
--
-- - The 'Eq' constraint for the 'Ord' instance is misleading: it is needed
--   only because 'Eq' is a superclass of 'Ord'.
--
-- - The 'Monoid' instance defines 'mappend' as 'union' and 'mempty' as
--   'empty'.
newtype TrieSet map a = TS { unTS :: TrieSetBase map a Bool }

inTS :: (TrieSetBase map a Bool -> TrieSetBase nap b Bool)
     -> (TrieSet map a -> TrieSet nap b)
inTS f = TS . f . unTS

instance Map map k => Base.Trie TrieSetBase Identity map k where
   mkTrie = Tr . unwrap
   tParts (Tr b m) = (Id b,m)

-- CMap contains TrieSetBase, not TrieSet, hence we must supply these instances
-- for TrieSetBase first
instance Eq (CMap map a Bool) => Eq (TrieSetBase map a Bool) where
   Tr b1 m1 == Tr b2 m2 = b1 == b2 && m1 == m2

instance (Eq (CMap map a Bool), OrdMap map a, Ord a)
      => Ord (TrieSetBase map a Bool)
 where
   compare = compare `on` Base.toAscList

instance Eq (CMap map a Bool) => Eq (TrieSet map a) where
   (==) = (==) `on` unTS

-- The CMap constraint is needed only because Eq is a superclass of Ord....
-- sigh
instance (Eq (CMap map a Bool), OrdMap map a, Ord a) => Ord (TrieSet map a)
 where
   compare = compare `on` unTS

instance Map map a => Monoid (TrieSet map a) where
   mempty  = empty
   mappend = union
   mconcat = unions

instance (Map map a, Show a) => Show (TrieSet map a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map a, Read a) => Read (TrieSet map a) where
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

instance (Map map k, Binary k, Binary a) => Binary (TrieSetBase map k a) where
   put (Tr v m) = put v >> (put . Map.serializeToList $ m)
   get = liftM2 Tr get (get >>= return . Map.deserializeFromList)

instance (Map map a, Binary a) => Binary (TrieSet map a) where
   put = put . unTS
   get = get >>= return . TS

-- * Construction

-- | @O(1)@. The empty set.
empty :: Map map a => TrieSet map a
empty = TS Base.empty

-- | @O(s)@. The singleton set containing only the given key.
singleton :: Map map a => [a] -> TrieSet map a
singleton k = TS$ Base.singleton k True

-- * Modification

-- | @O(min(m,s))@. Inserts the key into the set. If the key is already a
-- member of the set, the set is unchanged.
insert :: Map map a => [a] -> TrieSet map a -> TrieSet map a
insert k = inTS$ Base.insert k True

-- | @O(min(m,s))@. Removes the key from the set. If the key is not a member of
-- the set, the set is unchanged.
delete :: Map map a => [a] -> TrieSet map a -> TrieSet map a
delete = inTS . Base.delete

-- * Querying

-- | @O(1)@. 'True' iff the set is empty.
null :: Map map a => TrieSet map a -> Bool
null = Base.null . unTS

-- | @O(n m)@. The number of keys in the set. The value is built up lazily,
-- allowing for delivery of partial results without traversing the whole set.
size :: (Map map a, Num n) => TrieSet map a -> n
size = Base.size . unTS

-- | @O(n m)@. The number of keys in the set. The value is built strictly: no
-- value is returned until the set has been fully traversed.
size' :: (Map map a, Num n) => TrieSet map a -> n
size' = Base.size' . unTS

-- | @O(min(m,s))@. 'True' iff the given key is contained within the set.
member :: Map map a => [a] -> TrieSet map a -> Bool
member = Base.member .:. unTS

-- | @O(min(m,s))@. 'False' iff the given key is contained within the set.
notMember :: Map map a => [a] -> TrieSet map a -> Bool
notMember = Base.notMember .:. unTS

-- | @O(min(n1 m1,n2 m2))@. 'True' iff the first set is a subset of the second,
-- i.e. all keys that are members of the first set are also members of the
-- second set.
isSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isSubsetOf = Base.isSubmapOfBy (&&) `on` unTS

-- | @O(min(n1 m1,n2 m2))@. 'True' iff the first set is a proper subset of the
-- second, i.e. the first is a subset of the second, but the sets are not
-- equal.
isProperSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isProperSubsetOf = Base.isProperSubmapOfBy (&&) `on` unTS

-- * Combination

defaultUnion :: Bool -> Bool -> Bool
defaultUnion = error "TrieSet.union :: internal error"

-- | @O(min(n1 m1,n2 m2))@. The union of the two sets: the set which contains
-- all keys that are members of either set.
--
-- The worst-case performance occurs when the two sets are identical.
union :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
union = TS .: Base.unionWith defaultUnion `on` unTS

-- | @O(sum(n))@. The union of all the sets: the set which contains all keys
-- that are members of any of the sets.
--
-- The worst-case performance occurs when all the sets are identical.
unions :: Map map a => [TrieSet map a] -> TrieSet map a
unions = TS . Base.unionsWith defaultUnion . Prelude.map unTS

-- | @O(min(n1 m1,n2 m2))@. The difference of the two sets: the set which
-- contains all keys that are members of the first set and not members of the
-- second set.
--
-- The worst-case performance occurs when the two sets are identical.
difference :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
difference = TS .: Base.differenceWith
                      (error "TrieSet.difference :: internal error")
                   `on` unTS

-- | @O(min(n1 m1,n2 m2))@. The intersection of the two sets: the set which
-- contains all keys that are members of both sets.
--
-- The worst-case performance occurs when the two sets are identical.
intersection :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
intersection = TS .: Base.intersectionWith
                        (error "TrieSet.intersection :: internal error")
                     `on` unTS

-- * Filtering

-- | @O(n m)@. The set of those keys in the set for which the given predicate
-- returns 'True'.
filter :: Map map a => ([a] -> Bool) -> TrieSet map a -> TrieSet map a
filter p = inTS $ Base.filterWithKey (\k _ -> p k)

-- | @O(n m)@. A pair of sets: the first element contains those keys for which
-- the given predicate returns 'True', and the second element contains those
-- for which it was 'False'.
partition :: Map map a
          => ([a] -> Bool) -> TrieSet map a -> (TrieSet map a, TrieSet map a)
partition p = both TS . Base.partitionWithKey (\k _ -> p k) . unTS

-- * Mapping

-- | @O(n m)@. Apply the given function to all the keys in the set.
map :: (Map map a, Map map b) => ([a] -> [b]) -> TrieSet map a -> TrieSet map b
map = inTS . Base.mapKeysWith Base.fromList

-- | @O(n m)@. Apply the given function to the contents of all the keys in the
-- set.
mapIn :: (Map map a, Map map b) => (a -> b) -> TrieSet map a -> TrieSet map b
mapIn = inTS . Base.mapInKeysWith defaultUnion

-- * Folding

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toList' representation.
foldr :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldr f = Base.foldrWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toAscList' representation.
foldrAsc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldrAsc f = Base.foldrAscWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldr@ on the 'toDescList' representation.
foldrDesc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldrDesc f = Base.foldrDescWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl@ on the 'toList' representation.
foldl :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldl f = Base.foldlWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl@ on the 'toAscList' representation.
foldlAsc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldlAsc f = Base.foldlAscWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl@ on the 'toDescList' representation.
foldlDesc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldlDesc f = Base.foldlDescWithKey (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toList' representation.
foldl' :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldl' f = Base.foldlWithKey' (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toAscList' representation.
foldlAsc' :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldlAsc' f = Base.foldlAscWithKey' (\k _ -> f k) .:. unTS

-- | @O(n m)@. Equivalent to a list @foldl'@ on the 'toDescList'
-- representation.
foldlDesc' :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldlDesc' f = Base.foldlDescWithKey' (\k _ -> f k) .:. unTS

-- * Conversion between lists

-- | @O(n m)@. Converts the set to a list of the keys contained within, in
-- undefined order.
toList :: Map map a => TrieSet map a -> [[a]]
toList = Prelude.map fst . Base.toList . unTS

-- | @O(n m)@. Converts the set to a list of the keys contained within, in
-- ascending order.
toAscList :: OrdMap map a => TrieSet map a -> [[a]]
toAscList = Prelude.map fst . Base.toAscList . unTS

-- | @O(n m)@. Converts the set to a list of the keys contained within, in
-- descending order.
toDescList :: OrdMap map a => TrieSet map a -> [[a]]
toDescList = Prelude.map fst . Base.toDescList . unTS

-- | @O(n m)@. Creates a set from a list of keys.
fromList :: Map map a => [[a]] -> TrieSet map a
fromList = TS . Base.fromList . Prelude.map (flip (,) True)

-- * Ordering ops

-- | @O(m)@. Removes and returns the minimal key in the set. If the set is
-- empty, 'Nothing' and the original set are returned.
minView :: OrdMap map a => TrieSet map a -> (Maybe [a], TrieSet map a)
minView = (fmap fst *** TS) . Base.minView . unTS

-- | @O(m)@. Removes and returns the maximal key in the set. If the set is
-- empty, 'Nothing' and the original set are returned.
maxView :: OrdMap map a => TrieSet map a -> (Maybe [a], TrieSet map a)
maxView = (fmap fst *** TS) . Base.maxView . unTS

-- | @O(m)@. Like 'fst' composed with 'minView'. 'Just' the minimal key in the
-- set, or 'Nothing' if the set is empty.
findMin :: OrdMap map a => TrieSet map a -> Maybe [a]
findMin = fmap fst . Base.findMin . unTS

-- | @O(m)@. Like 'fst' composed with 'maxView'. 'Just' the maximal key in the
-- set, or 'Nothing' if the set is empty.
findMax :: OrdMap map a => TrieSet map a -> Maybe [a]
findMax = fmap fst . Base.findMax . unTS

-- | @O(m)@. Like 'snd' composed with 'minView'. The set without its minimal
-- key, or the unchanged original set if it was empty.
deleteMin :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMin = inTS Base.deleteMin

-- | @O(m)@. Like 'snd' composed with 'maxView'. The set without its maximal
-- key, or the unchanged original set if it was empty.
deleteMax :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMax = inTS Base.deleteMax

-- | @O(min(m,s))@. Splits the set in two about the given key. The first
-- element of the resulting pair is a set containing the keys lesser than the
-- given key; the second contains those keys that are greater.
split :: OrdMap map a => [a] -> TrieSet map a -> (TrieSet map a, TrieSet map a)
split = both TS .: Base.split .:. unTS

-- | @O(min(m,s))@. Like 'split', but also returns whether the given key was a
-- member of the set or not.
splitMember :: OrdMap map a
            => [a] -> TrieSet map a -> (TrieSet map a, Bool, TrieSet map a)
splitMember = (\(l,b,g) -> (TS l,unwrap b,TS g)) .: Base.splitLookup .:. unTS

-- | @O(m)@. 'Just' the key of the set which precedes the given key in order,
-- or 'Nothing' if the set is empty.
findPredecessor :: OrdMap map a => [a] -> TrieSet map a -> Maybe [a]
findPredecessor = fmap fst .: Base.findPredecessor .:. unTS

-- | @O(m)@. 'Just' the key of the set which succeeds the given key in order,
-- or 'Nothing' if the set is empty.
findSuccessor :: OrdMap map a => [a] -> TrieSet map a -> Maybe [a]
findSuccessor = fmap fst .: Base.findSuccessor .:. unTS

-- * Trie-only operations

-- | @O(s)@. Prepends the given key to all the keys of the set. For example:
--
-- > addPrefix "pre" (fromList ["a","b"]) == fromList ["prea","preb"]
addPrefix :: Map map a => [a] -> TrieSet map a -> TrieSet map a
addPrefix = TS .: Base.addPrefix .:. unTS

-- | @O(m)@. The set which contains all keys of which the given key is a
-- prefix, with the prefix removed from each key. If the given key is not a
-- prefix of any key in the set, an empty set is returned. For example:
--
-- > deletePrefix "a" (fromList ["a","ab","ac"]) == fromList ["","b","c"]
--
-- This function can be used, for instance, to reduce potentially expensive I/O
-- operations: if you need to check whether a string is a member of a set, but
-- you only have a prefix of it and retrieving the rest is an expensive
-- operation, calling 'deletePrefix' with what you have might allow you to
-- avoid the operation: if the resulting set is empty, the entire string cannot
-- be a member of the set.
deletePrefix :: Map map a => [a] -> TrieSet map a -> TrieSet map a
deletePrefix = TS .: Base.deletePrefix .:. unTS

-- | @O(m)@. A triple containing the longest common prefix of all keys in the
-- set, whether that prefix was a member of the set, and the set with that
-- prefix removed from all the keys as well as the set itself. Examples:
--
-- > splitPrefix (fromList ["a","b"]) == ("", False, fromList ["a","b"])
-- > splitPrefix (fromList ["a","ab","ac"]) == ("a", True, fromList ["b","c"])
splitPrefix :: Map map a => TrieSet map a -> ([a], Bool, TrieSet map a)
splitPrefix = (\(k,b,t) -> (k,unwrap b,TS t)) . Base.splitPrefix . unTS

-- | @O(m)@. The children of the longest common prefix in the trie as sets,
-- associated with their distinguishing key value. If the set contains less
-- than two keys, this function will return an empty map. Examples;
--
-- > children (fromList ["a","abc","abcd"])
-- >    == Map.fromList [('b',fromList ["c","cd"])]
-- > children (fromList ["b","c"])
-- >    == Map.fromList [('b',fromList [""]),('c',fromList [""])]
children :: Map map a => TrieSet map a -> map a (TrieSet map a)
children = Map.map TS . Base.children . unTS

-- | @O(1)@. The children of the first element of the longest common prefix in
-- the trie as sets, associated with their distinguishing key value. If the set
-- contains less than two keys, this function will return an empty map.
--
-- If the longest common prefix of all keys in the trie is the empty list, this
-- function is equivalent to 'children'. Otherwise, the result will always be a
-- single-element map.
--
-- Examples:
--
-- > children1 (fromList ["abc","abcd"])
-- >    == Map.fromList [('a',fromList ["bc","bcd"])]
-- > children1 (fromList ["b","c"])
-- >    == Map.fromList [('b',fromList [""]),('c',fromList [""])]
children1 :: Map map a => TrieSet map a -> map a (TrieSet map a)
children1 = Map.map TS . Base.children1 . unTS

-- * Visualization

-- | @O(n m)@. Displays the set's internal structure in an undefined way. That
-- is to say, no program should depend on the function's results.
showTrie :: (Show a, Map map a) => TrieSet map a -> ShowS
showTrie = Base.showTrieWith (\(Id b) -> showChar $ if b then 'X' else ' ')
         . unTS
