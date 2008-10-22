-- File created: 2008-10-18 21:33:40

-- An efficient implementation of sets of lists of enumerable elements, based
-- on tries.
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length.

module Data.Trie.Set.Enum where

import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import Data.List (foldl')
import Prelude hiding (lookup, filter, foldl, foldr, null, map)

data ListSet a = Tr !Bool !(IntMap (ListSet a)) deriving Show

-- instances: Eq, Monoid, Foldable, Ord, Show, Read

-- TODO: ponder whether it's clever or not to have b instead of a in the types

-- * Querying

-- O(1)
null :: ListSet a -> Bool
null (Tr False m) | Map.null m = True
null _                         = False

-- O(n). The number of elements in the set.
size :: ListSet a -> Int
size (Tr b m) = Map.fold ((+) . size) (fromEnum b) m

-- O(m).
member :: Enum a => [a] -> ListSet b -> Bool
member []     (Tr b _) = b
member (x:xs) (Tr _ m) =
   case Map.lookup (fromEnum x) m of
        Nothing -> False
        Just t  -> member xs t

isSubsetOf :: Enum a => ListSet a -> ListSet a -> Bool
isSubsetOf = undefined

isProperSubsetOf :: Enum a => ListSet a -> ListSet a -> Bool
isProperSubsetOf = undefined

-- * Construction

-- O(1)
empty :: ListSet a
empty = Tr False Map.empty

-- O(m)
singleton :: Enum a => [a] -> ListSet b
singleton []     = Tr True Map.empty
singleton (x:xs) = Tr False (Map.singleton (fromEnum x) (singleton xs))

-- O(m)
insert :: Enum a => [a] -> ListSet b -> ListSet b
insert []     (Tr _ m) = Tr True m
insert (x:xs) (Tr b m) = Tr b $
   Map.insertWith (\_ old -> insert xs old)
                  (fromEnum x)
                  (singleton xs) m

-- O(m)
delete :: Enum a => [a] -> ListSet b -> ListSet b
delete []     (Tr _ m) = Tr False m
delete (x:xs) (Tr b m) = Tr b $
   Map.update (\old -> let new = delete xs old
                        in if null new
                              then Nothing
                              else Just new)
              (fromEnum x) m

-- * Combination

-- O(n1+n2)
union :: ListSet a -> ListSet a -> ListSet a
union (Tr b1 m1) (Tr b2 m2) = Tr (b1 || b2) $ Map.unionWith union m1 m2

unions :: [ListSet a] -> ListSet a
unions = foldl' union empty

-- O(n1+n2)
difference :: ListSet a -> ListSet a -> ListSet a
difference (Tr b1 m1) (Tr b2 m2) = Tr (b1 && not b2)$Map.differenceWith f m1 m2
 where
   f t1 t2 = let t' = difference t1 t2 in if null t' then Nothing else Just t'

-- O(n1+n2)
intersection :: ListSet a -> ListSet a -> ListSet a
intersection (Tr b1 m1) (Tr b2 m2) =
   Tr (b1 && b2) (Map.intersectionWith intersection m1 m2)

-- * Filtering

-- O(n)
filter :: Enum a => ([a] -> Bool) -> ListSet a -> ListSet a
filter = undefined

-- O(n)
partition :: Enum a => (a -> Bool) -> ListSet a -> (ListSet a, ListSet a)
partition = undefined

-- * Mapping

-- O(n)
map :: (Enum a, Enum b) => ([a] -> [b]) -> ListSet a -> ListSet b
map = undefined

-- O(n)
-- needs a name!
map' :: (Enum a, Enum b) => (a -> b) -> ListSet a -> ListSet b
map' = undefined

-- * Folding

fold :: ([a] -> b -> b) -> b -> ListSet a -> b
fold = undefined

foldAsc :: ([a] -> b -> b) -> b -> ListSet a -> b
foldAsc = undefined

foldDesc :: ([a] -> b -> b) -> b -> ListSet a -> b
foldDesc = undefined

-- * Conversion between lists

-- O(n)
toList :: Enum a => ListSet a -> [[a]]
toList = genericToList Map.toList

-- O(n)
toAscList :: Enum a => ListSet a -> [[a]]
toAscList = genericToList Map.toAscList

genericToList :: Enum a => (IntMap (ListSet a) -> [(Int, ListSet a)])
                        -> ListSet a
                        -> [[a]]
genericToList = go []
 where
   go l f (Tr b m) =
      let xs = concatMap (\(x,t) -> go (toEnum x:l) f t) (f m)
       in if b
             then reverse l : xs
             else             xs
-- O(n)
fromList :: Enum a => [[a]] -> ListSet a
fromList = foldl' (flip insert) empty
