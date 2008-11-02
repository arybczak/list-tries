-- File created: 2008-10-18 21:33:40

-- An efficient implementation of sets of lists of enumerable elements, based
-- on tries.
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

module Data.Trie.Set.Enum where

import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Monad (mplus)
import qualified Data.DList as DL
import Data.DList (DList)
import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import qualified Data.List as List
import Data.List (foldl', maximumBy, minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

-- Invariant: any (Tr False _) has a True descendant.
data TrieSet a = Tr !Bool !(IntMap (TrieSet a)) deriving Show

-- instances: Eq, Monoid, Foldable, Ord, Show, Read

-- * Querying

-- O(1)
null :: TrieSet a -> Bool
null (Tr False m) | Map.null m = True
null _                         = False

-- O(n). The number of elements in the set.
size :: TrieSet a -> Int
size (Tr b m) = Map.fold ((+) . size) (fromEnum b) m

-- O(m).
member :: Enum a => [a] -> TrieSet a -> Bool
member []     (Tr b _) = b
member (x:xs) (Tr _ m) =
   case Map.lookup (fromEnum x) m of
        Nothing -> False
        Just t  -> member xs t

-- O(?)
isSubsetOf :: TrieSet a -> TrieSet a -> Bool
isSubsetOf (Tr True _) (Tr False _) = False
isSubsetOf (Tr _   m1) (Tr _    m2) = Map.isSubmapOfBy isSubsetOf m1 m2

-- O(?)
isProperSubsetOf :: TrieSet a -> TrieSet a -> Bool
isProperSubsetOf = go False
 where
   go _      (Tr True _) (Tr False _) = False
   go proper (Tr b1  m1) (Tr b2   m2) =
      -- This seems suboptimal but I can't think of anything better
      let proper' = or [ proper
                       , not b1 && b2
                       , not (Map.null $ Map.difference m2 m1)
                       ]
       in if Map.null m1
             then proper'
             else Map.isSubmapOfBy (go proper') m1 m2

-- * Construction

-- O(1)
empty :: TrieSet a
empty = Tr False Map.empty

-- O(m)
singleton :: Enum a => [a] -> TrieSet a
singleton []     = Tr True Map.empty
singleton (x:xs) = Tr False (Map.singleton (fromEnum x) (singleton xs))

-- O(m)
insert :: Enum a => [a] -> TrieSet a -> TrieSet a
insert []     (Tr _ m) = Tr True m
insert (x:xs) (Tr b m) = Tr b $
   Map.insertWith (\_ old -> insert xs old)
                  (fromEnum x)
                  (singleton xs) m

-- O(m)
delete :: Enum a => [a] -> TrieSet a -> TrieSet a
delete []     (Tr _ m) = Tr False m
delete (x:xs) (Tr b m) = Tr b $
   Map.update (\old -> let new = delete xs old
                        in if null new
                              then Nothing
                              else Just new)
              (fromEnum x) m

-- * Combination

-- O(n1+n2)
union :: TrieSet a -> TrieSet a -> TrieSet a
union (Tr b1 m1) (Tr b2 m2) = Tr (b1 || b2) $ Map.unionWith union m1 m2

unions :: [TrieSet a] -> TrieSet a
unions = foldl' union empty

-- O(n1+n2)
difference :: TrieSet a -> TrieSet a -> TrieSet a
difference (Tr b1 m1) (Tr b2 m2) = Tr (b1 && not b2)$Map.differenceWith f m1 m2
 where
   f t1 t2 = let t' = difference t1 t2 in if null t' then Nothing else Just t'

-- O(n1+n2)
intersection :: TrieSet a -> TrieSet a -> TrieSet a
intersection (Tr b1 m1) (Tr b2 m2) =
   Tr (b1 && b2) (Map.intersectionWith intersection m1 m2)

-- * Filtering

-- O(n)
filter :: Enum a => ([a] -> Bool) -> TrieSet a -> TrieSet a
filter p = fromList . Prelude.filter p . toList

-- O(n)
partition :: Enum a => ([a] -> Bool) -> TrieSet a -> (TrieSet a, TrieSet a)
partition p = (fromList *** fromList) . List.partition p . toList

split :: Enum a => [a] -> TrieSet a -> (TrieSet a, TrieSet a)
split xs tr = let (l,_,g) = splitMember xs tr in (l,g)

splitMember :: Enum a => [a] -> TrieSet a -> (TrieSet a, Bool, TrieSet a)
splitMember []     (Tr b m) = (empty, b, Tr False m)
splitMember (x:xs) (Tr b m) =
   let (ml, tr, mg) = Map.splitLookup (fromEnum x) m
    in case tr of
            Nothing  -> (Tr b ml, False, Tr False mg)
            Just tr' ->
               let (tl, b', tg) = splitMember xs tr'
                   ml' = if null tl then ml else Map.insert (fromEnum x) tl ml
                   mg' = if null tg then mg else Map.insert (fromEnum x) tg mg
                in (Tr b ml', b', Tr False mg')

-- * Mapping

-- O(n)
map :: (Enum a, Enum b) => ([a] -> [b]) -> TrieSet a -> TrieSet b
map f = fromList . Prelude.map f . toList

-- O(n)
-- needs a name!
map' :: (Enum a, Enum b) => (a -> b) -> TrieSet a -> TrieSet b
map' f (Tr b m) =
   Tr b $
      Map.fromDistinctAscList .
         Prelude.map (fromEnum.f.toEnum *** map' f) .
      Map.toAscList $ m

-- * Folding

-- O(n)
fold :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
fold f x = Prelude.foldr f x . toList

-- O(n)
foldAsc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldAsc f x = Prelude.foldr f x . toAscList

-- O(n)
foldDesc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldDesc f x = Prelude.foldr f x . toDescList

-- * Conversion between lists

-- O(n)
toList :: Enum a => TrieSet a -> [[a]]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: Enum a => TrieSet a -> [[a]]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: Enum a => TrieSet a -> [[a]]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Enum a => (IntMap (TrieSet a) -> [(Int, TrieSet a)])
                        -> ([a] -> DList [a] -> DList [a])
                        -> TrieSet a
                        -> [[a]]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go l f g (Tr b m) =
      let
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l `DL.snoc` toEnum x) f g t) .
            f $ m
       in if b
             then g (DL.toList l) xs
             else                 xs
-- O(n)
fromList :: Enum a => [[a]] -> TrieSet a
fromList = foldl' (flip insert) empty

-- * Min/max

-- O(m log b)
findMin :: Enum a => TrieSet a -> Maybe [a]
findMin = findMinMax (\(Tr b _) -> b)
                     (flip const)
                     (fst . fromJust . Map.minViewWithKey)

-- O(m log b)
findMax :: Enum a => TrieSet a -> Maybe [a]
findMax = findMinMax (\(Tr _ m) -> Map.null m)
                     (\(Tr b _) -> assert b)
                     (fst . fromJust . Map.maxViewWithKey)

findMinMax :: Enum a
           => (TrieSet a -> Bool)
           -> (TrieSet a -> [a] -> [a])
           -> (IntMap (TrieSet a) -> (Int, TrieSet a))
           -> TrieSet a
           -> Maybe [a]
findMinMax _ _ _ tr_ | null tr_ = Nothing
findMinMax f g h tr_ = Just (go f g h tr_)
 where
   go cond base mapView tr@(Tr _ m) =
      if cond tr
         then base tr []
         else let (k,t) = mapView m
               in toEnum k : go cond base mapView t

-- O(m log b)
deleteMin :: Enum a => TrieSet a -> TrieSet a
deleteMin = maybe empty snd . minView

-- O(m log b)
deleteMax :: Enum a => TrieSet a -> TrieSet a
deleteMax = maybe empty snd . maxView

-- O(m log b)
minView :: Enum a => TrieSet a -> Maybe ([a], TrieSet a)
minView = minMaxView (\(Tr b _) -> b)
                     (flip const)
                     (fst . fromJust . Map.minViewWithKey)

-- O(m log b)
maxView :: Enum a => TrieSet a -> Maybe ([a], TrieSet a)
maxView = minMaxView (\(Tr _ m) -> Map.null m)
                     (\(Tr b _) -> assert b)
                     (fst . fromJust . Map.maxViewWithKey)

minMaxView :: Enum a
           => (TrieSet a -> Bool)
           -> (TrieSet a -> ([a], TrieSet a) -> ([a], TrieSet a))
           -> (IntMap (TrieSet a) -> (Int, TrieSet a))
           -> TrieSet a
           -> Maybe ([a], TrieSet a)
minMaxView _ _ _ tr_ | null tr_ = Nothing
minMaxView f g h tr_ = Just (go f g h DL.empty tr_)
 where
   go cond base mapView xs tr@(Tr b m) =
      if cond tr
         then base tr (DL.toList xs, Tr False m)
         else let (k,      t)  = mapView m
                  (minMax, t') = go cond base mapView (xs `DL.snoc` toEnum k) t
               in ( minMax
                  , Tr b $ if null t'
                              then Map.delete            (fromEnum k) m
                              else Map.adjust (const t') (fromEnum k) m
                  )

-- * Trie-specific operations

-- O(m b)
findPredecessor :: Enum a => TrieSet a -> [a] -> Maybe [a]
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go _ [] = Nothing

   -- We need to try the trie at x and then the trie at the predecessor of x:
   -- e.g. if looking for "foo", we need to try any 'f' branch to see if it has
   -- "fob" first, before grabbing the next-best option of the maximum of the
   -- 'b' branch, say "bar".
   --
   -- If there's no branch less than 'f' we try the current position as a last
   -- resort.
   go (Tr b m) (x:xs) =
      let x'         = fromEnum x
          candidates = Prelude.filter ((< x').fst) $ Map.toList m
          (best,btr) = maximumBy (comparing fst) candidates

       in fmap (x:) (Map.lookup x' m >>= flip go xs)
          `mplus`
          if Prelude.null candidates
             then if b then Just [] else Nothing
             else fmap (toEnum best:) (findMax btr)

-- O(m b)
findSuccessor :: Enum a => TrieSet a -> [a] -> Maybe [a]
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go (Tr _ m) [] = do ((k,t),_) <- Map.minViewWithKey m
                       fmap (toEnum k:) (findMin t)

   go (Tr _ m) (x:xs) =
      let x'         = fromEnum x
          candidates = Prelude.filter ((> x').fst) $ Map.toList m
          (best,btr) = minimumBy (comparing fst) candidates

       in fmap (x:) (Map.lookup x' m >>= flip go xs)
          `mplus`
          if Prelude.null candidates
             then Nothing
             else fmap (toEnum best:) (findMin btr)
