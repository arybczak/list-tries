-- File created: 2008-11-08 15:52:33

-- The base implementation of a trie representing a set of lists, generalized
-- over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

{-# LANGUAGE CPP #-}

module Data.Trie.Set where

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Monad (join)
import qualified Data.DList as DL
import Data.DList (DList)
import qualified Data.List as List
import Data.List (foldl')
import Data.Maybe (fromJust)
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident), pfail)
#endif

import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Map (Map, OrdMap)

-- Invariant: any (Tr False _) has a True descendant.
data TrieSet map a = Tr !Bool !(CMap map a)

type CMap map a = map a (TrieSet map a)

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
null (Tr False m) | Map.null m = True
null _                         = False

-- O(n). The number of elements in the set.
size :: Map map a => TrieSet map a -> Int
size (Tr b m) = Map.foldValues ((+) . size) (fromEnum b) m

-- O(m).
member :: Map map a => [a] -> TrieSet map a -> Bool
member []     (Tr b _) = b
member (x:xs) (Tr _ m) =
   case Map.lookup m x of
        Nothing -> False
        Just t  -> member xs t

-- O(?)
isSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isSubsetOf (Tr True _) (Tr False _) = False
isSubsetOf (Tr _   m1) (Tr _    m2) = Map.isSubmapOfBy isSubsetOf m1 m2

-- O(?)
isProperSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
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
empty :: Map map a => TrieSet map a
empty = Tr False Map.empty

-- O(m)
singleton :: Map map a => [a] -> TrieSet map a
singleton []     = Tr True Map.empty
singleton (x:xs) = Tr False (Map.singleton x (singleton xs))

-- O(m)
insert :: Map map a => [a] -> TrieSet map a -> TrieSet map a
insert []     (Tr _ m) = Tr True m
insert (x:xs) (Tr b m) = Tr b $
   Map.insertWith (\_ old -> insert xs old)
                  m x (singleton xs)

-- O(m)
delete :: Map map a => [a] -> TrieSet map a -> TrieSet map a
delete []     (Tr _ m) = Tr False m
delete (x:xs) (Tr b m) = Tr b $
   Map.update (\old -> let new = delete xs old
                        in if null new
                              then Nothing
                              else Just new)
              m x

-- * Combination

-- O(n1+n2)
union :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
union (Tr b1 m1) (Tr b2 m2) = Tr (b1 || b2) (Map.unionWith union m1 m2)

unions :: Map map a => [TrieSet map a] -> TrieSet map a
unions = foldl' union empty

-- O(n1+n2)
difference :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
difference (Tr b1 m1) (Tr b2 m2) =
   Tr (b1 && not b2) (Map.differenceWith f m1 m2)
 where
   f t1 t2 = let t' = difference t1 t2 in if null t' then Nothing else Just t'

-- O(n1+n2)
intersection :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
intersection (Tr b1 m1) (Tr b2 m2) =
   tr (b1 && b2) (Map.intersectionWith intersection m1 m2)
 where
   tr b m = case Map.singletonView m of
                 Just (_, child@(Tr _ subM)) | null child -> Tr b subM
                 _                                        -> Tr b m

-- * Filtering

-- O(n)
filter :: Map map a => ([a] -> Bool) -> TrieSet map a -> TrieSet map a
filter p = fromList . Prelude.filter p . toList

-- O(n)
partition :: Map map a => ([a] -> Bool)
                       -> TrieSet map a
                       -> (TrieSet map a, TrieSet map a)
partition p = join (***) fromList . List.partition p . toList

split :: OrdMap map a => [a] -> TrieSet map a -> (TrieSet map a, TrieSet map a)
split xs tr = let (l,_,g) = splitMember xs tr in (l,g)

splitMember :: OrdMap map a => [a]
                            -> TrieSet map a
                            -> (TrieSet map a, Bool, TrieSet map a)
splitMember []     (Tr b m) = (empty, b, Tr False m)
splitMember (x:xs) (Tr b m) =
   let (ml, tr, mg) = Map.splitLookup m x
    in case tr of
            Nothing  -> (Tr b ml, False, Tr False mg)
            Just tr' ->
               let (tl, b', tg) = splitMember xs tr'
                   ml' = if null tl then ml else Map.insert ml x tl
                   mg' = if null tg then mg else Map.insert mg x tg
                in (Tr b ml', b', Tr False mg')

-- * Mapping

-- O(n)
map :: (Map map a, Map map b) => ([a] -> [b]) -> TrieSet map a -> TrieSet map b
map f = fromList . Prelude.map f . toList

-- O(n)
-- TODO: needs a name!
map' :: (Map map a, Map map b) => (a -> b) -> TrieSet map a -> TrieSet map b
map' f (Tr b m) =
   Tr b $
      Map.fromListWith union .
         Prelude.map (f *** map' f) .
      Map.toList $ m

-- * Folding

-- O(n)
fold :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
fold f x = Prelude.foldr f x . toList

-- O(n)
foldAsc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldAsc f x = Prelude.foldr f x . toAscList

-- O(n)
foldDesc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldDesc f x = Prelude.foldr f x . toDescList

-- * Conversion between lists

-- O(n)
toList :: Map map a => TrieSet map a -> [[a]]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: OrdMap map a => TrieSet map a -> [[a]]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: OrdMap map a => TrieSet map a -> [[a]]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Map map a => (CMap map a -> [(a, TrieSet map a)])
                           -> ([a] -> DList [a] -> DList [a])
                           -> TrieSet map a
                           -> [[a]]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go l f g (Tr b m) =
      let
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l `DL.snoc` x) f g t) .
            f $ m
       in if b
             then g (DL.toList l) xs
             else                 xs
-- O(n)
fromList :: Map map a => [[a]] -> TrieSet map a
fromList = foldl' (flip insert) empty

-- * Min/max

-- O(m log b)
findMin :: OrdMap map a => TrieSet map a -> Maybe [a]
findMin = findMinMax (\(Tr b _) -> b)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
findMax :: OrdMap map a => TrieSet map a -> Maybe [a]
findMax = findMinMax (\(Tr _ m) -> Map.null m)
                     (\(Tr b _) -> assert b)
                     (fst . Map.maxViewWithKey)

findMinMax :: OrdMap map a => (TrieSet map a -> Bool)
                           -> (TrieSet map a -> [a] -> [a])
                           -> (CMap map a -> Maybe (a, TrieSet map a))
                           -> TrieSet map a
                           -> Maybe [a]
findMinMax _ _ _ tr_ | null tr_ = Nothing
findMinMax f g h tr_ = Just (go f g h tr_)
 where
   go cond base mapView tr@(Tr _ m) =
      if cond tr
         then base tr []
         else let (k,t) = fromJust (mapView m)
               in k : go cond base mapView t

-- O(m log b)
deleteMin :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMin = maybe empty snd . minView

-- O(m log b)
deleteMax :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMax = maybe empty snd . maxView

-- O(m log b)
minView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
minView = minMaxView (\(Tr b _) -> b)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
maxView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
maxView = minMaxView (\(Tr _ m) -> Map.null m)
                     (\(Tr b _) -> assert b)
                     (fst . Map.maxViewWithKey)

minMaxView :: OrdMap map a
           => (TrieSet map a -> Bool)
           -> (TrieSet map a -> ([a], TrieSet map a) -> ([a], TrieSet map a))
           -> (CMap map a -> Maybe (a, TrieSet map a))
           -> TrieSet map a
           -> Maybe ([a], TrieSet map a)
minMaxView _ _ _ tr_ | null tr_ = Nothing
minMaxView f g h tr_ = Just (go f g h DL.empty tr_)
 where
   go cond base mapView xs tr@(Tr b m) =
      if cond tr
         then base tr (DL.toList xs, Tr False m)
         else let (k,      t)  = fromJust (mapView m)
                  (minMax, t') = go cond base mapView (xs `DL.snoc` k) t
               in ( minMax
                  , Tr b $ if null t'
                              then Map.delete            m k
                              else Map.adjust (const t') m k
                  )

-- * Trie-specific operations

-- O(m b)
findPredecessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
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
      let predecessor = Map.findPredecessor m x
       in fmap (x:) (Map.lookup m x >>= flip go xs)
          <|>
          case predecessor of
               Nothing         -> if b then Just [] else Nothing
               Just (best,btr) -> fmap (best:) (findMax btr)

-- O(m b)
findSuccessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go (Tr _ m) [] = do (k,t) <- fst $ Map.minViewWithKey m
                       fmap (k:) (findMin t)

   go (Tr _ m) (x:xs) =
      let successor = Map.findSuccessor m x
       in fmap (x:) (Map.lookup m x >>= flip go xs)
          <|>
          (successor >>= \(best,btr) -> fmap (best:) (findMin btr))
