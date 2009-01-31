-- File created: 2008-11-13 21:13:55

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts #-}

module Data.Trie.Base
   ( Trie(..)
   , null, size, member, notMember, lookup, lookupWithDefault
   , isSubmapOfBy, isProperSubmapOfBy
   , empty, singleton
   , insert, insertWith, insertWith'
   , delete, adjust, adjust', updateLookup, alter, alter'
   , unionWith, unionWithKey, unionWith', unionWithKey'
   , unionsWith, unionsWithKey, unionsWith', unionsWithKey'
   , differenceWith, differenceWithKey
   , intersectionWith,  intersectionWithKey
   , intersectionWith', intersectionWithKey'
   , filterWithKey, partitionWithKey
   , split, splitLookup
   , mapKeysWith, mapKeys'With
   , foldrWithKey, foldrAscWithKey, foldrDescWithKey
   , foldl'WithKey, foldl'AscWithKey, foldl'DescWithKey
   , toList, toAscList, toDescList
   , fromList, fromListWith, fromListWith', fromListWithKey, fromListWithKey'
   , findMin, findMax, deleteMin, deleteMax, minView, maxView
   , findPredecessor, findSuccessor
   , addPrefix, splitPrefix, lookupPrefix
   ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow       ((***), first)
import qualified Data.DList as DL
import Data.DList          (DList)
import Data.List           (foldl', partition)
import Data.Maybe          (fromJust)
import Prelude hiding      (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Classes
   ( Boolable(..)
   , Unwrappable(..)
   , Unionable(..), Differentiable(..), Intersectable(..)
   , Alt(..)
   , fmap', (<$!>)
   )
import Data.Trie.Base.Map (Map, OrdMap)
import Data.Trie.Util     (both)

class (Map map k, Functor st, Unwrappable st)
   => Trie trie st map k | trie -> st where

   mkTrie :: st a -> CMap trie map k a -> trie map k a
   tParts :: trie map k a -> (st a, CMap trie map k a)

type CMap trie map k v = map k (trie map k v)

hasValue, noValue :: Boolable b => b -> Bool
hasValue = toBool
noValue  = not . hasValue

tVal :: Trie trie st map k => trie map k a -> st a
tVal = fst . tParts

tMap :: Trie trie st map k => trie map k a -> CMap trie map k a
tMap = snd . tParts

mapVal :: Trie trie st map k => trie map k a
                             -> (st a -> st a)
                             -> trie map k a
mapVal tr f = mkTrie (f . tVal $ tr) (tMap tr)

mapMap :: (Trie trie st map k1, Trie trie st map k2)
       => trie map k1 a
       -> (CMap trie map k1 a -> CMap trie map k2 a)
       -> trie map k2 a
mapMap tr f = mkTrie (tVal tr) (f . tMap $ tr)

onVals :: Trie trie st map k => (st a -> st b -> st c)
                             -> trie map k a
                             -> trie map k b
                             -> st c
onVals f a b = f (tVal a) (tVal b)

onMaps :: Trie trie st map k => (  CMap trie map k a
                                -> CMap trie map k b
                                -> CMap trie map k c
                                )
                             -> trie map k a
                             -> trie map k b
                             -> CMap trie map k c
onMaps f a b = f (tMap a) (tMap b)

-----------------------

-- O(1)
--
-- Test the strict field last for maximal laziness
null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = Map.null (tMap tr) && (noValue.tVal $ tr)

-- O(n)
size :: (Boolable (st a), Trie trie st map k) => trie map k a -> Int
size tr = Map.foldValues ((+) . size) (fromEnum.hasValue.tVal $ tr) (tMap tr)

-- O(m)
member :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
member k tr = hasValue (lookup k tr)

-- O(m)
notMember :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
notMember k tr = not (member k tr)

-- O(m)
lookup :: (Alt st a, Trie trie st map k) => [k] -> trie map k a -> st a
lookup []     tr = tVal tr
lookup (x:xs) tr = maybe altEmpty (lookup xs) (Map.lookup (tMap tr) x)

-- O(m)
lookupWithDefault :: (Alt st a, Trie trie st map k)
                  => a -> [k] -> trie map k a -> a
lookupWithDefault def k tr = unwrap $ lookup k tr <|> pure def

-- O(min(n1,n2))
isSubmapOfBy :: ( Boolable (st a), Boolable (st b)
                , Alt st Bool
                , Trie trie st map k
                )
             => (st a -> st b -> st Bool)
             -> trie map k a
             -> trie map k b
             -> Bool
isSubmapOfBy f tr1 tr2 =
   let (v1,m1) = tParts tr1
       (v2,m2) = tParts tr2
    in and [ not (hasValue v1 && noValue v2)
           , unwrap $ f v1 v2 <|> pure True
           , Map.isSubmapOfBy (isSubmapOfBy f) m1 m2
           ]

-- O(min(n1,n2))
isProperSubmapOfBy :: ( Boolable (st a), Boolable (st b)
                      , Alt st Bool
                      , Trie trie st map k
                      )
                   => (st a -> st b -> st Bool)
                   -> trie map k a
                   -> trie map k b
                   -> Bool
isProperSubmapOfBy = go False
 where
   go proper f tr1 tr2 =
      let (v1,m1) = tParts tr1
          (v2,m2) = tParts tr2
          -- This seems suboptimal but I can't think of anything better
          proper' = or [ proper
                       , noValue v1 && hasValue v2
                       , not (Map.null $ Map.difference m2 m1)
                       ]
       in and [ not (hasValue v1 && noValue v2)
              , unwrap $ f v1 v2 <|> pure True
              , if Map.null m1
                   then proper'
                   else Map.isSubmapOfBy (go proper' f) m1 m2
              ]

-- * Construction

-- O(1)
empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty Map.empty

-- O(m)
singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton xs v = addPrefix xs $ mkTrie (pure v) Map.empty

-- O(m)
insert :: (Alt st a, Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

-- O(m)
insertWith :: (Alt st a, Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith = genericInsertWith (<$>)

-- O(m)
insertWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
            => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith' = genericInsertWith (<$!>)

genericInsertWith :: (Alt st a, Trie trie st map k)
                  => ((a -> a) -> st a -> st a)
                  -> (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
genericInsertWith (<$$>) f []     new tr =
   mapVal tr $ \old -> (f new <$$> old) <|> pure new

genericInsertWith (<$$>) f (x:xs) val tr = mapMap tr $ \m ->
   Map.insertWith (\_ old -> genericInsertWith (<$$>) f xs val old)
                  m x (singleton xs val)

-- O(m)
delete :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> trie map k a
delete = alter (const altEmpty)

-- O(m)
adjust :: Trie trie st map k
       => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust = genericAdjust fmap

-- O(m)
adjust' :: (Alt st a, Boolable (st a), Trie trie st map k)
        => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust' = genericAdjust fmap'

genericAdjust :: Trie trie st map k
              => ((a -> a) -> st a -> st a)
              -> (a -> a) -> [k] -> trie map k a -> trie map k a
genericAdjust myFmap f []     tr = mapVal tr (myFmap f)
genericAdjust myFmap f (x:xs) tr =
   mapMap tr $ \m -> Map.adjust (genericAdjust myFmap f xs) m x

-- O(m)
updateLookup :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> st a) -> [k] -> trie map k a -> (st a, trie map k a)
updateLookup f [] tr =
   let (v,m) = tParts tr
       v'    = if hasValue v then f (unwrap v) else v
    in (v' <|> v, mkTrie v' m)

updateLookup f (x:xs) orig =
   let m   = tMap orig
       old = Map.lookup m x
    in case old of
            Nothing -> (altEmpty, orig)
            Just tr ->
               let (ret, upd) = updateLookup f xs tr
                in ( ret
                   , mkTrie (tVal orig) $ if null upd
                                             then Map.delete             m x
                                             else Map.adjust (const upd) m x
                   )

-- O(m)
--
-- Lazy in exactly one case: the key is the prefix of another key in the trie.
-- Otherwise we have to test whether the function removed a key or not, lest
-- the trie fall into an invalid state.
alter :: (Alt st a, Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter = genericAlter (flip const)

-- O(m)
alter' :: (Alt st a, Boolable (st a), Trie trie st map k)
       => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter' = genericAlter seq

genericAlter :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (st a -> trie map k a -> trie map k a)
             -> (st a -> st a) -> [k] -> trie map k a -> trie map k a
genericAlter seeq f []     tr =
   let (v,m) = tParts tr
       v'    = f v
    in v' `seeq` mkTrie v' m

genericAlter seeq f (x:xs) tr = mapMap tr $ \m ->
   Map.alter (\mold -> case mold of
                            Nothing ->
                               let v = f altEmpty
                                in if hasValue v
                                      then Just (singleton xs (unwrap v))
                                      else Nothing
                            Just old ->
                               let new = genericAlter seeq f xs old
                                in if null new then Nothing else Just new)
              m x

-- * Combination

-- O(min(m1,m2))
unionWith :: (Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f = genericUnionWith (unionVals f) (flip const)

-- O(min(m1,m2))
unionWith' :: (Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith' f = genericUnionWith (unionVals' f) seq

genericUnionWith :: Trie trie st map k
                 => (st a -> st a -> st a)
                 -> (st a -> trie map k a -> trie map k a)
                 -> trie map k a
                 -> trie map k a
                 -> trie map k a
genericUnionWith valUnion seeq tr1 tr2 =
   let v = onVals valUnion tr1 tr2
    in v `seeq` (
          mkTrie v $
             onMaps (Map.unionWith (genericUnionWith valUnion seeq))
                    tr1 tr2)

-- O(min(m1,m2))
unionWithKey :: (Unionable st a, Trie trie st map k) => ([k] -> a -> a -> a)
                                                     -> trie map k a
                                                     -> trie map k a
                                                     -> trie map k a
unionWithKey = genericUnionWithKey unionVals (flip const)

-- O(min(m1,m2))
unionWithKey' :: (Unionable st a, Trie trie st map k) => ([k] -> a -> a -> a)
                                                      -> trie map k a
                                                      -> trie map k a
                                                      -> trie map k a
unionWithKey' = genericUnionWithKey unionVals' seq

genericUnionWithKey :: Trie trie st map k
                    => ((a -> a -> a) -> st a -> st a -> st a)
                    -> (st a -> trie map k a -> trie map k a)
                    -> ([k] -> a -> a -> a)
                    -> trie map k a
                    -> trie map k a
                    -> trie map k a
genericUnionWithKey = go DL.empty
 where
   go k valUnion seeq f tr1 tr2 =
      let v = onVals (valUnion (f $ DL.toList k)) tr1 tr2
       in v `seeq` (
             mkTrie v $
                onMaps (Map.unionWithKey $
                           \x -> go (k `DL.snoc` x) valUnion seeq f)
                       tr1 tr2)

unionsWith :: (Alt st a, Unionable st a, Trie trie st map k)
           => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith f = foldl' (unionWith f) empty

unionsWith' :: (Alt st a, Unionable st a, Trie trie st map k)
            => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith' f = foldl' (unionWith' f) empty

unionsWithKey :: (Alt st a, Unionable st a, Trie trie st map k)
              => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey j = foldl' (unionWithKey j) empty

unionsWithKey' :: (Alt st a, Unionable st a, Trie trie st map k)
               => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey' j = foldl' (unionWithKey' j) empty

-- O(min(m1,m2))
differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith f tr1 tr2 =
   let v = onVals (differenceVals f) tr1 tr2

       -- This would be lazy only in the case where the differing keys were at
       -- []. (And even then most operations on the trie would force the
       -- value.) For consistency with other keys and Patricia, just seq it for
       -- that case as well.
    in v `seq` mkTrie v $ onMaps (Map.differenceWith (g f)) tr1 tr2
 where
   g f' t1 t2 = let t' = differenceWith f' t1 t2
                 in if null t' then Nothing else Just t'

-- O(min(m1,m2))
differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey = go DL.empty
 where
   go k f tr1 tr2 =
      let v = onVals (differenceVals (f $ DL.toList k)) tr1 tr2

          -- see comment in differenceWith for seq explanation
       in v `seq` mkTrie v $ onMaps (Map.differenceWithKey (g k f)) tr1 tr2

   g k f x t1 t2 = let t' = go (k `DL.snoc` x) f t1 t2
                         in if null t' then Nothing else Just t'

-- O(min(m1,m2))
intersectionWith :: ( Boolable (st c), Intersectable st a b c
                     , Trie trie st map k
                     )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith f = genericIntersectionWith (intersectionVals f) (flip const)

-- O(min(m1,m2))
intersectionWith' :: ( Boolable (st c), Intersectable st a b c
                     , Trie trie st map k
                     )
                  => (a -> b -> c)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k c
intersectionWith' f = genericIntersectionWith (intersectionVals' f) seq

genericIntersectionWith :: (Boolable (st c), Trie trie st map k)
                        => (st a -> st b -> st c)
                        -> (st c -> trie map k c -> trie map k c)
                        -> trie map k a
                        -> trie map k b
                        -> trie map k c
genericIntersectionWith valIntersection seeq tr1 tr2 =
   tr seeq
      (onVals valIntersection tr1 tr2)
      (onMaps (Map.intersectionWith
                 (genericIntersectionWith valIntersection seeq))
              tr1 tr2)
 where
   tr seeq' v m =
      v `seeq'` (mkTrie v $
                    case Map.singletonView m of
                         Just (_, child) | null child -> tMap child
                         _                            -> m)

-- O(min(m1,m2))
intersectionWithKey :: ( Boolable (st c), Intersectable st a b c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey = genericIntersectionWithKey intersectionVals (flip const)

-- O(min(m1,m2))
intersectionWithKey' :: ( Boolable (st c), Intersectable st a b c
                        , Trie trie st map k
                        )
                     => ([k] -> a -> b -> c)
                     -> trie map k a
                     -> trie map k b
                     -> trie map k c
intersectionWithKey' = genericIntersectionWithKey intersectionVals' seq

genericIntersectionWithKey :: (Boolable (st c), Trie trie st map k)
                           => ((a -> b -> c) -> st a -> st b -> st c)
                           -> (st c -> trie map k c -> trie map k c)
                           -> ([k] -> a -> b -> c)
                           -> trie map k a
                           -> trie map k b
                           -> trie map k c
genericIntersectionWithKey = go DL.empty
 where
   go k valIntersection seeq f tr1 tr2 =
      tr seeq
         (onVals (valIntersection (f $ DL.toList k)) tr1 tr2)
         (onMaps (Map.intersectionWithKey $
                     \x -> go (k `DL.snoc` x) valIntersection seeq f)
                 tr1 tr2)

   tr seeq v m =
      v `seeq` (mkTrie v $
                   case Map.singletonView m of
                        Just (_, child) | null child -> tMap child
                        _                            -> m)

-- O(n m)
filterWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
              => ([k] -> a -> Bool) -> trie map k a -> trie map k a
filterWithKey p = fromList . Prelude.filter (uncurry p) . toList

-- O(n m)
partitionWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => ([k] -> a -> Bool)
                 -> trie map k a
                 -> (trie map k a, trie map k a)
partitionWithKey p = both fromList . partition (uncurry p) . toList

-- O(m)
split :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
      => [k] -> trie map k a -> (trie map k a, trie map k a)
split xs tr = let (l,_,g) = splitLookup xs tr in (l,g)

-- O(m)
splitLookup :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
            => [k]
            -> trie map k a
            -> (trie map k a, st a, trie map k a)
splitLookup []     tr = (empty, tVal tr, mkTrie altEmpty (tMap tr))
splitLookup (x:xs) tr =
   let (v,m) = tParts tr
       (ml, subTr, mg) = Map.splitLookup m x
    in case subTr of
            Nothing  -> (mkTrie v ml, altEmpty, mkTrie altEmpty mg)
            Just tr' ->
               let (tl, v', tg) = splitLookup xs tr'
                   ml' = if null tl then ml else Map.insert ml x tl
                   mg' = if null tg then mg else Map.insert mg x tg
                in (mkTrie v ml', v', mkTrie altEmpty mg')

-- * Mapping

-- O(n m)
mapKeysWith :: (Boolable (st a), Trie trie st map k1, Trie trie st map k2)
            => ([([k2],a)] -> trie map k2 a)
            -> ([k1] -> [k2])
            -> trie map k1 a
            -> trie map k2 a
mapKeysWith fromlist f = fromlist . Prelude.map (first f) . toList

-- O(n)
-- TODO: needs a name!
mapKeys'With :: (Unionable st a, Trie trie st map k1, Trie trie st map k2)
             => (a -> a -> a)
             -> (k1 -> k2)
             -> trie map k1 a
             -> trie map k2 a
mapKeys'With j f tr =
   mapMap tr $
      Map.fromListWith (unionWith j) .
         Prelude.map (f *** mapKeys'With j f) .
      Map.toList

-- * Folding

-- O(n)
foldrWithKey :: (Boolable (st a), Trie trie st map k)
             => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrWithKey f x = Prelude.foldr (uncurry f) x . toList

-- O(n)
foldrAscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrAscWithKey f x = Prelude.foldr (uncurry f) x . toAscList

-- O(n)
foldrDescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrDescWithKey f x = Prelude.foldr (uncurry f) x . toDescList

-- O(n)
foldl'WithKey :: (Boolable (st a), Trie trie st map k)
              => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'WithKey f x = foldl' (flip $ uncurry f) x . toList

-- O(n)
foldl'AscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'AscWithKey f x = foldl' (flip $ uncurry f) x . toAscList

-- O(n)
foldl'DescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                  => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'DescWithKey f x = foldl' (flip $ uncurry f) x . toDescList

-- * Conversion between lists

-- O(n)
toList :: (Boolable (st a), Trie trie st map k) => trie map k a -> [([k],a)]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: (Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> [([k],a)]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: (Boolable (st a), Trie trie st map k, OrdMap map k)
           => trie map k a -> [([k],a)]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: (Boolable (st a), Trie trie st map k)
              => (CMap trie map k a -> [(k, trie map k a)])
              -> (([k],a) -> DList ([k],a) -> DList ([k],a))
              -> trie map k a
              -> [([k],a)]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go xs tolist add tr =
      let (v,m) = tParts tr
          xs'   =
             DL.concat .
             Prelude.map (\(x,t) -> go (xs `DL.snoc` x) tolist add t) .
             tolist $ m
       in if hasValue v
             then add (DL.toList xs, unwrap v) xs'
             else                              xs'

-- O(n m)
fromList :: (Alt st a, Trie trie st map k) => [([k],a)] -> trie map k a
fromList = fromListWith const

-- O(n m)
fromListWith :: (Alt st a, Trie trie st map k)
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

-- O(n m)
fromListWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
              => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith' f = foldl' (flip . uncurry $ insertWith' f) empty

-- O(n m)
fromListWithKey :: (Alt st a, Trie trie st map k)
                => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey f = foldl' (\tr (k,v) -> insertWith (f k) k v tr) empty

-- O(n m)
fromListWithKey' :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey' f = foldl' (\tr (k,v) -> insertWith' (f k) k v tr) empty

-- * Min/max

-- O(m)
findMin :: (Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe ([k], a)
findMin = findMinMax (hasValue . tVal) (fst . Map.minViewWithKey)

-- O(m)
findMax :: (Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe ([k], a)
findMax = findMinMax (Map.null . tMap) (fst . Map.maxViewWithKey)

findMinMax :: (Boolable (st a), Trie trie st map k)
           => (trie map k a -> Bool)
           -> (CMap trie map k a -> Maybe (k, trie map k a))
           -> trie map k a
           -> Maybe ([k], a)
findMinMax _ _ tr_ | null tr_ = Nothing
findMinMax f g tr_ = Just (go f g DL.empty tr_)
 where
   go isWanted mapView xs tr =
      if isWanted tr
         then (DL.toList xs, unwrap (tVal tr))
         else let (k, tr') = fromJust . mapView . tMap $ tr
               in go isWanted mapView (xs `DL.snoc` k) tr'

-- O(m)
deleteMin :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMin = snd . minView

-- O(m)
deleteMax :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMax = snd . maxView

-- O(m)
minView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> (Maybe ([k], a), trie map k a)
minView = minMaxView (hasValue . tVal) (fst . Map.minViewWithKey)

-- O(m)
maxView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> (Maybe ([k], a), trie map k a)
maxView = minMaxView (Map.null . tMap) (fst . Map.maxViewWithKey)

minMaxView :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (trie map k a -> Bool)
           -> (CMap trie map k a -> Maybe (k, trie map k a))
           -> trie map k a
           -> (Maybe ([k], a), trie map k a)
minMaxView _ _ tr_ | null tr_ = (Nothing, tr_)
minMaxView f g tr_ = first Just (go f g tr_)
 where
   go isWanted mapView tr =
      let (v,m) = tParts tr
       in if isWanted tr
             then (([], unwrap v), mkTrie altEmpty m)
             else let (k,      tr')  = fromJust (mapView m)
                      (minMax, tr'') = go isWanted mapView tr'
                   in ( first (k:) minMax
                      , mkTrie v $ if null tr''
                                      then Map.delete              m k
                                      else Map.adjust (const tr'') m k
                      )

-- O(m)
findPredecessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => trie map k a -> [k] -> Maybe ([k], a)
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go _  [] = Nothing

   -- We need to try the trie at x and then the trie at the predecessor of x:
   -- e.g. if looking for "foo", we need to try any 'f' branch to see if it has
   -- "fob" first, before grabbing the next-best option of the maximum of the
   -- 'b' branch, say "bar".
   --
   -- If there's no branch less than 'f' we try the current position as a last
   -- resort.
   go tr (x:xs) =
      let (v,m) = tParts tr
          predecessor = Map.findPredecessor m x
       in fmap (first (x:)) (Map.lookup m x >>= flip go xs)
          <|>
          case predecessor of
               Nothing         ->
                  if hasValue v
                     then Just ([], unwrap v)
                     else Nothing
               Just (best,btr) -> fmap (first (best:)) (findMax btr)

-- O(m)
findSuccessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
              => trie map k a -> [k] -> Maybe ([k], a)
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go tr [] = do (k,t) <- fst . Map.minViewWithKey . tMap $ tr
                 fmap (first (k:)) (findMin t)

   go tr (x:xs) =
      let m = tMap tr
          successor = Map.findSuccessor m x
       in fmap (first (x:)) (Map.lookup m x >>= flip go xs)
          <|>
          (successor >>= \(best,btr) -> fmap (first (best:)) (findMin btr))

-- * Trie-only operations

-- O(s) where s is the input
addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix []     = id
addPrefix (x:xs) = mkTrie altEmpty . Map.singleton x . addPrefix xs

-- O(m)
splitPrefix :: (Alt st a, Trie trie st map k)
            => trie map k a -> ([k], trie map k a)
splitPrefix = go DL.empty
 where
   go xs tr =
      case Map.singletonView (tMap tr) of
           Just (x,tr') -> go (xs `DL.snoc` x) tr'
           Nothing      -> (DL.toList xs, tr)

-- O(m)
lookupPrefix :: (Alt st a, Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
lookupPrefix []     tr = tr
lookupPrefix (x:xs) tr =
   case Map.lookup (tMap tr) x of
        Nothing  -> empty
        Just tr' -> lookupPrefix xs tr'
