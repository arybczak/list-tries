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
   , differenceWith, differenceWithKey, differenceWith', differenceWithKey'
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

null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = (noValue.tVal $ tr) && Map.null (tMap tr)

size :: (Boolable (st a), Trie trie st map k) => trie map k a -> Int
size tr = Map.foldValues ((+) . size) (fromEnum.hasValue.tVal $ tr) (tMap tr)

member :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
member k tr = hasValue (lookup k tr)

notMember :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
notMember k tr = not (member k tr)

lookup :: (Alt st a, Trie trie st map k) => [k] -> trie map k a -> st a
lookup []     tr = tVal tr
lookup (x:xs) tr = maybe altEmpty (lookup xs) (Map.lookup (tMap tr) x)

lookupWithDefault :: (Alt st a, Trie trie st map k)
                  => a -> [k] -> trie map k a -> a
lookupWithDefault def k tr = unwrap $ lookup k tr <|> pure def

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

empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty Map.empty

singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton xs v = addPrefix xs $ mkTrie (pure v) Map.empty

insert :: (Alt st a, Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

insertWith :: (Alt st a, Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith = genericInsertWith (<$>)

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

delete :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> trie map k a
delete = alter (const altEmpty)

adjust :: Trie trie st map k
       => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust = genericAdjust fmap

adjust' :: (Alt st a, Boolable (st a), Trie trie st map k)
        => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust' = genericAdjust fmap'

genericAdjust :: Trie trie st map k
              => ((a -> a) -> st a -> st a)
              -> (a -> a) -> [k] -> trie map k a -> trie map k a
genericAdjust myFmap f []     tr = mapVal tr (myFmap f)
genericAdjust myFmap f (x:xs) tr =
   mapMap tr $ \m -> Map.adjust (genericAdjust myFmap f xs) m x

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

alter :: (Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter = genericAlter (flip const)

alter' :: (Boolable (st a), Trie trie st map k)
       => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter' = genericAlter seq

genericAlter :: (Boolable (st a), Trie trie st map k)
             => (st a -> trie map k a -> trie map k a)
             -> (st a -> st a) -> [k] -> trie map k a -> trie map k a
genericAlter seeq f []     tr =
   let (v,m) = tParts tr
       v'    = f v
    in v' `seeq` mkTrie v' m

genericAlter seeq f (x:xs) tr = mapMap tr $ \m ->
   Map.update (\old -> let new = genericAlter seeq f xs old
                        in if null new then Nothing else Just new)
              m x

-- * Combination

unionWith :: (Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f = genericUnionWith (unionVals f) (flip const)

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

unionWithKey :: (Unionable st a, Trie trie st map k) => ([k] -> a -> a -> a)
                                                     -> trie map k a
                                                     -> trie map k a
                                                     -> trie map k a
unionWithKey = genericUnionWithKey unionVals (flip const)

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

differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith = genericDifferenceWith (flip const)

differenceWith' :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
                => (a -> b -> Maybe a)
                -> trie map k a
                -> trie map k b
                -> trie map k a
differenceWith' = genericDifferenceWith seq

genericDifferenceWith :: ( Boolable (st a), Differentiable st a b
                         , Trie trie st map k
                         )
                      => (st a -> trie map k a -> trie map k a)
                      -> (a -> b -> Maybe a)
                      -> trie map k a
                      -> trie map k b
                      -> trie map k a
genericDifferenceWith seeq f tr1 tr2 =
   let v = onVals (differenceVals f) tr1 tr2
    in v `seeq` (mkTrie v $ onMaps (Map.differenceWith (g seeq f)) tr1 tr2)
 where
   g seeq' f' t1 t2 = let t' = genericDifferenceWith seeq' f' t1 t2
                       in if null t' then Nothing else Just t'

differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey = genericDifferenceWithKey (flip const)

differenceWithKey' :: ( Boolable (st a), Differentiable st a b
                      , Trie trie st map k
                      )
                   => ([k] -> a -> b -> Maybe a)
                   -> trie map k a
                   -> trie map k b
                   -> trie map k a
differenceWithKey' = genericDifferenceWithKey seq

genericDifferenceWithKey :: ( Boolable (st a), Differentiable st a b
                            , Trie trie st map k
                            )
                         => (st a -> trie map k a -> trie map k a)
                         -> ([k] -> a -> b -> Maybe a)
                         -> trie map k a
                         -> trie map k b
                         -> trie map k a
genericDifferenceWithKey = go DL.empty
 where
   go k seeq f tr1 tr2 =
      let v = onVals (differenceVals (f $ DL.toList k)) tr1 tr2
       in v `seeq` (mkTrie v $
                       onMaps (Map.differenceWithKey (g k seeq f)) tr1 tr2)

   g k seeq f x t1 t2 = let t' = go (k `DL.snoc` x) seeq f t1 t2
                         in if null t' then Nothing else Just t'

intersectionWith :: ( Boolable (st c), Intersectable st a b c
                     , Trie trie st map k
                     )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith f = genericIntersectionWith (intersectionVals f) (flip const)

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

intersectionWithKey :: ( Boolable (st c), Intersectable st a b c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey = genericIntersectionWithKey intersectionVals (flip const)

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

filterWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
              => ([k] -> a -> Bool) -> trie map k a -> trie map k a
filterWithKey p = fromList . Prelude.filter (uncurry p) . toList

partitionWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => ([k] -> a -> Bool)
                 -> trie map k a
                 -> (trie map k a, trie map k a)
partitionWithKey p = both fromList . partition (uncurry p) . toList

split :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
      => [k] -> trie map k a -> (trie map k a, trie map k a)
split xs tr = let (l,_,g) = splitLookup xs tr in (l,g)

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

mapKeysWith :: (Boolable (st a), Trie trie st map k1, Trie trie st map k2)
            => ([([k2],a)] -> trie map k2 a)
            -> ([k1] -> [k2])
            -> trie map k1 a
            -> trie map k2 a
mapKeysWith fromlist f = fromlist . Prelude.map (first f) . toList

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

foldrWithKey :: (Boolable (st a), Trie trie st map k)
             => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrWithKey f x = Prelude.foldr (uncurry f) x . toList

foldrAscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrAscWithKey f x = Prelude.foldr (uncurry f) x . toAscList

foldrDescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrDescWithKey f x = Prelude.foldr (uncurry f) x . toDescList

foldl'WithKey :: (Boolable (st a), Trie trie st map k)
              => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'WithKey f x = foldl' (flip $ uncurry f) x . toList

foldl'AscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'AscWithKey f x = foldl' (flip $ uncurry f) x . toAscList

foldl'DescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                  => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldl'DescWithKey f x = foldl' (flip $ uncurry f) x . toDescList

-- * Conversion between lists

toList :: (Boolable (st a), Trie trie st map k) => trie map k a -> [([k],a)]
toList = genericToList Map.toList DL.cons

toAscList :: (Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> [([k],a)]
toAscList = genericToList Map.toAscList DL.cons

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

fromList :: (Alt st a, Trie trie st map k) => [([k],a)] -> trie map k a
fromList = fromListWith const

fromListWith :: (Alt st a, Trie trie st map k)
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

fromListWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
              => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith' f = foldl' (flip . uncurry $ insertWith' f) empty

fromListWithKey :: (Alt st a, Trie trie st map k)
                => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey f = foldl' (\tr (k,v) -> insertWith (f k) k v tr) empty

fromListWithKey' :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey' f = foldl' (\tr (k,v) -> insertWith' (f k) k v tr) empty

-- * Min/max

findMin :: (Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe ([k], a)
findMin = findMinMax (hasValue.tVal) (fst . Map.minViewWithKey)

findMax :: (Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe ([k], a)
findMax = findMinMax (Map.null.tMap) (fst . Map.maxViewWithKey)

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

deleteMin :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMin = maybe empty snd . minView

deleteMax :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMax = maybe empty snd . maxView

minView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe (([k], a), trie map k a)
minView = minMaxView (hasValue.tVal) (fst . Map.minViewWithKey)

maxView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe (([k], a), trie map k a)
maxView = minMaxView (Map.null.tMap) (fst . Map.maxViewWithKey)

minMaxView :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (trie map k a -> Bool)
           -> (CMap trie map k a -> Maybe (k, trie map k a))
           -> trie map k a
           -> Maybe (([k], a), trie map k a)
minMaxView _ _ tr_ | null tr_ = Nothing
minMaxView f g tr_ = Just (go f g tr_)
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

addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix []     = id
addPrefix (x:xs) = mkTrie altEmpty . Map.singleton x . addPrefix xs

splitPrefix :: (Alt st a, Trie trie st map k)
            => trie map k a -> ([k], trie map k a)
splitPrefix = go DL.empty
 where
   go xs tr =
      case Map.singletonView (tMap tr) of
           Just (x,tr') -> go (xs `DL.snoc` x) tr'
           Nothing      -> (DL.toList xs, tr)

lookupPrefix :: (Alt st a, Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
lookupPrefix = go DL.empty
 where
   go pr []     tr = addPrefix (DL.toList pr) tr
   go pr (x:xs) tr =
      case Map.lookup (tMap tr) x of
           Nothing  -> empty
           Just tr' -> go (pr `DL.snoc` x) xs tr'
