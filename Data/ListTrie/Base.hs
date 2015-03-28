-- File created: 2008-11-13 21:13:55

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, Rank2Types #-}

module Data.ListTrie.Base
   ( Trie(..)
   , null, size, size', member, notMember, lookup, lookupWithDefault
   , isSubmapOfBy, isProperSubmapOfBy
   , empty, singleton
   , insert, insert', insertWith, insertWith'
   , delete, adjust, adjust', updateLookup, alter, alter'
   , unionWith, unionWithKey, unionWith', unionWithKey'
   , unionsWith, unionsWithKey, unionsWith', unionsWithKey'
   , differenceWith, differenceWithKey
   , intersectionWith,  intersectionWithKey
   , intersectionWith', intersectionWithKey'
   , filterWithKey, partitionWithKey
   , split, splitLookup
   , mapKeysWith, mapInKeysWith, mapInKeysWith'
   , foldrWithKey,  foldrAscWithKey,  foldrDescWithKey
   , foldlWithKey,  foldlAscWithKey,  foldlDescWithKey
   , foldlWithKey', foldlAscWithKey', foldlDescWithKey'
   , toList, toAscList, toDescList
   , fromList, fromListWith, fromListWith', fromListWithKey, fromListWithKey'
   , findMin, findMax, deleteMin, deleteMax, minView, maxView
   , findPredecessor, findSuccessor
   , lookupPrefix, addPrefix, deletePrefix, deleteSuffixes
   , splitPrefix, children, children1
   , showTrieWith
   ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow       ((***), first)
import qualified Data.DList as DL
import Data.DList          (DList)
import Data.Foldable       (foldr, foldl')
import Data.List           (partition)
import Data.Maybe          (fromJust)
import Prelude hiding      (lookup, filter, foldr, null)
import qualified Prelude

import qualified Data.ListTrie.Base.Map.Internal as Map
import Data.ListTrie.Base.Classes
   ( Boolable(..)
   , Unwrappable(..)
   , Unionable(..), Differentiable(..), Intersectable(..)
   , Alt(..)
   , fmap', (<$!>)
   )
import Data.ListTrie.Base.Map (Map, OrdMap)
import Data.ListTrie.Util     ((.:), both)

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

mapVal :: Trie trie st map k => (forall x y. (x -> y) -> x -> y)
                             -> trie map k a
                             -> (st a -> st a)
                             -> trie map k a
mapVal ($$) tr f = (mkTrie $$ (f . tVal $ tr)) (tMap tr)

mapMap :: (Trie trie st map k1, Trie trie st map k2)
       => (forall x y. (x -> y) -> x -> y)
       -> trie map k1 a
       -> (CMap trie map k1 a -> CMap trie map k2 a)
       -> trie map k2 a
mapMap ($$) tr f = (mkTrie $$ tVal tr) (f . tMap $ tr)

onVals :: Trie trie st map k => (forall x y. (x -> y) -> x -> y)
                             -> (st a -> st b -> st c)
                             -> trie map k a
                             -> trie map k b
                             -> st c
onVals ($$) f a b = f $$ tVal a $$ tVal b

onMaps :: Trie trie st map k => (forall x y. (x -> y) -> x -> y)
                             -> (  CMap trie map k a
                                -> CMap trie map k b
                                -> CMap trie map k c
                                )
                             -> trie map k a
                             -> trie map k b
                             -> CMap trie map k c
onMaps ($$) f a b = f $$ tMap a $$ tMap b

-----------------------

-- * Construction

-- O(1)
empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty Map.empty

-- O(s)
singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton xs v = addPrefix xs $ mkTrie (pure v) Map.empty

-- O(min(m,s))
insert :: (Alt st a, Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

-- O(min(m,s))
insert' :: (Alt st a, Boolable (st a), Trie trie st map k)
        => [k] -> a -> trie map k a -> trie map k a
insert' = insertWith' const

-- O(min(m,s))
insertWith :: (Alt st a, Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith = genericInsertWith ($) (<$>)

-- O(min(m,s))
insertWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
            => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith' = (seq <*>) .: genericInsertWith ($!) (<$!>)

genericInsertWith :: (Alt st a, Trie trie st map k)
                  => (forall x y. (x -> y) -> x -> y)
                  -> ((a -> a) -> st a -> st a)
                  -> (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
genericInsertWith ($$) (<$$>) f = go
 where
   go []     new tr =
      mapVal ($$) tr $ \old -> (f new <$$> old) <|> pure new

   go (x:xs) val tr = mapMap ($$) tr $ \m ->
      Map.insertWith (\_ old -> go xs val old) x (singleton xs val) m

-- O(min(m,s))
delete :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> trie map k a
delete = alter (const altEmpty)

-- O(min(m,s))
adjust :: Trie trie st map k
       => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust = genericAdjust ($) fmap

-- O(min(m,s))
adjust' :: (Alt st a, Boolable (st a), Trie trie st map k)
        => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust' = genericAdjust ($!) fmap'

genericAdjust :: Trie trie st map k
              => (forall x y. (x -> y) -> x -> y)
              -> ((a -> a) -> st a -> st a)
              -> (a -> a) -> [k] -> trie map k a -> trie map k a
genericAdjust ($$) (<$$>) f = go
 where
   go []     tr = mapVal ($$) tr (f <$$>)
   go (x:xs) tr = mapMap ($$) tr (Map.adjust (go xs) x)

-- O(min(m,s))
updateLookup :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> st a) -> [k] -> trie map k a -> (st a, trie map k a)
updateLookup f = go
 where
   go [] tr =
      let (v,m) = tParts tr
          v'    = if hasValue v then f (unwrap v) else v
       in (v, mkTrie v' m)

   go (x:xs) orig =
      let m   = tMap orig
       in case Map.lookup x m of
               Nothing -> (altEmpty, orig)
               Just tr ->
                  let (ret, upd) = go xs tr
                   in ( ret
                      , mkTrie (tVal orig) $ if null upd
                                                then Map.delete             x m
                                                else Map.adjust (const upd) x m
                      )

-- O(min(m,s))
--
-- Lazy in exactly one case: the key is the prefix of another key in the trie.
-- Otherwise we have to test whether the function removed a key or not, lest
-- the trie fall into an invalid state.
alter :: (Alt st a, Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter = genericAlter ($) (flip const)

-- O(min(m,s))
alter' :: (Alt st a, Boolable (st a), Trie trie st map k)
       => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter' = genericAlter ($!) seq

genericAlter :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (forall x y. (x -> y) -> x -> y)
             -> (st a -> trie map k a -> trie map k a)
             -> (st a -> st a) -> [k] -> trie map k a -> trie map k a
genericAlter ($$) seeq f = go
 where
   go []     tr =
      let (v,m) = tParts tr
          v'    = f v
       in v' `seeq` mkTrie v' m

   go (x:xs) tr = mapMap ($$) tr $ \m ->
      Map.alter (\mold -> case mold of
                               Nothing ->
                                  let v = f altEmpty
                                   in if hasValue v
                                         then Just (singleton xs (unwrap v))
                                         else Nothing
                               Just old ->
                                  let new = go xs old
                                   in if null new then Nothing else Just new)
                 x m

-- * Querying

-- O(1)
--
-- Test the strict field last for maximal laziness
null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = Map.null (tMap tr) && (noValue.tVal $ tr)

-- O(n m)
size :: (Boolable (st a), Trie trie st map k, Num n) => trie map k a -> n
size  tr = foldr  ((+) . size)  (if hasValue (tVal tr) then 1 else 0) (tMap tr)

-- O(n m)
size' :: (Boolable (st a), Trie trie st map k, Num n) => trie map k a -> n
size' tr = foldl' (flip $ (+) . size')
                  (if hasValue (tVal tr) then 1 else 0)
                  (tMap tr)

-- O(min(m,s))
member :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
member = hasValue .: lookup

-- O(min(m,s))
notMember :: (Alt st a, Boolable (st a), Trie trie st map k)
          => [k] -> trie map k a -> Bool
notMember = not .: member

-- O(min(m,s))
lookup :: (Alt st a, Trie trie st map k) => [k] -> trie map k a -> st a
lookup []     tr = tVal tr
lookup (x:xs) tr = maybe altEmpty (lookup xs) (Map.lookup x (tMap tr))

-- O(min(m,s))
lookupWithDefault :: (Alt st a, Trie trie st map k)
                  => a -> [k] -> trie map k a -> a
lookupWithDefault def k tr = unwrap $ lookup k tr <|> pure def

-- O(min(n1 m1,n2 m2))
isSubmapOfBy :: (Boolable (st a), Boolable (st b), Trie trie st map k)
             => (a -> b -> Bool)
             -> trie map k a
             -> trie map k b
             -> Bool
isSubmapOfBy f = go
 where
   go tr1 tr2 =
      let (v1,m1) = tParts tr1
          (v2,m2) = tParts tr2
          hv1     = hasValue v1
          hv2     = hasValue v2
       in and [ not (hv1 && not hv2)
              , (not hv1 && not hv2) || f (unwrap v1) (unwrap v2)
              , Map.isSubmapOfBy go m1 m2
              ]

-- O(min(n1 m1,n2 m2))
isProperSubmapOfBy :: (Boolable (st a), Boolable (st b), Trie trie st map k)
                   => (a -> b -> Bool)
                   -> trie map k a
                   -> trie map k b
                   -> Bool
isProperSubmapOfBy f = go False
 where
   go proper tr1 tr2 =
      let (v1,m1) = tParts tr1
          (v2,m2) = tParts tr2
          hv1     = hasValue v1
          hv2     = hasValue v2
          -- This seems suboptimal but I can't think of anything better
          proper' = or [ proper
                       , noValue v1 && hasValue v2
                       , not (Map.null $ Map.difference m2 m1)
                       ]
       in and [ not (hv1 && not hv2)
              , (not hv1 && not hv2) || f (unwrap v1) (unwrap v2)
              , if Map.null m1
                   then proper'
                   else Map.isSubmapOfBy (go proper') m1 m2
              ]


-- * Combination

-- O(min(n1 m1,n2 m2))
unionWith :: (Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f = genericUnionWith ($) (unionVals f) (flip const)

-- O(min(n1 m1,n2 m2))
unionWith' :: (Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith' f = genericUnionWith ($!) (unionVals' f) seq

genericUnionWith :: Trie trie st map k
                 => (forall x y. (x -> y) -> x -> y)
                 -> (st a -> st a -> st a)
                 -> (st a -> trie map k a -> trie map k a)
                 -> trie map k a
                 -> trie map k a
                 -> trie map k a
genericUnionWith ($$) valUnion seeq = go
 where
   go tr1 tr2 =
      let v = onVals ($$) valUnion tr1 tr2
       in v `seeq` (mkTrie v $ onMaps ($$) (Map.unionWith go) tr1 tr2)

-- O(min(n1 m1,n2 m2))
unionWithKey :: (Unionable st a, Trie trie st map k) => ([k] -> a -> a -> a)
                                                     -> trie map k a
                                                     -> trie map k a
                                                     -> trie map k a
unionWithKey = genericUnionWithKey ($) unionVals (flip const)

-- O(min(n1 m1,n2 m2))
unionWithKey' :: (Unionable st a, Trie trie st map k) => ([k] -> a -> a -> a)
                                                      -> trie map k a
                                                      -> trie map k a
                                                      -> trie map k a
unionWithKey' = genericUnionWithKey ($!) unionVals' seq

genericUnionWithKey :: Trie trie st map k
                    => (forall x y. (x -> y) -> x -> y)
                    -> ((a -> a -> a) -> st a -> st a -> st a)
                    -> (st a -> trie map k a -> trie map k a)
                    -> ([k] -> a -> a -> a)
                    -> trie map k a
                    -> trie map k a
                    -> trie map k a
genericUnionWithKey ($$) valUnion seeq f = go DL.empty
 where
   go k tr1 tr2 =
      let v = onVals ($$) (valUnion (f $ DL.toList k)) tr1 tr2
       in v `seeq` (mkTrie v $
                       onMaps ($$) (Map.unionWithKey $ go . (k `DL.snoc`))
                              tr1 tr2)

-- O(sum(n))
unionsWith :: (Alt st a, Unionable st a, Trie trie st map k)
           => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith f = foldl' (unionWith f) empty

-- O(sum(n))
unionsWith' :: (Alt st a, Unionable st a, Trie trie st map k)
            => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith' f = foldl' (unionWith' f) empty

-- O(sum(n))
unionsWithKey :: (Alt st a, Unionable st a, Trie trie st map k)
              => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey j = foldl' (unionWithKey j) empty

-- O(sum(n))
unionsWithKey' :: (Alt st a, Unionable st a, Trie trie st map k)
               => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey' j = foldl' (unionWithKey' j) empty

-- O(min(n1 m1,n2 m2))
differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith f = go
 where
   go tr1 tr2 =
      let v = onVals ($!) (differenceVals f) tr1 tr2

          -- This would be lazy only in the case where the differing keys were at
          -- []. (And even then most operations on the trie would force the
          -- value.) For consistency with other keys and Patricia, just seq it for
          -- that case as well.
       in v `seq` mkTrie v $ onMaps ($!) (Map.differenceWith g) tr1 tr2

   g t1 t2 = let t' = go t1 t2
              in if null t' then Nothing else Just t'

-- O(min(n1 m1,n2 m2))
differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey f = go DL.empty
 where
   go k tr1 tr2 =
      let v = onVals ($!) (differenceVals (f $ DL.toList k)) tr1 tr2

          -- see comment in differenceWith for seq explanation
       in v `seq` mkTrie v $
                     onMaps ($!) (Map.differenceWithKey (g k)) tr1 tr2

   g k x t1 t2 = let t' = go (k `DL.snoc` x) t1 t2
                  in if null t' then Nothing else Just t'

-- O(min(n1 m1,n2 m2))
intersectionWith :: ( Boolable (st c), Intersectable st a b c
                    , Trie trie st map k
                    )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith f = genericIntersectionWith ($) (intersectionVals f) (flip const)

-- O(min(n1 m1,n2 m2))
intersectionWith' :: ( Boolable (st c), Intersectable st a b c
                     , Trie trie st map k
                     )
                  => (a -> b -> c)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k c
intersectionWith' f = genericIntersectionWith ($!) (intersectionVals' f) seq

genericIntersectionWith :: (Boolable (st c), Trie trie st map k)
                        => (forall x y. (x -> y) -> x -> y)
                        -> (st a -> st b -> st c)
                        -> (st c -> trie map k c -> trie map k c)
                        -> trie map k a
                        -> trie map k b
                        -> trie map k c
genericIntersectionWith ($$) valIntersection seeq = go
 where
   go tr1 tr2 =
      tr seeq
         (onVals ($$) valIntersection tr1 tr2)
         (onMaps ($$) (Map.filter (not.null) .: Map.intersectionWith go)
                 tr1 tr2)

   tr seeq' v m =
      v `seeq'` (mkTrie v $
                    case Map.singletonView m of
                         Just (_, child) | null child -> tMap child
                         _                            -> m)

-- O(min(n1 m1,n2 m2))
intersectionWithKey :: ( Boolable (st c), Intersectable st a b c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey =
   genericIntersectionWithKey ($) intersectionVals (flip const)

-- O(min(n1 m1,n2 m2))
intersectionWithKey' :: ( Boolable (st c), Intersectable st a b c
                        , Trie trie st map k
                        )
                     => ([k] -> a -> b -> c)
                     -> trie map k a
                     -> trie map k b
                     -> trie map k c
intersectionWithKey' = genericIntersectionWithKey ($!) intersectionVals' seq

genericIntersectionWithKey :: (Boolable (st c), Trie trie st map k)
                           => (forall x y. (x -> y) -> x -> y)
                           -> ((a -> b -> c) -> st a -> st b -> st c)
                           -> (st c -> trie map k c -> trie map k c)
                           -> ([k] -> a -> b -> c)
                           -> trie map k a
                           -> trie map k b
                           -> trie map k c
genericIntersectionWithKey ($$) valIntersection seeq f = go DL.empty
 where
   go k tr1 tr2 =
      tr
         (onVals ($$) (valIntersection (f $ DL.toList k)) tr1 tr2)
         (onMaps ($$) (Map.filter (not.null) .:
                          Map.intersectionWithKey (go . (k `DL.snoc`)))
                 tr1 tr2)

   tr v m =
      v `seeq` (mkTrie v $
                   case Map.singletonView m of
                        Just (_, child) | null child -> tMap child
                        _                            -> m)

-- * Filtering

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

-- * Mapping

-- O(n m)
mapKeysWith :: (Boolable (st a), Trie trie st map k1, Trie trie st map k2)
            => ([([k2],a)] -> trie map k2 a)
            -> ([k1] -> [k2])
            -> trie map k1 a
            -> trie map k2 a
mapKeysWith fromlist f = fromlist . map (first f) . toList

-- O(n m)
mapInKeysWith :: (Unionable st a, Trie trie st map k1, Trie trie st map k2)
              => (a -> a -> a)
              -> (k1 -> k2)
              -> trie map k1 a
              -> trie map k2 a
mapInKeysWith = genericMapInKeysWith ($) unionWith

-- O(n m)
mapInKeysWith' :: (Unionable st a, Trie trie st map k1, Trie trie st map k2)
               => (a -> a -> a)
               -> (k1 -> k2)
               -> trie map k1 a
               -> trie map k2 a
mapInKeysWith' = genericMapInKeysWith ($!) unionWith'

genericMapInKeysWith :: ( Unionable st a
                        , Trie trie st map k1, Trie trie st map k2
                        )
                     => (forall x y. (x -> y) -> x -> y)
                     -> (f -> trie map k2 a -> trie map k2 a -> trie map k2 a)
                     -> f
                     -> (k1 -> k2)
                     -> trie map k1 a
                     -> trie map k2 a
genericMapInKeysWith ($$) unionW j f = go
 where
   go tr = mapMap ($$) tr $
              Map.fromListKVWith (unionW j) . map (f *** go) . Map.toListKV

-- * Folding

-- O(n m)
foldrWithKey :: (Boolable (st a), Trie trie st map k)
             => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrWithKey f x = foldr (uncurry f) x . toList

-- O(n m)
foldrAscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrAscWithKey f x = foldr (uncurry f) x . toAscList

-- O(n m)
foldrDescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldrDescWithKey f x = foldr (uncurry f) x . toDescList

-- O(n m)
foldlWithKey :: (Boolable (st a), Trie trie st map k)
             => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlWithKey f x = foldl (flip $ uncurry f) x . toList

-- O(n m)
foldlAscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlAscWithKey f x = foldl (flip $ uncurry f) x . toAscList

-- O(n m)
foldlDescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlDescWithKey f x = foldl (flip $ uncurry f) x . toDescList

-- O(n m)
foldlWithKey' :: (Boolable (st a), Trie trie st map k)
              => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlWithKey' f x = foldl' (flip $ uncurry f) x . toList

-- O(n m)
foldlAscWithKey' :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                 => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlAscWithKey' f x = foldl' (flip $ uncurry f) x . toAscList

-- O(n m)
foldlDescWithKey' :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                  => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldlDescWithKey' f x = foldl' (flip $ uncurry f) x . toDescList

-- * Conversion between lists

-- O(n m)
toList :: (Boolable (st a), Trie trie st map k) => trie map k a -> [([k],a)]
toList = genericToList Map.toListKV DL.cons

-- O(n m)
toAscList :: (Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> [([k],a)]
toAscList = genericToList Map.toAscList DL.cons

-- O(n m)
toDescList :: (Boolable (st a), Trie trie st map k, OrdMap map k)
           => trie map k a -> [([k],a)]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: (Boolable (st a), Trie trie st map k)
              => (CMap trie map k a -> [(k, trie map k a)])
              -> (([k],a) -> DList ([k],a) -> DList ([k],a))
              -> trie map k a
              -> [([k],a)]
genericToList tolist add = DL.toList . go DL.empty
 where
   go xs tr =
      let (v,m) = tParts tr
          xs'   =
             DL.concat .
             map (\(x,t) -> go (xs `DL.snoc` x) t) .
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
minMaxView _        _       tr_ | null tr_ = (Nothing, tr_)
minMaxView isWanted mapView tr_ = first Just (go tr_)
 where
   go tr =
      let (v,m) = tParts tr
       in if isWanted tr
             then (([], unwrap v), mkTrie altEmpty m)
             else let (k,      tr')  = fromJust (mapView m)
                      (minMax, tr'') = go tr'
                   in ( first (k:) minMax
                      , mkTrie v $ if null tr''
                                      then Map.delete              k m
                                      else Map.adjust (const tr'') k m
                      )

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
findMinMax _        _       tr_ | null tr_ = Nothing
findMinMax isWanted mapView tr_ = Just (go DL.empty tr_)
 where
   go xs tr =
      if isWanted tr
         then (DL.toList xs, unwrap (tVal tr))
         else let (k, tr') = fromJust . mapView . tMap $ tr
               in go (xs `DL.snoc` k) tr'

-- O(m)
deleteMin :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMin = snd . minView

-- O(m)
deleteMax :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMax = snd . maxView

-- O(min(m,s))
split :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
      => [k] -> trie map k a -> (trie map k a, trie map k a)
split xs tr = let (l,_,g) = splitLookup xs tr in (l,g)

-- O(min(m,s))
splitLookup :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
            => [k]
            -> trie map k a
            -> (trie map k a, st a, trie map k a)
splitLookup []     tr = (empty, tVal tr, mkTrie altEmpty (tMap tr))
splitLookup (x:xs) tr =
   let (v,m) = tParts tr
       (ml, subTr, mg) = Map.splitLookup x m
    in case subTr of
            Nothing  -> (mkTrie v ml, altEmpty, mkTrie altEmpty mg)
            Just tr' ->
               let (tl, v', tg) = splitLookup xs tr'
                   ml' = if null tl then ml else Map.insert x tl ml
                   mg' = if null tg then mg else Map.insert x tg mg
                in (mkTrie v ml', v', mkTrie altEmpty mg')

-- O(m)
findPredecessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => [k] -> trie map k a -> Maybe ([k], a)
findPredecessor _   tr | null tr = Nothing
findPredecessor xs_ tr_          = go xs_ tr_
 where
   go [] _ = Nothing

   -- We need to try the trie at x and then the trie at the predecessor of x:
   -- e.g. if looking for "foo", we need to try any 'f' branch to see if it has
   -- "fob" first, before grabbing the next-best option of the maximum of the
   -- 'b' branch, say "bar".
   --
   -- If there's no branch less than 'f' we try the current position as a last
   -- resort.
   go (x:xs) tr =
      let (v,m) = tParts tr
          predecessor = Map.findPredecessor x m
       in fmap (first (x:)) (Map.lookup x m >>= go xs)
          <|>
          case predecessor of
               Nothing         ->
                  if hasValue v
                     then Just ([], unwrap v)
                     else Nothing
               Just (best,btr) -> fmap (first (best:)) (findMax btr)

-- O(m)
findSuccessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
              => [k] -> trie map k a -> Maybe ([k], a)
findSuccessor _   tr | null tr = Nothing
findSuccessor xs_ tr_          = go xs_ tr_
 where
   go [] tr = do (k,t) <- fst . Map.minViewWithKey . tMap $ tr
                 fmap (first (k:)) (findMin t)

   go (x:xs) tr =
      let m = tMap tr
          successor = Map.findSuccessor x m
       in fmap (first (x:)) (Map.lookup x m >>= go xs)
          <|>
          (successor >>= \(best,btr) -> fmap (first (best:)) (findMin btr))

-- * Trie-only operations

-- O(s)
lookupPrefix :: (Alt st a, Boolable (st a), Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
lookupPrefix []     tr = tr
lookupPrefix (x:xs) tr =
   case Map.lookup x (tMap tr) of
        Nothing  -> empty
        Just tr' -> let tr'' = lookupPrefix xs tr'
                     in if null tr''
                           then tr''
                           else mkTrie altEmpty (Map.singleton x tr'')

-- O(s)
addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix []     = id
addPrefix (x:xs) = mkTrie altEmpty . Map.singleton x . addPrefix xs

-- O(s)
deletePrefix :: (Alt st a, Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
deletePrefix []     tr = tr
deletePrefix (x:xs) tr =
   case Map.lookup x (tMap tr) of
        Nothing  -> empty
        Just tr' -> deletePrefix xs tr'

-- O(s)
deleteSuffixes :: (Alt st a, Boolable (st a), Trie trie st map k)
               => [k] -> trie map k a -> trie map k a
deleteSuffixes []     _  = empty
deleteSuffixes (x:xs) tr =
   let (v,m) = tParts tr
    in case Map.lookup x m of
            Nothing  -> tr
            Just tr' -> let tr'' = deleteSuffixes xs tr'
                         in if null tr''
                               then mkTrie v (Map.delete x      m)
                               else mkTrie v (Map.insert x tr'' m)

-- O(m)
splitPrefix :: (Alt st a, Trie trie st map k)
            => trie map k a -> ([k], st a, trie map k a)
splitPrefix = go DL.empty
 where
   go xs tr =
      case Map.singletonView (tMap tr) of
           Just (x,tr') -> go (xs `DL.snoc` x) tr'
           Nothing      -> let (v,m) = tParts tr
                            in (DL.toList xs, v, mkTrie altEmpty m)

-- O(m)
children :: (Boolable (st a), Trie trie st map k)
         => trie map k a -> CMap trie map k a
children tr = let (v,m) = tParts tr
               in if hasValue v
                     then m
                     else case Map.singletonView m of
                               Just (_, tr') -> children tr'
                               Nothing       -> m

-- O(1)
children1 :: (Alt st a, Trie trie st map k)
          => trie map k a -> CMap trie map k a
children1 = tMap

-- * Visualization

-- O(n m)
showTrieWith :: (Show k, Trie trie st map k)
             => (st a -> ShowS) -> trie map k a -> ShowS
showTrieWith = go 0
 where
   go indent f tr =
      let (v,m) = tParts tr
          sv    = f v
          lv    = length (sv [])
       in sv . showChar ' '
        . (foldr (.) id . zipWith (flip ($)) (False : repeat True) $
              map (\(k,t) -> \b -> let sk = shows k
                                       lk = length (sk [])
                                       i  = indent + lv + 1
                                    in (if b
                                           then showChar '\n'
                                              . showString (replicate i ' ')
                                           else id)
                                     . showString "-> "
                                     . sk . showChar ' '
                                     . go (i + lk + 4) f t)
                  (Map.toListKV m))
