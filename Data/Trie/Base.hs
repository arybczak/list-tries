-- File created: 2008-11-13 21:13:55

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.Trie.Base where

import qualified Control.Applicative as A
import Control.Applicative (Applicative(..), Alternative((<|>)), (<$>))
import Control.Arrow       ((***), first)
import Control.Monad       (liftM2, join)
import qualified Data.DList as DL
import Data.DList          (DList)
import Data.List           (foldl', partition)
import Data.Maybe          (fromJust, isJust)
import Prelude hiding      (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Map (Map, OrdMap)

class Unwrappable  w where unwrap  :: w a -> a
class Boolable     b where toBool  :: b a -> Bool

hasValue, noValue :: Boolable b => b a -> Bool
hasValue = toBool
noValue  = not . hasValue

class SetValue v where
   unionVals        :: (a -> a -> a)       -> v a -> v a -> v a
   differenceVals   :: (a -> b -> Maybe a) -> v a -> v b -> v a
   intersectionVals :: (a -> b -> c)       -> v a -> v b -> v c

instance Unwrappable  Maybe where unwrap = fromJust
instance Boolable     Maybe where toBool =   isJust
instance SetValue     Maybe where
   unionVals f (Just a) (Just b) = Just (f a b)
   unionVals _ Nothing  mb       = mb
   unionVals _ ma       _        = ma

   differenceVals f (Just a) (Just b) = f a b
   differenceVals _ ma       _        = ma

   intersectionVals = liftM2

class (Map map k, Alternative st, Boolable st, SetValue st, Unwrappable st)
   => Trie trie st map k | trie -> st where

   mkTrie :: st a -> CMap trie map k a -> trie map k a
   tParts :: trie map k a -> (st a, CMap trie map k a)

type CMap trie map k v = map k (trie map k v)

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

mapt :: Trie trie st map k => (st a -> st b)
                           -> (CMap trie map k a -> CMap trie map k b)
                           -> trie map k a
                           -> trie map k b
mapt f g tr = mkTrie (f . tVal $ tr) (g . tMap $ tr)

-----------------------

null :: Trie trie st map k => trie map k a -> Bool
null tr = (noValue.tVal $ tr) && Map.null (tMap tr)

size :: Trie trie st map k => trie map k a -> Int
size tr = Map.foldValues ((+) . size) (fromEnum.hasValue.tVal $ tr) (tMap tr)

lookup :: Trie trie st map k => [k] -> trie map k a -> st a
lookup []     tr = tVal tr
lookup (x:xs) tr = maybe A.empty (lookup xs) (Map.lookup (tMap tr) x)

isSubmapOfBy :: Trie trie st map k => (st a -> st b -> st Bool)
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

isProperSubmapOfBy :: Trie trie st map k => (st a -> st b -> st Bool)
                                         -> trie map k a
                                         -> trie map k b
                                         -> Bool
isProperSubmapOfBy = go False
 where
   go proper f tr1 tr2 =
      let (v1,m1) = tParts tr1
          (v2,m2) = tParts tr2
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

empty :: Trie trie st map k => trie map k a
empty = mkTrie A.empty Map.empty

singleton :: Trie trie st map k => [k] -> a -> trie map k a
singleton []     v = mkTrie (pure v) Map.empty
singleton (x:xs) v = mkTrie A.empty (Map.singleton x (singleton xs v))

insertWith :: Trie trie st map k
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith f []     new tr = mapVal tr $ \old -> (f new <$> old) <|> pure new
insertWith f (x:xs) val tr = mapMap tr $ \m ->
   Map.insertWith (\_ old -> insertWith f xs val old)
                  m x (singleton xs val)

insertWithKey :: Trie trie st map k => ([k] -> a -> a -> a)
                                    -> [k] -> a
                                    -> trie map k a
                                    -> trie map k a
insertWithKey f k = insertWith (f k) k

adjust :: Trie trie st map k
       => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust f []     tr = mapVal tr (fmap f)
adjust f (x:xs) tr = mapMap tr $ \m -> Map.adjust (adjust f xs) m x

-- The given function returns the looked-up value and the updated value in a
-- pair.
--
-- TODO: since A.empty exists I think this can be done
updateLookup :: Trie trie st map k => (st a -> (st a, st a))
                                   -> [k]
                                   -> trie map k a
                                   -> (st a, trie map k a)
updateLookup f [] tr =
   let (v,m)    = tParts tr
       (va, vb) = f v
    in (va, mkTrie vb m)

updateLookup f (x:xs) orig =
   let m   = tMap orig
       old = Map.lookup m x
    in case old of
            Nothing -> (A.empty, orig)
            Just tr ->
               let (ret, upd) = updateLookup f xs tr
                in ( ret
                   , mkTrie (tVal orig) $ if null upd
                                             then Map.delete             m x
                                             else Map.adjust (const upd) m x
                   )

alter :: Trie trie st map k
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter f []     tr = mapVal tr f
alter f (x:xs) tr = mapMap tr $ \m ->
   Map.update (\old -> let new = alter f xs old
                        in if null new then Nothing else Just new)
              m x

-- * Combination

unionWith :: Trie trie st map k
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f tr1 tr2 =
   mkTrie (onVals (unionVals f) tr1 tr2)
          (onMaps (Map.unionWith (unionWith f)) tr1 tr2)

unionWithKey :: Trie trie st map k => ([k] -> a -> a -> a)
                                   -> trie map k a
                                   -> trie map k a
                                   -> trie map k a
unionWithKey = go DL.empty
 where
   go k f tr1 tr2 =
      mkTrie (onVals (unionVals (f $ DL.toList k)) tr1 tr2)
             (onMaps (Map.unionWithKey $ \x -> go (k `DL.snoc` x) f) tr1 tr2)

differenceWith :: Trie trie st map k => (a -> b -> Maybe a)
                                     -> trie map k a
                                     -> trie map k b
                                     -> trie map k a
differenceWith f tr1 tr2 =
   mkTrie (onVals (differenceVals f) tr1 tr2)
          (onMaps (Map.differenceWith (g f)) tr1 tr2)
 where
   g f' t1 t2 = let t' = differenceWith f' t1 t2
                 in if null t' then Nothing else Just t'

differenceWithKey :: Trie trie st map k => ([k] -> a -> b -> Maybe a)
                                        -> trie map k a
                                        -> trie map k b
                                        -> trie map k a
differenceWithKey = go DL.empty
 where
   go k f tr1 tr2 =
      mkTrie (onVals (differenceVals (f $ DL.toList k)) tr1 tr2)
             (onMaps (Map.differenceWithKey (g k f)) tr1 tr2)

   g k f x t1 t2 = let t' = go (k `DL.snoc` x) f t1 t2
                    in if null t' then Nothing else Just t'

intersectionWith :: Trie trie st map k => (a -> b -> c)
                                       -> trie map k a
                                       -> trie map k b
                                       -> trie map k c
intersectionWith f tr1 tr2 =
   tr (onVals (intersectionVals f) tr1 tr2)
      (onMaps (Map.intersectionWith (intersectionWith f)) tr1 tr2)
 where
   tr v m = mkTrie v $
               case Map.singletonView m of
                    Just (_, child) | null child -> tMap child
                    _                            -> m

intersectionWithKey :: Trie trie st map k => ([k] -> a -> b -> c)
                                          -> trie map k a
                                          -> trie map k b
                                          -> trie map k c
intersectionWithKey = go DL.empty
 where
   go k f tr1 tr2 =
      tr (onVals (intersectionVals (f $ DL.toList k)) tr1 tr2)
         (onMaps (Map.intersectionWithKey$ \x -> go (k `DL.snoc` x) f) tr1 tr2)

   tr v m = mkTrie v $
               case Map.singletonView m of
                    Just (_, child) | null child -> tMap child
                    _                            -> m

filterWithKey :: Trie trie st map k
              => ([k] -> a -> Bool) -> trie map k a -> trie map k a
filterWithKey p = fromList . Prelude.filter (uncurry p) . toList

partitionWithKey :: Trie trie st map k => ([k] -> a -> Bool)
                                       -> trie map k a
                                       -> (trie map k a, trie map k a)
partitionWithKey p = join (***) fromList . partition (uncurry p) . toList

splitLookup :: (Trie trie st map k, OrdMap map k)
            => [k]
            -> trie map k a
            -> (trie map k a, st a, trie map k a)
splitLookup []     tr = (empty, tVal tr, mkTrie A.empty (tMap tr))
splitLookup (x:xs) tr =
   let (v,m) = tParts tr
       (ml, subTr, mg) = Map.splitLookup m x
    in case subTr of
            Nothing  -> (mkTrie v ml, A.empty, mkTrie A.empty mg)
            Just tr' ->
               let (tl, v', tg) = splitLookup xs tr'
                   ml' = if null tl then ml else Map.insert ml x tl
                   mg' = if null tg then mg else Map.insert mg x tg
                in (mkTrie v ml', v', mkTrie A.empty mg')

-- * Mapping

mapKeysWith :: (Trie trie st map k1, Trie trie st map k2)
            => ([([k2],a)] -> trie map k2 a)
            -> ([k1] -> [k2])
            -> trie map k1 a
            -> trie map k2 a
mapKeysWith fromlist f = fromlist . Prelude.map (first f) . toList

-- TODO: needs a name!
mapKeys'With :: (Trie trie st map k1, Trie trie st map k2)
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

foldWithKey :: Trie trie st map k
            => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldWithKey f x = Prelude.foldr (uncurry f) x . toList

foldAscWithKey :: (Trie trie st map k, OrdMap map k)
               => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldAscWithKey f x = Prelude.foldr (uncurry f) x . toAscList

foldDescWithKey :: (Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldDescWithKey f x = Prelude.foldr (uncurry f) x . toDescList

-- * Conversion between lists

toList :: Trie trie st map k => trie map k a -> [([k],a)]
toList = genericToList Map.toList DL.cons

toAscList :: (Trie trie st map k, OrdMap map k) => trie map k a -> [([k],a)]
toAscList = genericToList Map.toAscList DL.cons

toDescList :: (Trie trie st map k, OrdMap map k) => trie map k a -> [([k],a)]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Trie trie st map k
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

fromList :: Trie trie st map k => [([k],a)] -> trie map k a
fromList = fromListWith const

fromListWith :: Trie trie st map k
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

fromListWithKey :: Trie trie st map k
                => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey f = foldl' (flip . uncurry $ insertWithKey f) empty

-- * Min/max

findMin :: (Trie trie st map k, OrdMap map k) => trie map k a -> Maybe ([k], a)
findMin = findMinMax (hasValue.tVal) (fst . Map.minViewWithKey)

findMax :: (Trie trie st map k, OrdMap map k) => trie map k a -> Maybe ([k], a)
findMax = findMinMax (Map.null.tMap) (fst . Map.maxViewWithKey)

findMinMax :: Trie trie st map k
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

deleteMin :: (Trie trie st map k, OrdMap map k) => trie map k a -> trie map k a
deleteMin = maybe empty snd . minView

deleteMax :: (Trie trie st map k, OrdMap map k) => trie map k a -> trie map k a
deleteMax = maybe empty snd . maxView

minView :: (Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe (([k], a), trie map k a)
minView = minMaxView (hasValue.tVal) (fst . Map.minViewWithKey)

maxView :: (Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe (([k], a), trie map k a)
maxView = minMaxView (Map.null.tMap) (fst . Map.maxViewWithKey)

minMaxView :: Trie trie st map k
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
             then (([], unwrap v), mkTrie A.empty m)
             else let (k,      tr')  = fromJust (mapView m)
                      (minMax, tr'') = go isWanted mapView tr'
                   in ( first (k:) minMax
                      , mkTrie v $ if null tr''
                                      then Map.delete              m k
                                      else Map.adjust (const tr'') m k
                      )

findPredecessor :: (Trie trie st map k, OrdMap map k)
                => trie map k a -> [k] -> Maybe ([k], a)
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go _  [] = Nothing
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

findSuccessor :: (Trie trie st map k, OrdMap map k)
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
