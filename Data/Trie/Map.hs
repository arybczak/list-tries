-- File created: 2008-11-11 11:24:30

-- The base implementation of a trie representing a map with list keys,
-- generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

{-# LANGUAGE CPP #-}

module Data.Trie.Map where

import Control.Applicative ((<|>))
import Control.Arrow ((***), first, second)
import Control.Exception (assert)
import Control.Monad (join, liftM2)
import qualified Data.DList as DL
import Data.DList (DList)
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Maybe as Maybe
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident), pfail)
#endif

import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Map (Map, OrdMap)

-- Invariant: any (Tr Nothing _) has a Just descendant.
data TrieMap map k v = Tr !(Maybe v) !(CMap map k v)

type CMap map k v = map k (TrieMap map k v)

-- instances: Eq, Monoid, Foldable, Ord

instance (Map map k, Show k, Show a) => Show (TrieMap map k a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map k, Read k, Read a) => Read (TrieMap map k a) where
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
null :: Map map k => TrieMap map k a -> Bool
null (Tr Nothing m) | Map.null m = True
null _                           = False

-- O(n). The number of elements in the set.
size :: Map map k => TrieMap map k a -> Int
size (Tr v m) = Map.foldValues ((+) . size) (fromEnum . isJust $ v) m

-- O(m).
member :: Map map k => [k] -> TrieMap map k a -> Bool
member k m = isJust (lookup k m)

-- O(m)
lookup :: Map map k => [k] -> TrieMap map k a -> Maybe a
lookup []     (Tr v _) = v
lookup (x:xs) (Tr _ m) = Map.lookup m x >>= lookup xs

-- O(?)
isSubmapOf :: (Map map k, Eq a) => TrieMap map k a -> TrieMap map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- O(?)
isSubmapOfBy :: Map map k
             => (a -> b -> Bool) -> TrieMap map k a -> TrieMap map k b -> Bool
isSubmapOfBy _ (Tr (Just _) _) (Tr Nothing _) = False
isSubmapOfBy f (Tr v1      m1) (Tr v2     m2) =
   fromMaybe True (liftM2 f v1 v2) && Map.isSubmapOfBy (isSubmapOfBy f) m1 m2

-- O(?)
isProperSubmapOf :: (Map map k, Eq a)
                 => TrieMap map k a -> TrieMap map k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- O(?)
isProperSubmapOfBy :: (Map map k) => (a -> b -> Bool)
                                  -> TrieMap map k a
                                  -> TrieMap map k b
                                  -> Bool
isProperSubmapOfBy = go False
 where
   go _      _ (Tr (Just _) _) (Tr Nothing _) = False
   go proper f (Tr v1      m1) (Tr v2     m2) =
      let proper' = or [ proper
                       , isNothing v1 && isJust v2
                       , not (Map.null $ Map.difference m2 m1)
                       ]
       in fromMaybe True (liftM2 f v1 v2) &&
          if Map.null m1
             then proper'
             else Map.isSubmapOfBy (go proper' f) m1 m2

-- * Construction

-- O(1)
empty :: Map map k => TrieMap map k a
empty = Tr Nothing Map.empty

-- O(m)
singleton :: Map map k => [k] -> a -> TrieMap map k a
singleton []     v = Tr (Just v) Map.empty
singleton (x:xs) v = Tr Nothing (Map.singleton x (singleton xs v))

-- O(m)
insert :: Map map k => [k] -> a -> TrieMap map k a -> TrieMap map k a
insert = insertWith const

-- O(m)
insertWith :: Map map k
           => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith f []     v (Tr o m) = Tr (maybe (Just v) (Just . f v) o) m
insertWith f (x:xs) v (Tr o m) = Tr o $
   Map.insertWith (\_ old -> insertWith f xs v old)
                  m x (singleton xs v)

-- O(m)
insertWithKey :: Map map k => ([k] -> a -> a -> a)
                           -> [k] -> a
                           -> TrieMap map k a
                           -> TrieMap map k a
insertWithKey = go DL.empty
 where
   go k f []     v (Tr o m) =
      Tr (maybe (Just v) (Just . f (DL.toList k) v) o) m

   go k f (x:xs) v (Tr o m) = Tr o $
      Map.insertWith (\_ old -> go (k `DL.snoc` x) f xs v old)
                     m x (singleton xs v)

-- O(m)
delete :: Map map k => [k] -> TrieMap map k a -> TrieMap map k a
delete = alter (const Nothing)

-- O(m)
adjust :: Map map k => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust f []     (Tr v m) = Tr (fmap f v) m
adjust f (x:xs) (Tr v m) = Tr v $ Map.adjust (adjust f xs) m x

-- O(m)
update :: Map map k
       => (a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
update f k t = snd (updateLookup f k t)

-- O(m)
updateLookup :: Map map k => (a -> Maybe a)
                          -> [k]
                          -> TrieMap map k a
                          -> (Maybe a, TrieMap map k a)
updateLookup f [] (Tr v m) =
   let v' = v >>= f
    in (v' <|> v, Tr v' m)

updateLookup f (x:xs) orig@(Tr v m) =
   let old = Map.lookup m x
    in case old of
            Nothing -> (Nothing, orig)
            Just tr ->
               let (ret, upd) = updateLookup f xs tr
                in ( ret
                   , Tr v $ if null upd
                               then Map.delete             m x
                               else Map.adjust (const upd) m x
                   )


alter :: Map map k
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter f []     (Tr v m) = Tr (f v) m
alter f (x:xs) (Tr v m) = Tr v $
   Map.update (\old -> let new = alter f xs old
                        in if null new then Nothing else Just new)
              m x

-- * Combination

-- O(n1+n2)
union :: Map map k => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union = unionWith const

-- O(n1+n2)
unionWith :: Map map k => (a -> a -> a)
                       -> TrieMap map k a
                       -> TrieMap map k a
                       -> TrieMap map k a
unionWith f (Tr v1 m1) (Tr v2 m2) =
   Tr (unionMaybes f v1 v2)
      (Map.unionWith (unionWith f) m1 m2)

-- O(n1+n2)
unionWithKey :: Map map k => ([k] -> a -> a -> a)
                          -> TrieMap map k a
                          -> TrieMap map k a
                          -> TrieMap map k a
unionWithKey = go DL.empty
 where
   go k f (Tr v1 m1) (Tr v2 m2) =
      Tr (unionMaybes (f $ DL.toList k) v1 v2)
         (Map.unionWithKey (\x -> go (k `DL.snoc` x) f)
                           m1 m2)

unionMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybes f ma mb = maybe mb (\a -> maybe ma (Just . f a) mb) ma

unions :: Map map k => [TrieMap map k a] -> TrieMap map k a
unions = unionsWith const

unionsWith :: Map map k
           => (a -> a -> a) -> [TrieMap map k a] ->  TrieMap map k a
unionsWith f = foldl' (unionWith f) empty

-- O(n1+n2)
difference :: Map map k
           => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
difference = differenceWith (\_ _ -> Nothing)

-- O(n1+n2)
differenceWith :: Map map k => (a -> b -> Maybe a)
                            -> TrieMap map k a
                            -> TrieMap map k b
                            -> TrieMap map k a
differenceWith f (Tr v1 m1) (Tr v2 m2) =
   Tr (differenceMaybes f v1 v2) (Map.differenceWith (g f) m1 m2)
 where
   g f' t1 t2 = let t' = differenceWith f' t1 t2
                 in if null t' then Nothing else Just t'

-- O(n1+n2)
differenceWithKey :: Map map k => ([k] -> a -> b -> Maybe a)
                               -> TrieMap map k a
                               -> TrieMap map k b
                               -> TrieMap map k a
differenceWithKey = go DL.empty
 where
   go k f (Tr v1 m1) (Tr v2 m2) =
      Tr (differenceMaybes (f $ DL.toList k) v1 v2)
         (Map.differenceWithKey (g k f) m1 m2)

   g k f x t1 t2 = let t' = go (k `DL.snoc` x) f t1 t2
                    in if null t' then Nothing else Just t'

differenceMaybes :: (a -> b -> Maybe a) -> Maybe a -> Maybe b -> Maybe a
differenceMaybes f ma mb = ma >>= \a -> maybe ma (f a) mb

-- O(n1+n2)
intersection :: Map map k
             => TrieMap map k a -> TrieMap map k b -> TrieMap map k a
intersection = intersectionWith const

-- O(n1+n2)
intersectionWith :: Map map k => (a -> b -> c)
                              -> TrieMap map k a
                              -> TrieMap map k b
                              -> TrieMap map k c
intersectionWith f (Tr v1 m1) (Tr v2 m2) =
   tr (intersectionMaybes f v1 v2)
      (Map.intersectionWith (intersectionWith f) m1 m2)
 where
   tr b m = case Map.singletonView m of
                 Just (_, child@(Tr _ subM)) | null child -> Tr b subM
                 _                                        -> Tr b m

-- O(n1+n2)
intersectionWithKey :: Map map k => ([k] -> a -> b -> c)
                                 -> TrieMap map k a
                                 -> TrieMap map k b
                                 -> TrieMap map k c
intersectionWithKey = go DL.empty
 where
   go k f (Tr v1 m1) (Tr v2 m2) =
      tr (intersectionMaybes (f $ DL.toList k) v1 v2)
         (Map.intersectionWithKey (\x -> go (k `DL.snoc` x) f) m1 m2)

   tr b m = case Map.singletonView m of
                 Just (_, child@(Tr _ subM)) | null child -> Tr b subM
                 _                                        -> Tr b m

intersectionMaybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
intersectionMaybes = liftM2

-- * Filtering

-- O(n)
filter :: Map map k => (a -> Bool) -> TrieMap map k a -> TrieMap map k a
filter p = filterWithKey (const p)

-- O(n)
filterWithKey :: Map map k
              => ([k] -> a -> Bool) -> TrieMap map k a -> TrieMap map k a
filterWithKey p = fromList . Prelude.filter (uncurry p) . toList

-- O(n)
partition :: Map map k => (a -> Bool)
                       -> TrieMap map k a
                       -> (TrieMap map k a, TrieMap map k a)
partition p = partitionWithKey (const p)

-- O(n)
partitionWithKey :: Map map k => ([k] -> a -> Bool)
                              -> TrieMap map k a
                              -> (TrieMap map k a, TrieMap map k a)
partitionWithKey p = join (***) fromList . List.partition (uncurry p) . toList

split :: OrdMap map k
      => [k] -> TrieMap map k a -> (TrieMap map k a, TrieMap map k a)
split xs tr = let (l,_,g) = splitLookup xs tr in (l,g)

splitLookup :: OrdMap map k => [k]
                            -> TrieMap map k a
                            -> (TrieMap map k a, Maybe a, TrieMap map k a)
splitLookup []     (Tr v m) = (empty, v, Tr Nothing m)
splitLookup (x:xs) (Tr v m) =
   let (ml, tr, mg) = Map.splitLookup m x
    in case tr of
            Nothing  -> (Tr v ml, Nothing, Tr Nothing mg)
            Just tr' ->
               let (tl, v', tg) = splitLookup xs tr'
                   ml' = if null tl then ml else Map.insert ml x tl
                   mg' = if null tg then mg else Map.insert mg x tg
                in (Tr v ml', v', Tr Nothing mg')

-- O(n)
mapMaybe :: Map map k
         => (a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybe = mapMaybeWithKey . const

-- O(n)
mapMaybeWithKey :: Map map k
                => ([k] -> a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybeWithKey f =
   fromList . Maybe.mapMaybe (\(k,v) -> fmap ((,) k) (f k v)) . toList

-- O(n)
mapEither :: Map map k => (a -> Either b c)
                       -> TrieMap map k a
                       -> (TrieMap map k b, TrieMap map k c)
mapEither = mapEitherWithKey . const

-- O(n)
mapEitherWithKey :: Map map k => ([k] -> a -> Either b c)
                              -> TrieMap map k a
                              -> (TrieMap map k b, TrieMap map k c)
mapEitherWithKey f =
   (fromList *** fromList) . partitionEithers .
   Prelude.map (\(k,v) -> either (Left . (,) k) (Right . (,) k) (f k v)) .
   toList

-- * Mapping

-- O(n)
map :: (Map map k) => (a -> b) -> TrieMap map k a -> TrieMap map k b
map f (Tr v m) = Tr (fmap f v) (Map.map (map f) m)

-- O(n)
mapWithKey :: (Map map k)
           => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey = go DL.empty
 where
   go k f (Tr v m) = Tr (fmap (f $ DL.toList k) v)
                        (Map.mapWithKey (\x -> go (k `DL.snoc` x) f) m)

-- O(n)
mapAccum :: (Map map k) => (acc -> a -> (acc, b))
                        -> acc
                        -> TrieMap map k a
                        -> (acc, TrieMap map k b)
mapAccum = genericMapAccum Map.mapAccum

-- O(n)
mapAccumWithKey :: (Map map k) => (acc -> [k] -> a -> (acc, b))
                               -> acc
                               -> TrieMap map k a
                               -> (acc, TrieMap map k b)
mapAccumWithKey = genericMapAccumWithKey Map.mapAccumWithKey

-- O(n)
mapAccumAsc :: (OrdMap map k) => (acc -> a -> (acc, b))
                              -> acc
                              -> TrieMap map k a
                              -> (acc, TrieMap map k b)
mapAccumAsc = genericMapAccum Map.mapAccumAsc

-- O(n)
mapAccumAscWithKey :: (OrdMap map k) => (acc -> [k] -> a -> (acc, b))
                                     -> acc
                                     -> TrieMap map k a
                                     -> (acc, TrieMap map k b)
mapAccumAscWithKey = genericMapAccumWithKey Map.mapAccumAscWithKey

-- O(n)
mapAccumDesc :: (OrdMap map k) => (acc -> a -> (acc, b))
                               -> acc
                               -> TrieMap map k a
                               -> (acc, TrieMap map k b)
mapAccumDesc = genericMapAccum Map.mapAccumDesc

-- O(n)
mapAccumDescWithKey :: (OrdMap map k) => (acc -> [k] -> a -> (acc, b))
                                      -> acc
                                      -> TrieMap map k a
                                      -> (acc, TrieMap map k b)
mapAccumDescWithKey = genericMapAccumWithKey Map.mapAccumDescWithKey

genericMapAccum :: (Map map k)
                => (  (acc -> TrieMap map k a -> (acc, TrieMap map k b))
                   -> acc
                   -> CMap map k a
                   -> (acc, CMap map k b)
                   )
                -> (acc -> a -> (acc, b))
                -> acc
                -> TrieMap map k a
                -> (acc, TrieMap map k b)
genericMapAccum subMapAccum f acc (Tr mv m) =
   let (acc', v') =
          case mv of
               Nothing -> (acc, Nothing)
               Just v  -> second Just (f acc v)
    in second (Tr v') $ subMapAccum (genericMapAccum subMapAccum f) acc' m

genericMapAccumWithKey :: (Map map k)
                       => (  (  acc
                             -> k
                             -> TrieMap map k a
                             -> (acc, TrieMap map k b)
                             )
                          -> acc
                          -> CMap map k a
                          -> (acc, CMap map k b)
                          )
                       -> (acc -> [k] -> a -> (acc, b))
                       -> acc
                       -> TrieMap map k a
                       -> (acc, TrieMap map k b)
genericMapAccumWithKey = go DL.empty
 where
   go k subMapAccum f acc (Tr mv m) =
      let (acc', v') =
             case mv of
                  Nothing -> (acc, Nothing)
                  Just v  -> second Just (f acc (DL.toList k) v)
       in second (Tr v') $
             subMapAccum (\a x -> go (k `DL.snoc` x) subMapAccum f a) acc' m

-- O(n)
mapKeys :: (Map map k1, Map map k2)
        => ([k1] -> [k2]) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys = mapKeysWith const

-- O(n)
mapKeysWith :: (Map map k1, Map map k2) => (a -> a -> a)
                                        -> ([k1] -> [k2])
                                        -> TrieMap map k1 a
                                        -> TrieMap map k2 a
mapKeysWith j f = fromListWith j . Prelude.map (first f) . toList

-- O(n)
-- TODO: needs a name!
mapKeys' :: (Map map k1, Map map k2)
         => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys' f (Tr v m) =
   Tr v $
      Map.fromListWith union .
         Prelude.map (f *** mapKeys' f) .
      Map.toList $ m

-- O(n)
-- TODO: needs a name!
mapKeys'With :: (Map map k1, Map map k2)
             => (a -> a -> a)
             -> (k1 -> k2)
             -> TrieMap map k1 a
             -> TrieMap map k2 a
mapKeys'With j f (Tr v m) =
   Tr v $
      Map.fromListWith (unionWith j) .
         Prelude.map (f *** mapKeys' f) .
      Map.toList $ m

-- * Folding

-- O(n)
fold :: Map map k => (a -> b -> b) -> b -> TrieMap map k a -> b
fold f = foldWithKey (const f)

-- O(n)
foldWithKey :: Map map k => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldWithKey f x = Prelude.foldr (uncurry f) x . toList

-- O(n)
foldAsc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldAsc f = foldAscWithKey (const f)

-- O(n)
foldAscWithKey :: OrdMap map k
               => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldAscWithKey f x = Prelude.foldr (uncurry f) x . toAscList

-- O(n)
foldDesc :: OrdMap map k => (a -> b -> b) -> b -> TrieMap map k a -> b
foldDesc f = foldDescWithKey (const f)

-- O(n)
foldDescWithKey :: OrdMap map k
                => ([k] -> a -> b -> b) -> b -> TrieMap map k a -> b
foldDescWithKey f x = Prelude.foldr (uncurry f) x . toDescList

-- * Conversion between lists

-- O(n)
toList :: Map map k => TrieMap map k a -> [([k],a)]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: OrdMap map k => TrieMap map k a -> [([k],a)]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Map map k => (CMap map k a -> [(k, TrieMap map k a)])
                           -> (([k],a) -> DList ([k],a) -> DList ([k],a))
                           -> TrieMap map k a
                           -> [([k],a)]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go l f g (Tr mv m) =
      let
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l `DL.snoc` x) f g t) .
            f $ m
       in case mv of
               Just v  -> g (DL.toList l, v) xs
               Nothing ->                    xs
-- O(n*m)
fromList :: Map map k => [([k],a)] -> TrieMap map k a
fromList = fromListWith const

-- O(n*m)
fromListWith :: Map map k => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

-- O(n*m)
fromListWithKey :: Map map k
                => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey f = foldl' (flip . uncurry $ insertWithKey f) empty

-- * Min/max

-- O(m log b)
findMin :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMin = findMinMax (\(Tr v _) -> isJust v)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
findMax :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMax = findMinMax (\(Tr _ m) -> Map.null m)
                     (\(Tr v _) -> assert (isJust v))
                     (fst . Map.maxViewWithKey)

findMinMax :: OrdMap map k => (TrieMap map k a -> Bool)
                           -> (TrieMap map k a -> ([k],a) -> ([k],a))
                           -> (CMap map k a -> Maybe (k, TrieMap map k a))
                           -> TrieMap map k a
                           -> Maybe ([k], a)
findMinMax _ _ _ tr_ | null tr_ = Nothing
findMinMax f g h tr_ = Just (go DL.empty f g h tr_)
 where
   go k cond base mapView tr@(Tr v m) =
      if cond tr
         then base tr (DL.toList k, fromJust v)
         else let (x,t) = fromJust (mapView m)
               in go (k `DL.snoc` x) cond base mapView t

-- O(m log b)
deleteMin :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMin = maybe empty snd . minView

-- O(m log b)
deleteMax :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMax = maybe empty snd . maxView

-- O(m log b)
minView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
minView = minMaxView (\(Tr v _) -> isJust v)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
maxView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
maxView = minMaxView (\(Tr _ m) -> Map.null m)
                     (\(Tr v _) -> assert (isJust v))
                     (fst . Map.maxViewWithKey)

minMaxView :: OrdMap map k
           => (TrieMap map k a -> Bool)
           -> (  TrieMap map k a
              -> (([k], a), TrieMap map k a)
              -> (([k], a), TrieMap map k a)
              )
           -> (CMap map k a -> Maybe (k, TrieMap map k a))
           -> TrieMap map k a
           -> Maybe (([k], a), TrieMap map k a)
minMaxView _ _ _ tr_ | null tr_ = Nothing
minMaxView f g h tr_ = Just (go f g h DL.empty tr_)
 where
   go cond base mapView xs tr@(Tr v m) =
      if cond tr
         then base tr ((DL.toList xs, fromJust v), Tr v m)
         else let (k,      t)  = fromJust (mapView m)
                  (minMax, t') = go cond base mapView (xs `DL.snoc` k) t
               in ( minMax
                  , Tr v $ if null t'
                              then Map.delete            m k
                              else Map.adjust (const t') m k
                  )

-- O(m b)
findPredecessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go _        []     = Nothing
   go (Tr v m) (x:xs) =
      let candidates = Map.toDescList . fst . Map.split m $ x
          (best,btr) = head candidates

       in fmap (first (x:)) (Map.lookup m x >>= flip go xs)
          <|>
          if Prelude.null candidates
             then fmap ((,) []) v
             else fmap (first (best:)) (findMax btr)

-- O(m b)
findSuccessor :: OrdMap map k => TrieMap map k a -> [k] -> Maybe ([k], a)
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go (Tr _ m) [] = do (k,t) <- fst $ Map.minViewWithKey m
                       fmap (first (k:)) (findMin t)

   go (Tr _ m) (x:xs) =
      let candidates = Map.toAscList . snd . Map.split m $ x
          (best,btr) = head candidates

       in fmap (first (x:)) (Map.lookup m x >>= flip go xs)
          <|>
          if Prelude.null candidates
             then Nothing
             else fmap (first (best:)) (findMin btr)
