-- File created: 2008-11-07 17:30:16

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances #-}

module Data.ListTrie.Base.Map
   ( Map(..), OrdMap(..)
   , AList, WrappedIntMap
   ) where

import Control.Applicative (pure, (<*>))
import Control.Arrow       ((***), first, second)
import Control.Monad       (liftM, liftM2)
import Data.Foldable       (Foldable(..))
import Data.Function       (on)
import Data.List           ( foldl1'
                           , mapAccumL, nubBy, partition
                           , sort, sortBy
                           )
import Data.Ord            (comparing)
import Data.Traversable    (Traversable(..), mapAccumR)
import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Prelude hiding ( foldl,foldl1,foldr,foldr1
                      , mapM,sequence
                      , null,lookup,filter -- for Haddock
                      )
import qualified Prelude

import Data.ListTrie.Util (both, (.:))

#ifdef MIN_VERSION_containers -- from Cabal
# if !(MIN_VERSION_containers(0,3,0))
# define TOO_OLD_CONTAINERS
# endif
#else
#define TOO_OLD_CONTAINERS
#endif

-- | Minimal complete implementation:
--
-- * 'eqCmp'
--
-- * 'null'
--
-- * 'lookup'
--
-- * 'alter'
--
-- * 'unionWithKey', 'differenceWithKey', 'intersectionWithKey'
--
-- * 'toList'
--
-- * 'empty' or 'fromList' or 'fromListWith'
--
-- * 'isSubmapOfBy'
--
-- For decent performance, supplying at least 'mapAccumWithKey' and 'filter' as
-- well is probably a good idea.
class Foldable (m k) => Map m k where
   -- | Like an 'Eq' instance over k, but should compare on the same type as
   -- @m@ does. In most cases this can be defined just as @const (==)@.
   eqCmp :: m k a -> k -> k -> Bool

   empty     ::                     m k a
   singleton ::           k -> a -> m k a
   -- | Precondition: the two keys differ
   doubleton :: k -> a -> k -> a -> m k a

   null   ::      m k a -> Bool
   lookup :: k -> m k a -> Maybe a

   -- | Strictness can be whatever is more optimal for the map type, shouldn't
   -- matter
   insertWith :: (a -> a -> a) -> k -> a -> m k a -> m k a
   insert     ::                  k -> a -> m k a -> m k a

   update :: (a -> Maybe a) -> k -> m k a -> m k a
   adjust :: (a -> a)       -> k -> m k a -> m k a
   delete ::                   k -> m k a -> m k a

   alter :: (Maybe a -> Maybe a) -> k -> m k a -> m k a

   unionWith           ::      (a -> a -> a)       -> m k a -> m k a -> m k a
   differenceWith      ::      (a -> b -> Maybe a) -> m k a -> m k b -> m k a
   intersectionWith    ::      (a -> b -> c)       -> m k a -> m k b -> m k c
   unionWithKey        :: (k -> a -> a -> a)       -> m k a -> m k a -> m k a
   differenceWithKey   :: (k -> a -> b -> Maybe a) -> m k a -> m k b -> m k a
   intersectionWithKey :: (k -> a -> b -> c)       -> m k a -> m k b -> m k c

   map             ::      (a -> b) -> m k a -> m k b
   mapWithKey      :: (k -> a -> b) -> m k a -> m k b
   mapAccum        :: (a ->      b -> (a,c)) -> a -> m k b -> (a, m k c)
   mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> m k b -> (a, m k c)

   filter :: (a -> Bool) -> m k a -> m k a

   toList       :: m k a -> [(k,a)]
   fromList     ::                  [(k,a)] -> m k a
   fromListWith :: (a -> a -> a) -> [(k,a)] -> m k a

   serializeToList     :: m k a -> [(k,a)]
   deserializeFromList :: [(k,a)] -> m k a

   isSubmapOfBy :: (a -> b -> Bool) -> m k a -> m k b -> Bool

   singletonView :: m k a -> Maybe (k,a)

   empty         = fromList []
   singleton k v = insert k v empty
   doubleton k v = insert k v .: singleton

   insert           = insertWith const
   insertWith f k v = alter (\mold -> Just $ case mold of
                                                    Nothing  -> v
                                                    Just old -> f v old)
                            k

   adjust f = update (Just . f)
   delete   = update (const Nothing)
   update f = alter  (f =<<)

   unionWith        = unionWithKey        . const
   differenceWith   = differenceWithKey   . const
   intersectionWith = intersectionWithKey . const

   map                 = mapWithKey . const
   mapWithKey      f   = snd . mapAccumWithKey (\_ k v -> ((), f k v)) ()
   mapAccum        f   = mapAccumWithKey (const . f)
   mapAccumWithKey f z =
      second fromList .
         mapAccumL (\a (k,v) -> fmap ((,) k) (f a k v)) z .
      toList

   filter p = fromList . Prelude.filter (p . snd) . toList

   -- | Should be strict in the keys
   fromList       = fromListWith const
   fromListWith f = foldr (uncurry $ insertWith f) empty

   serializeToList     = toList
   deserializeFromList = fromList

   singletonView m =
      case toList m of
           [x] -> Just x
           _   -> Nothing

-- |  Minimal complete definition:
--
-- * 'ordCmp'
--
-- * 'toAscList' or 'toDescList'
--
-- * 'splitLookup'
--
-- For decent performance, supplying at least the following is probably a good
-- idea:
--
-- * 'minViewWithKey', 'maxViewWithKey'
--
-- * 'mapAccumAscWithKey', 'mapAccumDescWithKey'
class Map m k => OrdMap m k where
   -- | Like an Ord instance over k, but should compare on the same type as @m@
   -- does. In most cases this can be defined just as @const compare@.
   ordCmp :: m k a -> k -> k -> Ordering

   toAscList            :: m k a -> [(k,a)]
   toDescList           :: m k a -> [(k,a)]

   splitLookup :: k -> m k a -> (m k a, Maybe a, m k a)
   split       :: k -> m k a -> (m k a,          m k a)

   minViewWithKey :: m k a -> (Maybe (k,a), m k a)
   maxViewWithKey :: m k a -> (Maybe (k,a), m k a)

   findPredecessor :: k -> m k a -> Maybe (k,a)
   findSuccessor   :: k -> m k a -> Maybe (k,a)

   mapAccumAsc         :: (a ->      b -> (a,c)) -> a -> m k b -> (a, m k c)
   mapAccumAscWithKey  :: (a -> k -> b -> (a,c)) -> a -> m k b -> (a, m k c)
   mapAccumDesc        :: (a ->      b -> (a,c)) -> a -> m k b -> (a, m k c)
   mapAccumDescWithKey :: (a -> k -> b -> (a,c)) -> a -> m k b -> (a, m k c)

   toAscList  = reverse . toDescList
   toDescList = reverse . toAscList

   split m k = let (a,_,b) = splitLookup m k in (a,b)

   minViewWithKey m =
      case toAscList m of
           []     -> (Nothing, m)
           (x:xs) -> (Just x, fromList xs)

   maxViewWithKey m =
      case toDescList m of
           []     -> (Nothing, m)
           (x:xs) -> (Just x, fromList xs)

   findPredecessor m = fst . maxViewWithKey . fst . split m
   findSuccessor   m = fst . minViewWithKey . snd . split m

   mapAccumAsc  f = mapAccumAscWithKey  (const . f)
   mapAccumDesc f = mapAccumDescWithKey (const . f)
   mapAccumAscWithKey f z =
      second fromList .
         mapAccumL (\a (k,v) -> fmap ((,) k) (f a k v)) z .
      toAscList
   mapAccumDescWithKey f z =
      second fromList .
         mapAccumL (\a (k,v) -> fmap ((,) k) (f a k v)) z .
      toDescList

------------- Instances

newtype AList k v = AL [(k,v)]

-- AList has to be ordering-ignorant
instance (Eq k, Eq v) => Eq (AList k v) where
   AL []     == AL ys = Prelude.null ys
   AL (x:xs) == AL ys =
      let (my,ys') = deleteAndGetBy (==x) ys
       in case my of
               Nothing -> False
               Just _  -> AL xs == AL ys'

instance (Ord k, Ord v) => Ord (AList k v) where
   compare (AL xs) (AL ys) = compare (sort xs) (sort ys)

instance Functor (AList k)  where fmap f (AL xs) = AL (fmap (second f) xs)
instance Foldable (AList k) where
    fold        (AL xs) = fold        (Prelude.map snd xs)
    foldMap f   (AL xs) = foldMap f   (Prelude.map snd xs)
    foldl   f z (AL xs) = foldl   f z (Prelude.map snd xs)
    foldl1  f   (AL xs) = foldl1  f   (Prelude.map snd xs)
    foldr   f z (AL xs) = foldr   f z (Prelude.map snd xs)
    foldr1  f   (AL xs) = foldr1  f   (Prelude.map snd xs)

instance Traversable (AList k) where
   traverse f (AL xs) =
      fmap AL . traverse (liftM2 fmap ((,).fst) snd . second f) $ xs

instance Eq k => Map AList k where
   eqCmp = const (==)

   empty             = AL []
   singleton k v     = AL [(k,v)]
   doubleton a b p q = AL [(a,b),(p,q)]

   null     (AL xs) = Prelude.null xs
   lookup x (AL xs) = Prelude.lookup x xs

   alter f k (AL xs) =
      let (old, ys) = deleteAndGetBy ((== k).fst) xs
       in case f (fmap snd old) of
               Nothing -> AL ys
               Just v  -> AL $ (k,v) : ys

   delete k (AL xs) = AL$ deleteBy (\a (b,_) -> a == b) k xs

   unionWithKey f (AL xs) (AL ys) =
      AL . uncurry (++) $ updateFirstsBy (\(k,x) (_,y) -> Just (k, f k x y))
                                         ((==) `on` fst)
                                         xs ys

   differenceWithKey f (AL xs) (AL ys) =
      AL . fst $ updateFirstsBy (\(k,x) (_,y) -> fmap ((,) k) (f k x y))
                                (\x y -> fst x == fst y)
                                xs ys

   intersectionWithKey f_ (AL xs_) (AL ys_) = AL$ go f_ xs_ ys_
    where
      go _ [] _ = []
      go f ((k,x):xs) ys =
         let (my,ys') = deleteAndGetBy ((== k).fst) ys
          in case my of
                  Just (_,y) -> (k, f k x y) : go f xs ys'
                  Nothing    ->                go f xs ys

   mapWithKey f (AL xs) = AL $ Prelude.map (\(k,v) -> (k, f k v)) xs

   mapAccumWithKey f z (AL xs) =
      second AL $ mapAccumL (\a (k,v) -> let (a',v') = f a k v
                                          in (a', (k, v')))
                            z xs

   toList (AL xs) = xs
   fromList       = AL . nubBy ((==) `on` fst)
   fromListWith   = AL .: go
    where
      go _ []     = []
      go f (x:xs) =
         -- We add some extra strictness here to match the other map types
         -- (strict in key even for singletons) and because we don't need the
         -- laziness (strict in value)
         let (as,bs) = partition (((==) `on` fst) x) xs
             v       = foldl1' f . Prelude.map snd $ x:as
          in fst x `seq` v `seq` ((fst x, v) : go f bs)

   isSubmapOfBy f_ (AL xs_) (AL ys_) = go f_ xs_ ys_
    where
      go _ []         _  = True
      go f ((k,x):xs) ys =
         let (my,ys') = deleteAndGetBy ((== k).fst) ys
          in case my of
                  Just (_,y) -> f x y && go f xs ys'
                  Nothing    -> False

instance Ord k => OrdMap AList k where
   ordCmp = const compare

   toAscList  = sortBy (       comparing fst) . toList
   toDescList = sortBy (flip $ comparing fst) . toList

   splitLookup k (AL xs) =
      let (ls,gs)  = partition ((< k).fst) xs
          (mx,gs') = deleteAndGetBy ((== k).fst) gs
       in (AL ls, fmap snd mx, AL gs')

deleteAndGetBy :: (a -> Bool) -> [a] -> (Maybe a, [a])
deleteAndGetBy = go []
 where
   go ys _ []     = (Nothing, ys)
   go ys p (x:xs) =
      if p x
         then (Just x, xs ++ ys)
         else go (x:ys) p xs

-- This is from Data.List, just with a more general type signature...
deleteBy :: (a -> b -> Bool) -> a -> [b] -> [b]
deleteBy _  _ []     = []
deleteBy eq x (y:ys) = if x `eq` y then ys else y : deleteBy eq x ys

updateFirstsBy :: (a -> b -> Maybe a)
               -> (a -> b -> Bool)
               -> [a]
               -> [b]
               -> ([a],[b])
updateFirstsBy _ _  []     ys  = ([],ys)
updateFirstsBy f eq (x:xs) ys =
   let (my,ys') = deleteAndGetBy (eq x) ys
    in case my of
            Nothing -> first (x:) $ updateFirstsBy f eq xs ys
            Just y  ->
               case f x y of
                    Just z  -> first (z:) $ updateFirstsBy f eq xs ys'
                    Nothing ->              updateFirstsBy f eq xs ys'

instance Ord k => Map M.Map k where
   eqCmp = const (==)

   empty     = M.empty
   singleton = M.singleton

   null   = M.null
   lookup = M.lookup

   insertWith = M.insertWith'

   update = M.update
   adjust = M.adjust
   delete = M.delete

   alter  = M.alter

   unionWith           = M.unionWith
   differenceWith      = M.differenceWith
   intersectionWith    = M.intersectionWith
   unionWithKey        = M.unionWithKey
   differenceWithKey   = M.differenceWithKey
   intersectionWithKey = M.intersectionWithKey

   map             = M.map
   mapWithKey      = M.mapWithKey
   mapAccum        = M.mapAccum
   mapAccumWithKey = M.mapAccumWithKey

   filter = M.filter

   toList       = M.toList
   fromList     = M.fromList
   fromListWith = M.fromListWith

   serializeToList     = M.toAscList
   deserializeFromList = M.fromDistinctAscList

   isSubmapOfBy = M.isSubmapOfBy

   singletonView m =
      case M.minViewWithKey m of
           Just (a,others) | M.null others -> Just a
           _                               -> Nothing

instance Ord k => OrdMap M.Map k where
   ordCmp = const compare

   toAscList = M.toAscList

   splitLookup = M.splitLookup
   split       = M.split

   minViewWithKey m = maybe (Nothing, m) (first Just) (M.minViewWithKey m)
   maxViewWithKey m = maybe (Nothing, m) (first Just) (M.maxViewWithKey m)

   mapAccumAsc         = M.mapAccum
   mapAccumAscWithKey  = M.mapAccumWithKey
   mapAccumDesc        = mapAccumR
#ifdef TOO_OLD_CONTAINERS
   mapAccumDescWithKey f z =
      second M.fromList . mapAccumR (\a (k,v) -> second ((,) k) $ f a k v) z
                        . M.toAscList
#else
   mapAccumDescWithKey = M.mapAccumRWithKey
#endif

newtype WrappedIntMap k v = IMap (IM.IntMap v) deriving (Eq,Ord)

instance Functor (WrappedIntMap k) where fmap f (IMap m) = IMap (fmap f m)
instance Foldable (WrappedIntMap k) where
    fold        (IMap m) = fold        m
    foldMap f   (IMap m) = foldMap f   m
    foldl   f z (IMap m) = foldl   f z m
    foldl1  f   (IMap m) = foldl1  f   m
    foldr   f z (IMap m) = foldr   f z m
    foldr1  f   (IMap m) = foldr1  f   m

instance Traversable (WrappedIntMap k) where
#ifdef TOO_OLD_CONTAINERS
   traverse  = error "Data.ListTrie.Base.Map :: too old containers, no Traversable IntMap"
   sequenceA = error "Data.ListTrie.Base.Map :: too old containers, no Traversable IntMap"
   mapM      = error "Data.ListTrie.Base.Map :: too old containers, no Traversable IntMap"
   sequence  = error "Data.ListTrie.Base.Map :: too old containers, no Traversable IntMap"
#else
   traverse f (IMap m) = pure IMap <*> traverse f m
   sequenceA (IMap m) = pure IMap <*> sequenceA m
   mapM f (IMap m) = liftM IMap (mapM f m)
   sequence (IMap m) = liftM IMap (sequence m)
#endif

instance Enum k => Map WrappedIntMap k where
   eqCmp = const ((==) `on` fromEnum)

   empty       = IMap IM.empty
   singleton k = IMap . IM.singleton (fromEnum k)

   null     (IMap m) = IM.null m
   lookup k (IMap m) = IM.lookup (fromEnum k) m

   insertWith f k v (IMap m) = IMap$ IM.insertWith f (fromEnum k) v m

   update f k (IMap m) = IMap$ IM.update f (fromEnum k) m
   adjust f k (IMap m) = IMap$ IM.adjust f (fromEnum k) m
   delete   k (IMap m) = IMap$ IM.delete   (fromEnum k) m

   alter  f k (IMap m) = IMap$ IM.alter  f (fromEnum k) m

   unionWith        f (IMap x) (IMap y) = IMap$ IM.unionWith        f x y
   differenceWith   f (IMap x) (IMap y) = IMap$ IM.differenceWith   f x y

#ifdef TOO_OLD_CONTAINERS
   intersectionWith =
      error "Data.ListTrie.Base.Map :: too old containers, Data.IntMap.intersectionWith has restricted type"
#else
   intersectionWith f (IMap x) (IMap y) = IMap$ IM.intersectionWith f x y
#endif

   unionWithKey      f (IMap x) (IMap y) =
      IMap$ IM.unionWithKey (f . toEnum) x y
   differenceWithKey f (IMap x) (IMap y) =
      IMap$ IM.differenceWithKey (f . toEnum) x y

#ifdef TOO_OLD_CONTAINERS
   intersectionWithKey =
      error "Data.ListTrie.Base.Map :: too old containers, Data.IntMap.intersectionWithKey has restricted type"
#else
   intersectionWithKey f (IMap x) (IMap y) =
      IMap$ IM.intersectionWithKey (f . toEnum) x y
#endif

   map             f   (IMap x) = IMap$ IM.map f x
   mapWithKey      f   (IMap x) = IMap$ IM.mapWithKey (f . toEnum) x
   mapAccum        f z (IMap x) = second IMap$ IM.mapAccum f z x
   mapAccumWithKey f z (IMap x) =
      second IMap$ IM.mapAccumWithKey (\a -> f a . toEnum) z x

   filter p (IMap x) = IMap $ IM.filter p x

   toList (IMap m) = Prelude.map (first toEnum) . IM.toList $ m
   fromList        = IMap . IM.fromList       . Prelude.map (first fromEnum)
   fromListWith f  = IMap . IM.fromListWith f . Prelude.map (first fromEnum)

   serializeToList (IMap x) = Prelude.map (first toEnum) . IM.toAscList $ x
   deserializeFromList      = IMap . IM.fromDistinctAscList . Prelude.map (first fromEnum)

   isSubmapOfBy f (IMap x) (IMap y) = IM.isSubmapOfBy f x y

   singletonView (IMap m) =
      case IM.minViewWithKey m of
           Just (a,others) | IM.null others -> Just (first toEnum a)
           _                                -> Nothing

instance Enum k => OrdMap WrappedIntMap k where
   ordCmp = const (compare `on` fromEnum)

   toAscList (IMap m) = Prelude.map (first toEnum) . IM.toAscList $ m

   splitLookup k (IMap m) =
      (\(a,b,c) -> (IMap a, b, IMap c)) . IM.splitLookup (fromEnum k) $ m

   split k (IMap m) = both IMap . IM.split (fromEnum k) $ m

   minViewWithKey o@(IMap m) =
      maybe (Nothing, o) (Just . first toEnum *** IMap) (IM.minViewWithKey m)
   maxViewWithKey o@(IMap m) =
      maybe (Nothing, o) (Just . first toEnum *** IMap) (IM.maxViewWithKey m)

   mapAccumAsc         f z (IMap m) = second IMap $ IM.mapAccum f z m
   mapAccumAscWithKey  f z (IMap m) =
      second IMap $ IM.mapAccumWithKey (\a k -> f a (toEnum k)) z m

#ifdef TOO_OLD_CONTAINERS
   mapAccumDesc        f z (IMap m) =
      second (IMap . IM.fromList)
         . mapAccumR (\a (k,v) -> second ((,) k) $ f a v) z
         . IM.toAscList $ m
   mapAccumDescWithKey f z (IMap m) =
      second (IMap . IM.fromList)
         . mapAccumR (\a (k,v) -> second ((,) k) $ f a (toEnum k) v) z
         . IM.toAscList $ m
#else
   mapAccumDesc        f z (IMap m) = second IMap $ mapAccumR f z m
   mapAccumDescWithKey f z (IMap m) =
      second IMap $ IM.mapAccumRWithKey (\a k -> f a (toEnum k)) z m
#endif
