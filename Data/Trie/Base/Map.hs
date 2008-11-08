-- File created: 2008-11-07 17:30:16

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Trie.Base.Map where

import Control.Arrow ((***), first)
import Control.Monad (join)
import Data.List     (foldl', partition, sortBy)
import Data.Ord      (comparing)
import qualified Data.IntMap as IM
import qualified Data.Map    as M

class Map m k where
   empty     :: m k a
   singleton :: k -> a -> m k a

   null   :: m k a -> Bool
   lookup :: m k a -> k -> Maybe a

   insert     :: m k a -> k -> a -> m k a
   insertWith :: (a -> a -> a) -> m k a -> k -> a -> m k a

   update :: (a -> Maybe a) -> m k a -> k -> m k a
   adjust :: (a -> a)       -> m k a -> k -> m k a
   delete ::                   m k a -> k -> m k a

   unionWith        :: (a -> a -> a)       -> m k a -> m k a -> m k a
   differenceWith   :: (a -> b -> Maybe a) -> m k a -> m k b -> m k a
   intersectionWith :: (a -> b -> c)       -> m k a -> m k b -> m k c
   difference       ::                        m k a -> m k b -> m k a

   foldValues :: (a -> b -> b) -> b -> m k a -> b

   toList   :: m k a -> [(k,a)]
   fromList :: [(k,a)] -> m k a

   isSubmapOfBy :: (a -> b -> Bool) -> m k a -> m k b -> Bool

   singleton  = insert empty
   insert     = insertWith const
   adjust f   = update (Just . f)
   delete     = update (const Nothing)
   difference = differenceWith (\_ _ -> Nothing)

-- Minimal complete definition: toAscList or toDescList, and splitLookup
--
-- fromDistinctAscList and fromDistinctAscList are only used in the default
-- definitions of splitLookup, minViewWithKey and maxViewWithKey, and default
-- to fromList
class Map m k => OrdMap m k where
   toAscList            :: m k a -> [(k,a)]
   toDescList           :: m k a -> [(k,a)]
   fromDistinctAscList  :: [(k,a)] -> m k a
   fromDistinctDescList :: [(k,a)] -> m k a

   splitLookup :: m k a -> k -> (m k a, Maybe a, m k a)
   split       :: m k a -> k -> (m k a,          m k a)

   minViewWithKey :: m k a -> (Maybe (k,a), m k a)
   maxViewWithKey :: m k a -> (Maybe (k,a), m k a)

   toAscList  = reverse . toDescList
   toDescList = reverse . toAscList
   fromDistinctAscList  = fromList
   fromDistinctDescList = fromList

   split m k = let (a,_,b) = splitLookup m k in (a,b)

   minViewWithKey m =
      case toAscList m of
           []     -> (Nothing, m)
           (x:xs) -> (Just x, fromDistinctAscList xs)

   maxViewWithKey m =
      case toDescList m of
           []     -> (Nothing, m)
           (x:xs) -> (Just x, fromDistinctDescList xs)

newtype AList k v = AL [(k,v)]

instance Eq k => Map AList k where
   empty              = AL []
   singleton k v      = AL [(k,v)]

   null (AL xs)       = Prelude.null xs
   lookup (AL xs) x   = Prelude.lookup x xs

   insertWith f (AL xs) k v =
      let (old, ys) = deleteAndGetBy ((== k).fst) xs
       in case old of
               Just (_,v2) -> AL$ (k, f v v2) : ys
               Nothing     -> AL$ (k,   v)    : xs

   update f orig@(AL xs) k =
      let (old, ys) = deleteAndGetBy ((== k).fst) xs
       in case old of
               Nothing    -> orig
               Just (_,v) ->
                  case f v of
                       Nothing -> AL             ys
                       Just v' -> AL $ (k, v') : ys

   delete (AL xs) k = AL$ deleteBy (\a (b,_) -> a == b) k xs

   unionWith f (AL xs) (AL ys) =
      AL$ updateFirstsBy (\(k,x) (_,y) -> fmap ((,) k) (Just $ f x y))
                         (\x y -> fst x == fst y)
                         xs ys

   differenceWith f (AL xs) (AL ys) =
      AL$ updateFirstsBy (\(k,x) (_,y) -> fmap ((,) k) (f x y))
                         (\x y -> fst x == fst y)
                         xs ys

   intersectionWith f_ (AL xs_) (AL ys_) = AL$ go f_ xs_ ys_
    where
      go _ [] _ = []
      go f ((k,x):xs) ys =
         let (my,ys') = deleteAndGetBy ((== k).fst) ys
          in case my of
                  Just (_,y) -> (k, f x y) : go f xs ys'
                  Nothing    ->              go f xs ys

   foldValues f z (AL xs) = foldr (f.snd) z xs

   toList (AL xs) = xs
   fromList       = AL

   isSubmapOfBy f_ (AL xs_) (AL ys_) = go f_ xs_ ys_
    where
      go _ []         _  = True
      go f ((k,x):xs) ys =
         let (my,ys') = deleteAndGetBy ((== k).fst) ys
          in case my of
                  Just (_,y) -> f x y && go f xs ys'
                  Nothing    -> False

instance Ord k => OrdMap AList k where
   toAscList  = sortBy (       comparing fst) . toList
   toDescList = sortBy (flip $ comparing fst) . toList
   
   splitLookup (AL xs) k =
      let (ls,gs) = partition ((< k).fst) xs
       in case gs of
               (k',x):gs' | k' == k -> (AL ls, Just x, AL gs')
               _                    -> (AL ls, Nothing, AL gs)

deleteAndGetBy :: (a -> Bool) -> [a] -> (Maybe a, [a])
deleteAndGetBy = go []
 where
   go ys _ []     = (Nothing, ys)
   go ys p (x:xs) =
      if p x
         then (Just x, xs ++ ys)
         else go (x:ys) p xs

-- These two are from Data.List, just with more general type signatures...
deleteBy :: (a -> b -> Bool) -> a -> [b] -> [b]
deleteBy _  _ []     = []
deleteBy eq x (y:ys) = if x `eq` y then ys else y : deleteBy eq x ys

deleteFirstsBy :: (a -> b -> Bool) -> [a] -> [b] -> [a]
deleteFirstsBy = foldl' . flip . deleteBy . flip

updateFirstsBy :: (a -> b -> Maybe a) -> (a -> b -> Bool) -> [a] -> [b] -> [a]
updateFirstsBy _ _  []     _  = []
updateFirstsBy f eq (x:xs) ys =
   let (my,ys') = deleteAndGetBy (eq x) ys
    in case my of
            Nothing -> x : updateFirstsBy f eq xs ys
            Just y  ->
               case f x y of
                    Just z  -> z : updateFirstsBy f eq xs ys'
                    Nothing ->     updateFirstsBy f eq xs ys'

instance Ord k => Map M.Map k where
   empty        = M.empty
   singleton    = M.singleton

   null         = M.null
   lookup       = flip M.lookup

   insertWith f m k v = M.insertWith' f k v m

   update = flip . M.update
   adjust = flip . M.adjust
   delete = flip   M.delete

   unionWith        = M.unionWith
   differenceWith   = M.differenceWith
   intersectionWith = M.intersectionWith

   foldValues = M.fold

   toList   = M.toList
   fromList = M.fromList

   isSubmapOfBy = M.isSubmapOfBy

instance Ord k => OrdMap M.Map k where
   toAscList            = M.toAscList
   fromDistinctAscList  = M.fromDistinctAscList
   fromDistinctDescList = fromDistinctAscList . reverse

   splitLookup = flip M.splitLookup
   split       = flip M.split

   minViewWithKey m = maybe (Nothing, m) (first Just) (M.minViewWithKey m)
   maxViewWithKey m = maybe (Nothing, m) (first Just) (M.maxViewWithKey m)

newtype IMap k v = IMap (IM.IntMap v)

instance Enum k => Map IMap k where
   empty               = IMap IM.empty
   singleton k v       = IMap$ IM.singleton (fromEnum k) v

   null (IMap m)       = IM.null m
   lookup (IMap m) k   = IM.lookup (fromEnum k) m

   insertWith f (IMap m) k v = IMap$ IM.insertWith f (fromEnum k) v m

   update f (IMap m) k = IMap$ IM.update f (fromEnum k) m
   adjust f (IMap m) k = IMap$ IM.adjust f (fromEnum k) m
   delete   (IMap m) k = IMap$ IM.delete   (fromEnum k) m

   unionWith        f (IMap x) (IMap y) = IMap$ IM.unionWith        f x y
   differenceWith   f (IMap x) (IMap y) = IMap$ IM.differenceWith   f x y

   -- http://hackage.haskell.org/trac/ghc/ticket/2644
   --intersectionWith f (IMap x) (IMap y) = IMap$ IM.intersectionWith f x y
   intersectionWith = undefined

   foldValues f z (IMap m) = IM.fold f z m

   toList (IMap m) = map (first toEnum) . IM.toList $ m
   fromList        = IMap . IM.fromList . map (first fromEnum)

   isSubmapOfBy f (IMap x) (IMap y) = IM.isSubmapOfBy f x y

instance Enum k => OrdMap IMap k where
   toAscList (IMap m)   = map (first toEnum) . IM.toAscList $ m
   fromDistinctAscList  = IMap . IM.fromDistinctAscList . map (first fromEnum)
   fromDistinctDescList = fromDistinctAscList . reverse

   splitLookup (IMap m) =
      (\(a,b,c) -> (IMap a, b, IMap c)) . flip IM.splitLookup m . fromEnum

   split (IMap m) = join (***) IMap . flip IM.split m . fromEnum

   minViewWithKey o@(IMap m) =
      maybe (Nothing, o) (Just . first toEnum *** IMap) (IM.minViewWithKey m)
   maxViewWithKey o@(IMap m) =
      maybe (Nothing, o) (Just . first toEnum *** IMap) (IM.maxViewWithKey m)
