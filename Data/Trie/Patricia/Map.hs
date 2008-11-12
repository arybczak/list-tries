-- File created: 2008-11-12 14:16:48

-- The base implementation of a Patricia trie representing a map with list
-- keys, generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Data.Trie.Patricia.Map where

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

-- Invariant: any (Tr Nothing _ _) has at least two children, all of which are
-- Just or have a Just descendant.
data TrieMap map k v = Tr !(Maybe v) ![k] !(CMap map k v)

type CMap map k v = map k (TrieMap map k v)

-- instances: Eq, Monoid, Foldable, Ord

instance (Map map k, Show k, Show a) => Show (TrieMap map k a) where
   showsPrec p s = showParen (p > 10) $
      showString "fromList " . shows (toList s)

instance (Map map k, Eq k, Read k, Read a) => Read (TrieMap map k a) where
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
null (Tr Nothing p m) | Map.null m = assert (Prelude.null p) True
null _                             = False

-- O(n). The number of elements in the set.
size :: Map map k => TrieMap map k a -> Int
size (Tr v _ m) = Map.foldValues ((+) . size) (fromEnum . isJust $ v) m

-- O(m).
member :: (Map map k, Eq k) => [k] -> TrieMap map k a -> Bool
member k m = isJust (lookup k m)

-- O(m)
lookup :: (Map map k, Eq k) => [k] -> TrieMap map k a -> Maybe a
lookup k (Tr v prefix m) =
   case comparePrefixes prefix k of
        Same                   -> v
        PostFix (Right (x:xs)) -> Map.lookup m x >>= lookup xs
        _ -> Nothing

-- O(?)
isSubmapOf :: (Map map k, Eq k, Eq a)
           => TrieMap map k a -> TrieMap map k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- O(?)
isSubmapOfBy :: (Map map k, Eq k)
             => (a -> b -> Bool) -> TrieMap map k a -> TrieMap map k b -> Bool
isSubmapOfBy f tr1@(Tr v1 pre1 m1) (Tr v2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        DifferedAt _ _ _  -> False
        PostFix (Right _) -> null tr1
        PostFix (Left xs) -> go m2 v1 m1 xs
        Same              -> fromMaybe True (liftM2 f v1 v2)
                          && Map.isSubmapOfBy (isSubmapOfBy f) m1 m2
 where
   go mr vl ml (x:xs) =
      case Map.lookup mr x of
           Nothing              -> False
           Just (Tr vr pre mr') ->
              case comparePrefixes xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go mr' vl ml ys
                   Same              ->
                      fromMaybe True (liftM2 f vl vr) &&
                      Map.isSubmapOfBy (isSubmapOfBy f) ml mr'

   go _ _ _ _ =
      error "Data.Trie.Patricia.Map.isSubmapOfBy :: internal error"

-- O(?)
isProperSubmapOf :: (Map map k, Eq k, Eq a)
                 => TrieMap map k a -> TrieMap map k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- O(?)
isProperSubmapOfBy :: (Map map k, Eq k) => (a -> b -> Bool)
                                        -> TrieMap map k a
                                        -> TrieMap map k b
                                        -> Bool
isProperSubmapOfBy = f False
 where
   f proper g tr1@(Tr v1 pre1 m1) (Tr v2 pre2 m2) =
      case comparePrefixes pre1 pre2 of
           DifferedAt _ _ _  -> False
           PostFix (Right _) -> null tr1
           PostFix (Left xs) -> go proper g m2 v1 m1 xs
           Same              -> same proper g v1 v2 m1 m2

   go proper g mr vl ml (x:xs) =
      case Map.lookup mr x of
           Nothing              -> False
           Just (Tr vr pre mr') ->
              case comparePrefixes xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go proper g mr' vl ml ys
                   Same              -> same proper g vl vr ml mr'

   go _ _ _ _ _ _ =
      error "Data.Trie.Patricia.Map.isProperSubmapOfBy :: internal error"

   same _      _ (Just _) Nothing _  _  = False
   same proper g vl       vr      ml mr =
      let proper' = or [ proper
                       , isNothing vl && isJust vr
                       , not (Map.null $ Map.difference mr ml)
                       ]
       in fromMaybe True (liftM2 g vl vr) &&
          if Map.null ml
             then proper'
             else Map.isSubmapOfBy (f proper' g) ml mr

-- * Construction

-- O(1)
empty :: Map map k => TrieMap map k a
empty = Tr Nothing [] Map.empty

-- O(1)
singleton :: Map map k => [k] -> a -> TrieMap map k a
singleton k v = Tr (Just v) k Map.empty

-- O(m)
insert :: (Map map k, Eq k) => [k] -> a -> TrieMap map k a -> TrieMap map k a
insert = insertWith const

-- O(m)
insertWith :: (Map map k, Eq k)
           => (a -> a -> a) -> [k] -> a -> TrieMap map k a -> TrieMap map k a
insertWith f k v tr@(Tr o prefix m) =
   case comparePrefixes prefix k of
        Same                   -> Tr (Just $ maybe v (f v) o) prefix m
        PostFix (Left  (p:pr)) -> Tr (Just v) k $ Map.singleton p (Tr o pr m)
        PostFix (Right (x:xs)) ->
           if null tr
              then Tr (Just v) k m
              else Tr o prefix $
                      Map.insertWith (\_ old -> insertWith f xs v old)
                                     m x (singleton xs v)

        DifferedAt pr' (p:pr) (x:xs) ->
           Tr Nothing pr' $ Map.doubleton x (singleton xs v) p (Tr o pr m)

        _ -> error "Data.Trie.Patricia.Map.insertWith :: internal error"

-- O(m)
insertWithKey :: (Map map k, Eq k) => ([k] -> a -> a -> a)
                                   -> [k] -> a
                                   -> TrieMap map k a
                                   -> TrieMap map k a
insertWithKey f k = insertWith (f k) k

-- O(m)
delete :: (Map map k, Eq k) => [k] -> TrieMap map k a -> TrieMap map k a
delete = alter (const Nothing)

-- O(m)
adjust :: (Map map k, Eq k)
       => (a -> a) -> [k] -> TrieMap map k a -> TrieMap map k a
adjust f k tr@(Tr v prefix m) =
   case comparePrefixes prefix k of
        Same                   -> Tr (fmap f v) prefix m
        PostFix (Right (x:xs)) -> Tr v prefix $ Map.adjust (adjust f xs) m x
        _                      -> tr

-- O(m)
update :: (Map map k, Eq k)
       => (a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
update f k t = snd (updateLookup f k t)

-- O(m)
updateLookup :: (Map map k, Eq k) => (a -> Maybe a)
                                  -> [k]
                                  -> TrieMap map k a
                                  -> (Maybe a, TrieMap map k a)
updateLookup f k orig@(Tr v prefix m) =
   case comparePrefixes prefix k of
        Same                   -> let v' = v >>= f
                                   in (v' <|> v, Tr v' prefix m)
        PostFix (Right (x:xs)) ->
           let old = Map.lookup m x
            in case old of
                    Nothing -> (Nothing, orig)
                    Just tr ->
                       let (ret, upd) = updateLookup f xs tr
                        in ( ret
                           , Tr v prefix $ if null upd
                                              then Map.delete             m x
                                              else Map.adjust (const upd) m x
                           )
        _ -> (Nothing, orig)

-- O(m)
alter :: (Map map k, Eq k)
      => (Maybe a -> Maybe a) -> [k] -> TrieMap map k a -> TrieMap map k a
alter f k tr@(Tr v prefix m) =
   case comparePrefixes prefix k of
        Same                   -> tryCompress (Tr (f v) prefix m)
        PostFix (Right (x:xs)) ->
           tryCompress . Tr v prefix $
              Map.update (\old -> let new = alter f xs old
                                   in if null new then Nothing else Just new)
                         m x
        _ -> tr

-- * Combination

union :: (Map map k, Eq k)
      => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
union = unionWith const

unionWith :: (Map map k, Eq k) => (a -> a -> a)
                               -> TrieMap map k a
                               -> TrieMap map k a
                               -> TrieMap map k a
unionWith f (Tr v1 pre1 m1) (Tr v2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        Same              ->
           tryCompress $ Tr (unionMaybes f v1 v2) pre1 (mapUnion f m1 m2)
        PostFix remainder ->
           tryCompress $ either (Tr v2 pre2 . mapUnion f m2 . decompress m1 v1)
                                (Tr v1 pre1 . mapUnion f m1 . decompress m2 v2)
                                remainder

        DifferedAt pr (x:xs) (y:ys) ->
           Tr Nothing pr $ Map.doubleton x (Tr v1 xs m1) y (Tr v2 ys m2)

        _ -> can'tHappen
 where
   mapUnion = Map.unionWith . unionWith

   decompress m v (x:xs) = Map.singleton x (Tr v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Map.unionWith :: internal error"

unionWithKey :: (Map map k, Eq k) => ([k] -> a -> a -> a)
                                  -> TrieMap map k a
                                  -> TrieMap map k a
                                  -> TrieMap map k a
unionWithKey = go DL.empty
 where
   go k f (Tr v1 pre1 m1) (Tr v2 pre2 m2) =
      case comparePrefixes pre1 pre2 of
           Same              ->
              let k' = DL.toList $ k `DL.append` DL.fromList pre1
               in tryCompress $
                     Tr (unionMaybes (f k') v1 v2)
                        pre1
                        (mapUnion f k pre1 m1 m2)
           PostFix remainder ->
              tryCompress $
                 either (Tr v2 pre2 . mapUnion f k pre2 m2 . decompress m1 v1)
                        (Tr v1 pre1 . mapUnion f k pre1 m1 . decompress m2 v2)
                        remainder

           DifferedAt pr (x:xs) (y:ys) ->
              Tr Nothing pr $ Map.doubleton x (Tr v1 xs m1) y (Tr v2 ys m2)

           _ -> can'tHappen

   mapUnion f k p = Map.unionWith (go (k `DL.append` DL.fromList p) f)

   decompress m v (x:xs) = Map.singleton x (Tr v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Map.unionWith :: internal error"

unionMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybes f ma mb = maybe mb (\a -> maybe ma (Just . f a) mb) ma

unions :: (Map map k, Eq k) => [TrieMap map k a] -> TrieMap map k a
unions = unionsWith const

unionsWith :: (Map map k, Eq k)
           => (a -> a -> a) -> [TrieMap map k a] -> TrieMap map k a
unionsWith f = foldl' (unionWith f) empty

difference :: (Map map k, Eq k)
           => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
difference = differenceWith (\_ _ -> Nothing)

differenceWith :: (Map map k, Eq k) => (a -> b -> Maybe a)
                                    -> TrieMap map k a
                                    -> TrieMap map k b
                                    -> TrieMap map k a
differenceWith j_ tr1@(Tr v1 pre1 m1) tr2@(Tr v2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        DifferedAt _ _ _   -> tr1
        Same               -> tr j_ v1 v2 pre1 (mapDifference j_ m1 m2)
        PostFix (Left  xs) -> goRight j_ tr1 m2  xs
        PostFix (Right xs) -> goLeft  j_ tr1 tr2 xs

 where
   mapDifference = Map.differenceWith . differenceWith'
   differenceWith' j a b =
      let c = differenceWith j a b
       in if null c then Nothing else Just c

   tr j v v' p m = tryCompress $ Tr (differenceMaybes j v v') p m

   goRight j left@(Tr v pre m) rightMap (x:xs) =
      case Map.lookup rightMap x of
           Nothing                     -> left
           Just right'@(Tr v' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> left
                   Same               -> tr j v v' pre (mapDifference j m m')
                   PostFix (Left  ys) -> goRight j left m'     ys
                   PostFix (Right ys) -> goLeft  j left right' ys

   goRight _ _ _ _ =
      error "Data.Trie.Patricia.Map.differenceWith :: internal error"

   goLeft j (Tr vl prel ml) right@(Tr vr _ mr) (x:xs) =
      tryCompress . Tr vl prel $ Map.adjust f ml x
    where
      f left@(Tr v pre m) =
         case comparePrefixes pre xs of
              DifferedAt _ _ _   -> left
              Same               -> tr j v vr pre (mapDifference j m mr)
              PostFix (Left  ys) -> goRight j left mr    ys
              PostFix (Right ys) -> goLeft  j left right ys

   goLeft _ _ _ _ =
      error "Data.Trie.Patricia.Map.differenceWith :: internal error"

differenceWithKey :: (Map map k, Eq k) => ([k] -> a -> b -> Maybe a)
                                       -> TrieMap map k a
                                       -> TrieMap map k b
                                       -> TrieMap map k a
differenceWithKey = go DL.empty
 where
   go k j tr1@(Tr v1 pre1 m1) tr2@(Tr v2 pre2 m2) =
      case comparePrefixes pre1 pre2 of
           DifferedAt _ _ _   -> tr1
           Same               -> tr j k v1 v2 pre1 m1 m2
           PostFix (Left  xs) -> goRight k j tr1 m2  xs
           PostFix (Right xs) -> goLeft  k j tr1 tr2 xs

   mapDifference k j = Map.differenceWith (differenceWithKey' k j)
   differenceWithKey' k j a b =
      let c = go k j a b
       in if null c then Nothing else Just c

   tr j k v v' p m m' =
      let k' = k `DL.append` DL.fromList p
       in tryCompress $ Tr (differenceMaybes (j $ DL.toList k') v v')
                           p
                           (mapDifference k' j m m')

   goRight k j left@(Tr v pre m) rightMap (x:xs) =
      case Map.lookup rightMap x of
           Nothing                     -> left
           Just right'@(Tr v' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> left
                   Same               -> tr j k v v' pre m m'
                   PostFix (Left  ys) -> goRight k j left m'     ys
                   PostFix (Right ys) -> goLeft  k j left right' ys

   goRight _ _ _ _ _ =
      error "Data.Trie.Patricia.Map.differenceWithKey :: internal error"

   goLeft k j (Tr vl prel ml) right@(Tr vr _ mr) (x:xs) =
      tryCompress . Tr vl prel $ Map.adjust f ml x
    where
      f left@(Tr v pre m) =
         case comparePrefixes pre xs of
              DifferedAt _ _ _   -> left
              Same               -> tr j k v vr pre m mr
              PostFix (Left  ys) -> goRight k j left mr    ys
              PostFix (Right ys) -> goLeft  k j left right ys

   goLeft _ _ _ _ _ =
      error "Data.Trie.Patricia.Map.differenceWithKey :: internal error"

differenceMaybes :: (a -> b -> Maybe a) -> Maybe a -> Maybe b -> Maybe a
differenceMaybes f ma mb = ma >>= \a -> maybe ma (f a) mb

intersection :: (Map map k, Eq k)
             => TrieMap map k a -> TrieMap map k a -> TrieMap map k a
intersection = intersectionWith const

intersectionWith :: forall a b c map k.
                    (Map map k, Eq k) => (a -> b -> c)
                                      -> TrieMap map k a
                                      -> TrieMap map k b
                                      -> TrieMap map k c
intersectionWith j_ (Tr v1 pre1 m1) (Tr v2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        DifferedAt _ _ _ -> empty
        Same             -> tr j_ v1 v2 pre1 (mapIntersect j_ m1 m2)
        PostFix remainder ->
           either (go j_ m2 v1 m1 pre1) (go (flip j_) m1 v2 m2 pre2) remainder

 where
   mapIntersect j = Map.intersectionWith (intersectionWith j)
   tr j v v' p m = tryCompress $ Tr (intersectionMaybes j v v') p m

   -- Has to be explicitly typed or it won't compile!
   go :: (x -> y -> z)
      -> CMap map k y
      -> Maybe x
      -> CMap map k x
      -> [k]
      -> [k]
      -> TrieMap map k z
   go j ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing              -> empty
           Just (Tr v' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> empty
                   Same               -> tr j v v' pre (mapIntersect j mb m')
                   PostFix (Right ys) -> go (flip j) mb v' m' (pre ++ ys) ys
                   PostFix (Left  ys) -> go j m' v mb pre ys

   go _ _ _ _ _ _ =
      error "Data.Trie.Patricia.Map.intersectionWith :: internal error"

intersectionWithKey :: forall a b c map k.
                       (Map map k, Eq k) => ([k] -> a -> b -> c)
                                         -> TrieMap map k a
                                         -> TrieMap map k b
                                         -> TrieMap map k c
intersectionWithKey = f DL.empty
 where
   f k j_ (Tr v1 pre1 m1) (Tr v2 pre2 m2) =
      case comparePrefixes pre1 pre2 of
           DifferedAt _ _ _ -> empty
           Same             -> tr k j_ v1 v2 pre1 m1 m2
           PostFix remainder ->
              either (go k         j_  m2 v1 m1 pre1)
                     (go k (flip . j_) m1 v2 m2 pre2)
                     remainder

   mapIntersect k j = Map.intersectionWith (f k j)

   tr k j v v' p m m' =
      let k' = k `DL.append` DL.fromList p
       in tryCompress $ Tr (intersectionMaybes (j $ DL.toList k') v v')
                           p
                           (mapIntersect k' j m m')

   -- As in intersectionWith: has to be explicitly typed
   go :: DList k
      -> ([k] -> x -> y -> z)
      -> CMap map k y
      -> Maybe x
      -> CMap map k x
      -> [k]
      -> [k]
      -> TrieMap map k z
   go k j ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing              -> empty
           Just (Tr v' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> empty
                   Same               -> tr k j v v' pre mb m'
                   PostFix (Left  ys) -> go k j m' v mb pre ys
                   PostFix (Right ys) ->
                      go k (flip . j) mb v' m' (pre ++ ys) ys

   go _ _ _ _ _ _ _ =
      error "Data.Trie.Patricia.Map.intersectionWithKey :: internal error"

intersectionMaybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
intersectionMaybes = liftM2

-- * Filtering

-- O(n)
filter :: (Map map k, Eq k)
       => (a -> Bool) -> TrieMap map k a -> TrieMap map k a
filter p = filterWithKey (const p)

-- O(n)
filterWithKey :: (Map map k, Eq k)
              => ([k] -> a -> Bool) -> TrieMap map k a -> TrieMap map k a
filterWithKey p = fromList . Prelude.filter (uncurry p) . toList

-- O(n)
partition :: (Map map k, Eq k) => (a -> Bool)
                               -> TrieMap map k a
                               -> (TrieMap map k a, TrieMap map k a)
partition p = partitionWithKey (const p)

-- O(n)
partitionWithKey :: (Map map k, Eq k) => ([k] -> a -> Bool)
                                      -> TrieMap map k a
                                      -> (TrieMap map k a, TrieMap map k a)
partitionWithKey p = join (***) fromList . List.partition (uncurry p) . toList

split :: (OrdMap map k, Ord k)
      => [k] -> TrieMap map k a -> (TrieMap map k a, TrieMap map k a)
split x tr = let (l,_,g) = splitLookup x tr in (l,g)

splitLookup :: (OrdMap map k, Ord k)
            => [k]
            -> TrieMap map k a
            -> (TrieMap map k a, Maybe a, TrieMap map k a)
splitLookup xs orig@(Tr v pre m) =
   case comparePrefixes pre xs of
        Same                     -> (empty, v, tr Nothing pre m)
        DifferedAt _ (p:_) (x:_) ->
           case compare p x of
                LT -> (orig, Nothing, empty)
                GT -> (empty, Nothing, orig)
                EQ -> can'tHappen

        PostFix (Left       _) -> (empty, Nothing, orig)
        PostFix (Right (y:ys)) ->
           let (ml, maybeTr, mg) = Map.splitLookup m y
            in case maybeTr of
                    Nothing  -> (tr v pre ml, Nothing, tr Nothing pre mg)
                    Just tr' ->
                       let (tl, v', tg) = splitLookup ys tr'
                           ml' = if null tl then ml else Map.insert ml y tl
                           mg' = if null tg then mg else Map.insert mg y tg
                        in (tr v pre ml', v', tr Nothing pre mg')

        _ -> can'tHappen
 where
   tr p q r = tryCompress (Tr p q r)
   can'tHappen = error "Data.Trie.Patricia.Map.splitLookup :: internal error"

-- O(n)
mapMaybe :: (Map map k, Eq k)
         => (a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybe = mapMaybeWithKey . const

-- O(n)
mapMaybeWithKey :: (Map map k, Eq k)
                => ([k] -> a -> Maybe b) -> TrieMap map k a -> TrieMap map k b
mapMaybeWithKey f =
   fromList . Maybe.mapMaybe (\(k,v) -> fmap ((,) k) (f k v)) . toList

-- O(n)
mapEither :: (Map map k, Eq k) => (a -> Either b c)
                               -> TrieMap map k a
                               -> (TrieMap map k b, TrieMap map k c)
mapEither = mapEitherWithKey . const

-- O(n)
mapEitherWithKey :: (Map map k, Eq k) => ([k] -> a -> Either b c)
                                      -> TrieMap map k a
                                      -> (TrieMap map k b, TrieMap map k c)
mapEitherWithKey f =
   (fromList *** fromList) . partitionEithers .
   Prelude.map (\(k,v) -> either (Left . (,) k) (Right . (,) k) (f k v)) .
   toList

-- * Mapping

-- O(n)
map :: (Map map k) => (a -> b) -> TrieMap map k a -> TrieMap map k b
map f (Tr v p m) = Tr (fmap f v) p (Map.map (map f) m)

-- O(n)
mapWithKey :: (Map map k)
           => ([k] -> a -> b) -> TrieMap map k a -> TrieMap map k b
mapWithKey = go DL.empty
 where
   go k f (Tr v p m) =
      let k' = k `DL.append` DL.fromList p
       in Tr (fmap (f $ DL.toList k') v)
             p
             (Map.mapWithKey (\x -> go (k' `DL.snoc` x) f) m)

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
genericMapAccum subMapAccum f acc (Tr mv p m) =
   let (acc', v') =
          case mv of
               Nothing -> (acc, Nothing)
               Just v  -> second Just (f acc v)
    in second (Tr v' p) $ subMapAccum (genericMapAccum subMapAccum f) acc' m

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
   go k subMapAccum f acc (Tr mv p m) =
      let k'         = k `DL.append` DL.fromList p
          (acc', v') =
             case mv of
                  Nothing -> (acc, Nothing)
                  Just v  -> second Just (f acc (DL.toList k') v)
       in second (Tr v' p) $
             subMapAccum (\a x -> go (k' `DL.snoc` x) subMapAccum f a) acc' m

-- O(n)
mapKeys :: (Map map k1, Map map k2, Eq k2)
        => ([k1] -> [k2]) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys = mapKeysWith const

-- O(n)
mapKeysWith :: (Map map k1, Map map k2, Eq k2) => (a -> a -> a)
                                               -> ([k1] -> [k2])
                                               -> TrieMap map k1 a
                                               -> TrieMap map k2 a
mapKeysWith j f = fromListWith j . Prelude.map (first f) . toList

-- O(n)
-- needs a name!
mapKeys' :: (Map map k1, Map map k2, Eq k2)
         => (k1 -> k2) -> TrieMap map k1 a -> TrieMap map k2 a
mapKeys' f (Tr v p m) =
   Tr v (Prelude.map f p) $
      Map.fromListWith union .
         Prelude.map (f *** mapKeys' f) .
      Map.toList $ m

-- O(n)
-- TODO: needs a name!
mapKeys'With :: (Map map k1, Map map k2, Eq k2)
             => (a -> a -> a)
             -> (k1 -> k2)
             -> TrieMap map k1 a
             -> TrieMap map k2 a
mapKeys'With j f (Tr v p m) =
   Tr v (Prelude.map f p) $
      Map.fromListWith (unionWith j) .
         Prelude.map (f *** mapKeys'With j f) .
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
   go l f g (Tr mv p m) =
      let
         l' = l `DL.append` DL.fromList p
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l' `DL.snoc` x) f g t) .
            f $ m
       in case mv of
               Just v  -> g (DL.toList l', v) xs
               Nothing ->                     xs

-- O(n*m)
fromList :: (Map map k, Eq k) => [([k],a)] -> TrieMap map k a
fromList = fromListWith const

-- O(n*m)
fromListWith :: (Map map k, Eq k)
             => (a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

-- O(n*m)
fromListWithKey :: (Map map k, Eq k)
                => ([k] -> a -> a -> a) -> [([k],a)] -> TrieMap map k a
fromListWithKey f = foldl' (flip . uncurry $ insertWithKey f) empty


-- * Min/max

-- O(m log b)
findMin :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMin = findMinMax (\(Tr v _ _) -> isJust v)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
findMax :: OrdMap map k => TrieMap map k a -> Maybe ([k], a)
findMax = findMinMax (\(Tr _ _ m) -> Map.null m)
                     (\(Tr v _ _) -> assert (isJust v))
                     (fst . Map.maxViewWithKey)

findMinMax :: Map map k => (TrieMap map k a -> Bool)
                        -> (TrieMap map k a -> ([k],a) -> ([k],a))
                        -> (CMap map k a -> Maybe (k, TrieMap map k a))
                        -> TrieMap map k a
                        -> Maybe ([k], a)
findMinMax _ _ _ tr_ | null tr_ = Nothing
findMinMax f g h tr_ = Just (go f g h tr_)
 where
   go cond base mapView tr@(Tr v pre m) =
      if cond tr
         then base tr (pre, fromJust v)
         else let (k,t) = fromJust (mapView m)
               in first (prepend pre k) (go cond base mapView t)

-- O(m log b)
deleteMin :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMin = maybe empty snd . minView

-- O(m log b)
deleteMax :: OrdMap map k => TrieMap map k a -> TrieMap map k a
deleteMax = maybe empty snd . maxView

-- O(m log b)
minView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
minView = minMaxView (\(Tr v _ _) -> isJust v)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
maxView :: OrdMap map k => TrieMap map k a -> Maybe (([k], a), TrieMap map k a)
maxView = minMaxView (\(Tr _ _ m) -> Map.null m)
                     (\(Tr v _ _) -> assert (isJust v))
                     (fst . Map.maxViewWithKey)

minMaxView :: Map map k
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
   go cond base mapView xs tr@(Tr v pre m) =
      let xs' = xs `DL.append` DL.fromList pre
       in if cond tr
             then base tr ((DL.toList xs', fromJust v), Tr Nothing pre m)
             else let (k,   t)  = fromJust (mapView m)
                      (res, t') = go cond base mapView (xs' `DL.snoc` k) t
                   in ( res
                      , Tr v pre $ if null t'
                                      then Map.delete            m k
                                      else Map.adjust (const t') m k
                      )

-- O(m b)
findPredecessor :: (OrdMap map k, Ord k)
                => TrieMap map k a -> [k] -> Maybe ([k], a)
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go tr@(Tr v pre m) xs =
      case comparePrefixes pre xs of
           Same             -> Nothing
           PostFix (Left _) -> Nothing

           DifferedAt _ (p:_) (x:_) ->
              case compare p x of
                   LT -> findMax tr
                   GT -> Nothing
                   EQ -> can'tHappen

           PostFix (Right (y:ys)) ->
              let predecessor = Map.findPredecessor m y
               in fmap (first (prepend pre y)) (Map.lookup m y >>= flip go ys)
                  <|>
                  case predecessor of
                     Nothing         -> fmap ((,) pre) v
                     Just (best,btr) ->
                        fmap (first (prepend pre best)) (findMax btr)

           _ -> can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Map.findPredecessor :: internal error"

-- O(m b)
findSuccessor :: (OrdMap map k, Ord k)
              => TrieMap map k a -> [k] -> Maybe ([k], a)
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go tr@(Tr _ pre m) xs =
      case comparePrefixes pre xs of
           Same -> do (k,t) <- fst $ Map.minViewWithKey m
                      fmap (first (prepend pre k)) (findMin t)

           DifferedAt _ (p:_) (x:_) ->
              case compare p x of
                   LT -> Nothing
                   GT -> findMin tr
                   EQ -> can'tHappen

           PostFix (Left  _)      -> findMin tr
           PostFix (Right (y:ys)) ->
              let successor = Map.findSuccessor m y
               in fmap (first (prepend pre y)) (Map.lookup m y >>= flip go ys)
                  <|>
                  (successor >>= \(best,btr) ->
                      fmap (first (prepend pre best)) (findMin btr))

           _ -> can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Map.findSuccessor :: internal error"

-- helpers

prepend :: [a] -> a -> [a] -> [a]
prepend prefix key = (prefix++) . (key:)

data PrefixOrdering a
   = Same
   | PostFix (Either [a] [a])
   | DifferedAt [a] [a] [a]

comparePrefixes :: Eq a => [a] -> [a] -> PrefixOrdering a
comparePrefixes = go []
 where
   go _ [] [] = Same
   go _ [] xs = PostFix (Right xs)
   go _ xs [] = PostFix (Left  xs)

   go samePart xs@(a:as) ys@(b:bs) =
      if a == b
         then go (a:samePart) as bs
         else DifferedAt (reverse samePart) xs ys

tryCompress :: Map map k => TrieMap map k a -> TrieMap map k a
tryCompress tr@(Tr v pre m) =
   case Map.singletonView m of
        Just (x, Tr v' pre' subM)
           | isNothing v -> tryCompress $ Tr v' (prepend pre x pre') subM

           | isNothing v' && Map.null subM -> Tr v pre subM

        Nothing | isNothing v -> Tr v [] m

        _ -> tr
