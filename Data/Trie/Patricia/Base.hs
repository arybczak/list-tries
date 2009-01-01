-- File created: 2008-12-28 17:20:14

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, ScopedTypeVariables #-}

module Data.Trie.Patricia.Base
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
import Control.Exception   (assert)
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

   mkTrie :: st a -> [k] -> CMap trie map k a -> trie map k a
   tParts :: trie map k a -> (st a, [k], CMap trie map k a)

type CMap trie map k v = map k (trie map k v)

hasValue, noValue :: Boolable b => b -> Bool
hasValue = toBool
noValue  = not . hasValue

tVal :: Trie trie st map k => trie map k a -> st a
tVal = (\(a,_,_) -> a) . tParts

tMap :: Trie trie st map k => trie map k a -> CMap trie map k a
tMap = (\(_,_,c) -> c) . tParts

-----------------------

-- O(1)
null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = let (v,p,m) = tParts tr
           in noValue v && Map.null m && assert (Prelude.null p) True

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
lookup k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> v
            PostFix (Right (x:xs)) -> maybe altEmpty (lookup xs)
                                            (Map.lookup m x)
            _                      -> altEmpty

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
isSubmapOfBy f_ trl trr =
   let (vl,prel,ml) = tParts trl
       (vr,prer,mr) = tParts trr
    in case comparePrefixes (Map.eqCmp ml) prel prer of
            DifferedAt _ _ _  -> False

            -- Special case here: if the left trie is empty we return True.
            PostFix (Right _) -> null trl
            PostFix (Left xs) -> go f_ mr vl ml xs
            Same              -> (unwrap $ f_ vl vr <|> pure True)
                              && Map.isSubmapOfBy (isSubmapOfBy f_) ml mr
 where
   go f mr vl ml (x:xs) =
      case Map.lookup mr x of
           Nothing -> False
           Just tr ->
              let (vr,pre,mr') = tParts tr
               in case comparePrefixes (Map.eqCmp mr) xs pre of
                     DifferedAt _ _ _  -> False
                     PostFix (Right _) -> False
                     PostFix (Left ys) -> go f mr' vl ml ys
                     Same              ->
                        (unwrap $ f vl vr <|> pure True)
                     && Map.isSubmapOfBy (isSubmapOfBy f) ml mr'

   go _ _ _ _ [] =
      error "Data.Trie.Patricia.Base.isSubmapOfBy :: internal error"

-- O(min(n1,n2))
isProperSubmapOfBy :: ( Boolable (st a), Boolable (st b)
                      , Alt st Bool
                      , Trie trie st map k
                      )
                   => (st a -> st b -> st Bool)
                   -> trie map k a
                   -> trie map k b
                   -> Bool
isProperSubmapOfBy = f False
 where
   f proper g trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _  -> False

              -- Special case, as in isSubsetOf.
              --
              -- Note that properness does not affect this: if we hit this
              -- case, we already know that the right trie is nonempty.
               PostFix (Right _) -> null trl
               PostFix (Left xs) -> go proper g mr vl ml xs
               Same              -> same proper g vl vr ml mr

   go proper g mr vl ml (x:xs) =
      case Map.lookup mr x of
           Nothing -> False
           Just tr ->
              let (vr,pre,mr') = tParts tr
               in case comparePrefixes (Map.eqCmp mr) xs pre of
                       DifferedAt _ _ _  -> False
                       PostFix (Right _) -> False
                       PostFix (Left ys) -> go proper g mr' vl ml ys
                       Same              -> same proper g vl vr ml mr'

   go _ _ _ _ _ [] =
      error "Data.Trie.Patricia.Base.isProperSubmapOfBy :: internal error"

   same _      _ vl vr _  _  | hasValue vl && noValue vr = False
   same proper g vl vr ml mr =
      -- As the non-Patricia version, so does this seem suboptimal.
      let proper' = or [ proper
                       , noValue vl && hasValue vr
                       , not (Map.null $ Map.difference mr ml)
                       ]
       in (unwrap $ g vl vr <|> pure True) &&
          if Map.null ml
             then proper'
             else Map.isSubmapOfBy (f proper' g) ml mr

-- * Construction

-- O(1)
empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty [] Map.empty

-- O(1)
singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton k v = mkTrie (pure v) k Map.empty

-- O(m)
insert :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

-- O(m)
insertWith :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith = genericInsertWith (<$>)

-- O(m)
insertWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
            => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith' = genericInsertWith (<$!>)

genericInsertWith :: (Alt st a, Boolable (st a), Trie trie st map k)
                  => ((a -> a) -> st a -> st a)
                  -> (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
genericInsertWith (<$$>) f k new tr =
   let (old,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same -> mkTrie ((f new <$$> old) <|> pure new) prefix m

            PostFix (Left (p:pr)) -> mkTrie (pure new) k
                                            (Map.singleton p (mkTrie old pr m))
            PostFix (Right (x:xs)) ->
               -- Minor optimization: instead of tryCompress we just check for
               -- the case of an empty trie
               if null tr
                  then singleton k new
                  else mkTrie old prefix $
                          Map.insertWith
                             (\_ oldt ->
                                genericInsertWith (<$$>) f xs new oldt)
                             m x (singleton xs new)

            DifferedAt pr' (p:pr) (x:xs) ->
               mkTrie altEmpty pr' $ Map.doubleton x (singleton xs new)
                                                   p (mkTrie old pr m)

            _ -> error "Data.Trie.Patricia.Base.insertWith :: internal error"

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
genericAdjust myFmap f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> mkTrie (myFmap f v) prefix m
            PostFix (Right (x:xs)) ->
               mkTrie v prefix $ Map.adjust (genericAdjust myFmap f xs) m x
            _                      -> tr

-- O(m)
updateLookup :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> st a) -> [k] -> trie map k a -> (st a, trie map k a)
updateLookup f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> let v' = if hasValue v
                                                  then f (unwrap v)
                                                  else v
                                       in (v' <|> v, mkTrie v' prefix m)
            PostFix (Right (x:xs)) ->
               case Map.lookup m x of
                    Nothing  -> (altEmpty, tr)
                    Just tr' ->
                       let (ret, upd) = updateLookup f xs tr'
                        in ( ret
                           , mkTrie v prefix $
                                if null upd
                                   then Map.delete m x
                                   else Map.adjust (const upd) m x
                           )
            _ -> (altEmpty, tr)

-- O(m)
alter :: (Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter = genericAlter (flip const)

-- O(m)
alter' :: (Boolable (st a), Trie trie st map k)
       => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter' = genericAlter seq

genericAlter :: (Boolable (st a), Trie trie st map k)
             => (st a -> trie map k a -> trie map k a)
             -> (st a -> st a) -> [k] -> trie map k a -> trie map k a
genericAlter seeq f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   ->
               let v' = f v
                in v' `seeq` tryCompress (mkTrie v' prefix m)
            PostFix (Right (x:xs)) ->
               tryCompress . mkTrie v prefix $
                  Map.update (\t -> let new = genericAlter seeq f xs t
                                     in if null new then Nothing else Just new)
                             m x
            _ -> tr

-- * Combination

-- The *Key versions are mostly rewritten from the basic ones: they have an
-- additional O(m) cost from keeping track of the key, which is why the basic
-- ones can't just call them.

-- O(min(n1,n2))
unionWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f = genericUnionWith (unionVals f) (flip const)

-- O(min(n1,n2))
unionWith' :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith' f = genericUnionWith (unionVals' f) seq

genericUnionWith :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => (st a -> st a -> st a)
                 -> (st a -> trie map k a -> trie map k a)
                 -> trie map k a
                 -> trie map k a
                 -> trie map k a
genericUnionWith valUnion seeq tr1 tr2 =
   let (v1,pre1,m1) = tParts tr1
       (v2,pre2,m2) = tParts tr2
    in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
            Same ->
               let v = valUnion v1 v2
                in v `seeq` (tryCompress.mkTrie v pre1 $
                                            mapUnion valUnion seeq m1 m2)

            PostFix remainder ->
               tryCompress $
                  either
                     (mkTrie v2 pre2 . mapUnion valUnion seeq m2 .
                        decompress m1 v1)
                     (mkTrie v1 pre1 . mapUnion valUnion seeq m1 .
                        decompress m2 v2)
                     remainder

            DifferedAt pr (x:xs) (y:ys) ->
               mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                  y (mkTrie v2 ys m2)

            _ -> can'tHappen
 where
   mapUnion vu s = Map.unionWith (genericUnionWith vu s)

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Base.unionWith :: internal error"

-- O(min(n1,n2))
unionWithKey :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
             => ([k] -> a -> a -> a)
             -> trie map k a
             -> trie map k a
             -> trie map k a
unionWithKey = genericUnionWithKey unionVals (flip const)

-- O(min(n1,n2))
unionWithKey' :: ( Alt st a, Boolable (st a), Unionable st a
                 , Trie trie st map k
                 )
              => ([k] -> a -> a -> a)
              -> trie map k a
              -> trie map k a
              -> trie map k a
unionWithKey' = genericUnionWithKey unionVals' seq

genericUnionWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
                    => ((a -> a -> a) -> st a -> st a -> st a)
                    -> (st a -> trie map k a -> trie map k a)
                    -> ([k] -> a -> a -> a)
                    -> trie map k a
                    -> trie map k a
                    -> trie map k a
genericUnionWithKey = go DL.empty
 where
   go k valUnion seeq j tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               Same ->
                  let k' = DL.toList $ k `DL.append` DL.fromList pre1
                      v  = valUnion (j k') v1 v2
                   in v `seeq`
                         (tryCompress.mkTrie v pre1 $
                            mapUnion valUnion seeq j k pre1 m1 m2)

               PostFix remainder ->
                  tryCompress $
                     either
                        (mk v2 pre2 . mapUnion valUnion seeq j k pre2 m2 .
                           decompress m1 v1)
                        (mk v1 pre1 . mapUnion valUnion seeq j k pre1 m1 .
                           decompress m2 v2)
                        remainder

               DifferedAt pr (x:xs) (y:ys) ->
                  mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                     y (mkTrie v2 ys m2)

               _ -> can'tHappen

   mk = mkTrie

   mapUnion v s j k p =
      Map.unionWithKey $
         \x -> go (k `DL.append` DL.fromList p `DL.snoc` x) v s j

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Base.unionWithKey :: internal error"

unionsWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
           => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith j = foldl' (unionWith j) empty

unionsWith' :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
            => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith' j = foldl' (unionWith' j) empty

unionsWithKey :: ( Alt st a, Boolable (st a)
                 , Unionable st a, Trie trie st map k
                 )
              => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey j = foldl' (unionWithKey j) empty

unionsWithKey' :: ( Alt st a, Boolable (st a)
                  , Unionable st a, Trie trie st map k
                  )
               => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey' j = foldl' (unionWithKey' j) empty

-- O(min(n1,n2))
differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith = genericDifferenceWith (flip const)

-- O(min(n1,n2))
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
genericDifferenceWith seeq_ j_ tr1 tr2 =
   let (v1,pre1,m1) = tParts tr1
       (v2,pre2,m2) = tParts tr2
    in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
            DifferedAt _ _ _   -> tr1
            Same               -> mk seeq_ j_ v1 v2 pre1 m1 m2
            PostFix (Left  xs) -> goRight seeq_ j_ tr1 m2  xs
            PostFix (Right xs) -> goLeft  seeq_ j_ tr1 tr2 xs
 where
   mapDifference seeq j = Map.differenceWith (dw seeq j)
   dw seeq j a b =
      let c = genericDifferenceWith seeq j a b
       in if null c then Nothing else Just c

   mk seeq j v v' p m m' =
      let vd = differenceVals j v v'
       in vd `seeq` (tryCompress.mkTrie vd p $ mapDifference seeq j m m')

   -- See the comment in 'intersection' for a longish example of the idea
   -- behind this, which is basically that if we see two prefixes like "foo"
   -- and "foobar", we traverse the "foo" trie looking for "bar". Then if we
   -- find "barbaz", we traverse the "foobar" trie looking for "baz", and so
   -- on.
   --
   -- We have two functions for the two tries because set difference is a
   -- noncommutative operation.
   goRight seeq j left rightMap (x:xs) =
      let (v,pre,m) = tParts left
       in case Map.lookup rightMap x of
               Nothing     -> left
               Just right' ->
                  let (v',pre',m') = tParts right'
                   in case comparePrefixes (Map.eqCmp m) xs pre' of
                           DifferedAt _ _ _   -> left
                           Same               -> mk seeq j v v' pre m m'
                           PostFix (Left  ys) -> goRight seeq j left m'     ys
                           PostFix (Right ys) -> goLeft  seeq j left right' ys

   goRight _ _ _ _ [] = can'tHappen

   goLeft seeq j left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.adjust f ml x
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> left'
                  Same               -> mk seeq j v vr pre m mr
                  PostFix (Left  ys) -> goRight seeq j left' mr    ys
                  PostFix (Right ys) -> goLeft  seeq j left' right ys

   goLeft _ _ _ _ [] = can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.differenceWith :: internal error"

-- O(min(n1,n2))
differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey = genericDifferenceWithKey (flip const)

-- O(min(n1,n2))
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
   go k seeq_ j_ tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               DifferedAt _ _ _   -> tr1
               Same               -> mk seeq_ j_ k v1 v2 pre1 m1 m2
               PostFix (Left  xs) -> goRight k seeq_ j_ tr1 m2  xs
               PostFix (Right xs) -> goLeft  k seeq_ j_ tr1 tr2 xs

   mapDifference k seeq j =
      Map.differenceWithKey (\x -> dw (k `DL.snoc` x) seeq j)

   dw k seeq j a b =
      let c = go k seeq j a b
       in if null c then Nothing else Just c

   mk seeq j k v v' p m m' =
      let k' = k `DL.append` DL.fromList p
          vd = differenceVals (j $ DL.toList k') v v'
       in vd `seeq` (tryCompress.mkTrie vd p $ mapDifference k' seeq j m m')

   goRight k seeq j left rightMap (x:xs) =
      let (v,pre,m) = tParts left
       in case Map.lookup rightMap x of
               Nothing    -> left
               Just right ->
                  let (v',pre',m') = tParts right
                   in case comparePrefixes (Map.eqCmp m) xs pre' of
                           DifferedAt _ _ _   -> left
                           Same               -> mk seeq j k v v' pre m m'
                           PostFix (Left  ys) -> goRight k seeq j left m'    ys
                           PostFix (Right ys) -> goLeft  k seeq j left right ys

   goRight _ _ _ _ _ [] = can'tHappen

   goLeft k seeq j left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.adjust f ml x
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> left'
                  Same               -> mk seeq j k v vr pre m mr
                  PostFix (Left  ys) -> goRight k seeq j left' mr    ys
                  PostFix (Right ys) -> goLeft  k seeq j left' right ys

   goLeft _ _ _ _ _ [] = can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.differenceWithKey :: internal error"

-- O(min(n1,n2))
intersectionWith :: ( Alt st c, Boolable (st c)
                    , Intersectable st a b c, Intersectable st b a c
                    , Trie trie st map k
                    )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith f = genericIntersectionWith (intersectionVals f) (flip const)

-- O(min(n1,n2))
intersectionWith' :: ( Alt st c, Boolable (st c)
                     , Intersectable st a b c, Intersectable st b a c
                     , Trie trie st map k
                     )
                  => (a -> b -> c)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k c
intersectionWith' f = genericIntersectionWith (intersectionVals' f) seq

genericIntersectionWith :: forall a b c k map st trie.
                           ( Alt st c, Boolable (st c)
                           , Trie trie st map k
                           )
                        => (st a -> st b -> st c)
                        -> (st c -> trie map k c -> trie map k c)
                        -> trie map k a
                        -> trie map k b
                        -> trie map k c
genericIntersectionWith valIsect_ seeq_ trl trr =
   let (vl,prel,ml) = tParts trl
       (vr,prer,mr) = tParts trr
    in case comparePrefixes (Map.eqCmp ml) prel prer of
            DifferedAt _ _ _  -> empty
            Same              -> mk valIsect_ seeq_ vl vr prel ml mr
            PostFix remainder ->
               -- use the one with a longer prefix as the base for the
               -- intersection, and descend into the map of the one with a
               -- shorter prefix
               either (go       valIsect_  seeq_ mr vl ml (DL.fromList prel))
                      (go (flip valIsect_) seeq_ ml vr mr (DL.fromList prer))
                      remainder
 where
   -- Can't pointlessify due to the monomorphism restriction
   mapIntersect valIsect seeq =
      Map.intersectionWith (genericIntersectionWith valIsect seeq)

   mk valIsect seeq v v' p m m' =
      let vi = valIsect v v'
       in vi `seeq` (tryCompress.mkTrie vi p $ mapIntersect valIsect seeq m m')

   -- Polymorphic recursion in 'go' (valIsect :: st a -> st b -> st c ---> st b
   -- -> st a -> st c) means that it has to be explicitly typed in order to
   -- compile.

   -- Like goLeft and goRight in 'difference', but handles both cases (since
   -- this is a commutative operation).
   --
   -- Traverse the map given as the 1st argument, looking for anything that
   -- begins with the given key (x:xs).
   --
   -- If it's found, great: make an intersected trie out of the trie found in
   -- the map and the boolean, map, and prefix given.
   --
   -- If it's not found but might still be, there are two cases.
   --
   -- 1. Say we've got the following two TrieSets:
   --
   -- fromList ["car","cat"]
   -- fromList ["car","cot"]
   --
   -- i.e. (where <> is stuff we don't care about here)
   --
   -- Tr False "ca" (fromList [('r', Tr True ""  <>),<>])
   -- Tr False "c"  (fromList [('a', Tr True "r" <>),<>])
   --
   -- We came in here with (x:xs) = "a", the remainder of comparing "ca" and
   -- "c". We're looking for anything that begins with "ca" from the children
   -- of the "c".
   --
   -- We find the prefix pre' = "r", and comparePrefixes gives PostFix (Right
   -- "r"). So now we want anything beginning with "car" in the other trie. We
   -- switch to traversing the other trie, i.e. the other given map: the
   -- children of "ca".
   --
   -- 2. Say we have the following:
   --
   -- fromList ["cat"]
   -- fromList ["cat","cot","cap"]
   --
   -- i.e.
   --
   -- Tr True "cat" <>
   -- Tr False "c" (fromList [('a',Tr False "" (fromList [('t',<>)])),<>])
   --
   -- (x:xs) = "at" now, and we find pre' = "". We get PostFix (Left "t"). This
   -- means that we're staying in the same trie, just looking for "t" now
   -- instead of "at". So we jump into the m' map.
   --
   -- Note that the prefix and boolean don't change: we've already got "ca",
   -- and we'd still like "cat" so we keep the True from there.
   go :: (Alt st z, Boolable (st z))
      => (st x -> st y -> st z)
      -> (st z -> trie map k z -> trie map k z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go valIsect seeq ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               ->
                          mk valIsect seeq v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in go (flip valIsect) seeq mb v' m' nextPre ys
                       PostFix (Left  ys) ->
                              go       valIsect  seeq m' v  mb pre     ys

   go _ _ _ _ _ _ [] =
      error "Data.Trie.Patricia.Map.intersectionWith :: internal error"

-- O(min(n1,n2))
intersectionWithKey :: ( Alt st c, Boolable (st c)
                       , Intersectable st a b c, Intersectable st b a c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey = genericIntersectionWithKey intersectionVals (flip const)

-- O(min(n1,n2))
intersectionWithKey' :: ( Alt st c, Boolable (st c)
                        , Intersectable st a b c, Intersectable st b a c
                        , Trie trie st map k
                        )
                     => ([k] -> a -> b -> c)
                     -> trie map k a
                     -> trie map k b
                     -> trie map k c
intersectionWithKey' = genericIntersectionWithKey intersectionVals' seq

genericIntersectionWithKey :: forall a b c k map st trie.
                              (Alt st c, Boolable (st c), Trie trie st map k)
                           => ((a -> b -> c) -> st a -> st b -> st c)
                           -> (st c -> trie map k c -> trie map k c)
                           -> ([k] -> a -> b -> c)
                           -> trie map k a
                           -> trie map k b
                           -> trie map k c
genericIntersectionWithKey = main DL.empty
 where
   main k valIsect seeq j trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _ -> empty
               Same             -> mk k valIsect seeq j vl vr prel ml mr
               PostFix remainder ->
                  let prel' = DL.fromList prel
                      prer' = DL.fromList prer
                   in either
                         (go k        valIsect  seeq       j  mr vl ml prel')
                         (go k (flipp valIsect) seeq (flip.j) ml vr mr prer')
                         remainder

   mk k valIsect seeq j v v' p m m' =
      let k' = k `DL.append` DL.fromList p
          vi = valIsect (j $ DL.toList k') v v'
       in vi `seeq` (tryCompress.mkTrie vi p $
                                    mapIntersect k' valIsect seeq j m m')

   mapIntersect k valIsect seeq j =
      Map.intersectionWithKey (\x -> main (k `DL.snoc` x) valIsect seeq j)

   flipp :: ((x -> y -> z) -> st x -> st y -> st z)
         -> ((y -> x -> z) -> st y -> st x -> st z)
   flipp f g = flip $ f (flip g)

   -- See intersectionWith: this explicit type is necessary
   go :: (Alt st z, Boolable (st z))
      => DList k
      -> ((x -> y -> z) -> st x -> st y -> st z)
      -> (st z -> trie map k z -> trie map k z)
      -> ([k] -> x -> y -> z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go k valIsect seeq j ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               ->
                          mk k valIsect seeq j v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in go k (flipp valIsect) seeq (flip.j)
                                 mb v' m' nextPre ys
                       PostFix (Left  ys) ->
                              go k        valIsect  seeq       j
                                 m' v  mb pre     ys

   go _ _ _ _ _ _ _ _ [] =
      error "Data.Trie.Patricia.Map.intersectionWithKey :: internal error"

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

-- O(m)
split :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
      => [k] -> trie map k a -> (trie map k a, trie map k a)
split xs tr = let (l,_,g) = splitLookup xs tr in (l,g)

-- O(m)
splitLookup :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
            => [k]
            -> trie map k a
            -> (trie map k a, st a, trie map k a)
splitLookup xs tr =
   let (v,pre,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) pre xs of
            Same                     -> (empty, v, mk altEmpty pre m)
            DifferedAt _ (p:_) (x:_) ->
               case Map.ordCmp m p x of
                    LT -> (tr, altEmpty, empty)
                    GT -> (empty, altEmpty, tr)
                    EQ -> can'tHappen

            PostFix (Left  _)      -> (empty, altEmpty, tr)
            PostFix (Right (y:ys)) ->
               let (ml, maybeTr, mg) = Map.splitLookup m y
                in case maybeTr of
                        -- Prefix goes in left side of split since it's shorter
                        -- than the given key and thus lesser
                        Nothing  -> (mk v pre ml, altEmpty, mk altEmpty pre mg)
                        Just tr' ->
                           let (tl, v', tg) = splitLookup ys tr'
                               ml' = if null tl then ml else Map.insert ml y tl
                               mg' = if null tg then mg else Map.insert mg y tg
                            in (mk v pre ml', v', mk altEmpty pre mg')
            _ -> can'tHappen
 where
   mk v pre m = tryCompress (mkTrie v pre m)
   can'tHappen = error "Data.Trie.Patricia.Base.splitLookup :: internal error"

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
mapKeys'With :: ( Alt st a, Boolable (st a), Unionable st a
                , Trie trie st map k1, Trie trie st map k2
                )
             => (a -> a -> a)
             -> (k1 -> k2)
             -> trie map k1 a
             -> trie map k2 a
mapKeys'With j f tr =
   let (v,p,m) = tParts tr
    in mkTrie v (Prelude.map f p) $
          Map.fromListWith (unionWith j) .
             Prelude.map (f *** mapKeys'With j f) .
          Map.toList $ m

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
   go l tolist add tr =
      let (v,p,m) = tParts tr
          l'      = l `DL.append` DL.fromList p
          xs      =
             DL.concat .
             Prelude.map (\(x,t) -> go (l' `DL.snoc` x) tolist add t) .
             tolist $ m
       in if hasValue v
             then add (DL.toList l', unwrap v) xs
             else                              xs

-- O(n m)
fromList :: (Alt st a, Boolable (st a), Trie trie st map k)
         => [([k],a)] -> trie map k a
fromList = fromListWith const

-- O(n m)
fromListWith :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

-- O(n m)
fromListWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith' f = foldl' (flip . uncurry $ insertWith' f) empty

-- O(n m)
fromListWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
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
deleteMin = maybe empty snd . minView

-- O(m)
deleteMax :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
          => trie map k a -> trie map k a
deleteMax = maybe empty snd . maxView

-- O(m)
minView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> Maybe (([k], a), trie map k a)
minView = minMaxView (hasValue.tVal) (fst . Map.minViewWithKey)

-- O(m)
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
      let (v,pre,m) = tParts tr
       in if isWanted tr
             then (([], unwrap v), mkTrie altEmpty pre m)
             else let (k,      tr')  = fromJust (mapView m)
                      (minMax, tr'') = go isWanted mapView tr'
                   in ( first (k:) minMax
                      , mkTrie v pre $ if null tr''
                                          then Map.delete              m k
                                          else Map.adjust (const tr'') m k
                      )

-- O(m)
findPredecessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => trie map k a -> [k] -> Maybe ([k], a)
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go tr xs =
      let (v,pre,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) pre xs of
               Same             -> Nothing
               PostFix (Left _) -> Nothing

               DifferedAt _ (p:_) (x:_) ->
                  case Map.ordCmp m p x of
                       LT -> findMax tr
                       GT -> Nothing
                       EQ -> can'tHappen

               -- See comment in non-Patricia version for explanation of
               -- algorithm
               PostFix (Right (y:ys)) ->
                  let predecessor = Map.findPredecessor m y
                   in first (prepend pre y) <$> (Map.lookup m y >>= flip go ys)
                      <|>
                      case predecessor of
                           Nothing         ->
                              if hasValue v
                                 then Just (pre, unwrap v)
                                 else Nothing
                           Just (best,btr) ->
                              first (prepend pre best) <$> findMax btr
               _ -> can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.findPredecessor :: internal error"

-- O(m)
findSuccessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
              => trie map k a -> [k] -> Maybe ([k], a)
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go tr xs =
      let (_,pre,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) pre xs of
               Same -> do (k,t) <- fst $ Map.minViewWithKey m
                          first (prepend pre k) <$> findMin t

               DifferedAt _ (p:_) (x:_) ->
                  case Map.ordCmp m p x of
                       LT -> Nothing
                       GT -> findMin tr
                       EQ -> can'tHappen

               PostFix (Left _)       -> findMin tr
               PostFix (Right (y:ys)) ->
                  let successor = Map.findSuccessor m y
                   in first (prepend pre y) <$> (Map.lookup m y >>= flip go ys)
                      <|>
                      (successor >>= \(best,btr) ->
                         first (prepend pre best) <$> findMin btr)

               _ -> can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.findSuccessor :: internal error"

-- * Trie-only operations

-- O(s) where s is the input
addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix xs tr =
   let (v,pre,m) = tParts tr
    in mkTrie v (xs ++ pre) m

-- O(1)
splitPrefix :: (Alt st a, Trie trie st map k)
            => trie map k a -> ([k], trie map k a)
splitPrefix tr =
   let (v,pre,m) = tParts tr
    in (pre, mkTrie v [] m)

-- O(m)
lookupPrefix :: (Alt st a, Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
lookupPrefix xs tr =
   let (_,pre,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) pre xs of
            Same                   -> tr
            PostFix (Left _)       -> tr
            DifferedAt _ _ _       -> empty
            PostFix (Right (y:ys)) ->
               case Map.lookup m y of
                    Nothing  -> empty
                    Just tr' -> lookupPrefix ys tr'

            _ -> error "Data.Trie.Patricia.Base.lookupPrefix :: internal error"

-- helpers

prepend :: [a] -> a -> [a] -> [a]
prepend prefix key = (prefix++) . (key:)

data PrefixOrdering a
   = Same
   | PostFix (Either [a] [a])
   | DifferedAt [a] [a] [a]

-- Same                  If they're equal.
-- PostFix (Left  xs)    If the first argument was longer: xs is the remainder.
-- PostFix (Right xs)    Likewise, but for the second argument.
-- DifferedAt pre xs ys  Otherwise. pre is the part that was the same and
--                       xs and ys are the remainders for the first and second
--                       arguments respectively.
--
--                       all (pre `isPrefixOf`) [xs,ys] --> True.
comparePrefixes :: (a -> a -> Bool) -> [a] -> [a] -> PrefixOrdering a
comparePrefixes = go []
 where
   go _ _ [] [] = Same
   go _ _ [] xs = PostFix (Right xs)
   go _ _ xs [] = PostFix (Left  xs)

   go samePart (===) xs@(a:as) ys@(b:bs) =
      if a === b
         then go (a:samePart) (===) as bs
         else DifferedAt (reverse samePart) xs ys

-- After modifying the trie, compress a trie node into the prefix if possible.
--
-- Doesn't recurse into children, only checks if this node and its child can be
-- joined into one. Does it repeatedly, though, until it can't compress any
-- more.
--
-- Note that this is a sledgehammer: for optimization, instead of using this in
-- every function, we could write a separate tryCompress for each function,
-- checking only for those cases that we know can arise. This has been done in
-- 'insert', at least, but not in many places.
tryCompress :: (Boolable (st a), Trie trie st map k)
            => trie map k a -> trie map k a
tryCompress tr =
   let (v,pre,m) = tParts tr
    in case Map.singletonView m of

          -- We can compress the trie if there is only one child
          Just (x, tr')
             -- If the parent is empty, we can collapse it into the child
             | noValue v -> tryCompress $ mkTrie v' (prepend pre x pre') subM

             -- If the parent is full and the child is empty and childless, the
             -- child is irrelevant
             | noValue v' && Map.null subM -> mkTrie v pre subM
           where
             (v',pre',subM) = tParts tr'

          -- If the trie is empty, make sure the prefix is as well.
          --
          -- This case can arise in 'intersectionWith', at least.
          Nothing | noValue v && Map.null m -> mkTrie v [] m

          -- Otherwise, leave it unchanged.
          _ -> tr
