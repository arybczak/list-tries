-- File created: 2008-12-28 17:20:14

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, ScopedTypeVariables #-}

module Data.Trie.Patricia.Base
   ( Trie(..)
   , null, size, member, notMember, lookup, lookupWithDefault
   , isSubmapOfBy, isProperSubmapOfBy
   , empty, singleton
   , insert, insertWith, delete, adjust, updateLookup, alter
   , unionWith, unionWithKey, unionsWith, unionsWithKey
   , differenceWith, differenceWithKey, intersectionWith, intersectionWithKey
   , filterWithKey, partitionWithKey
   , split, splitLookup
   , mapKeysWith, mapKeys'With
   , foldWithKey, foldAscWithKey, foldDescWithKey
   , toList, toAscList, toDescList, fromList, fromListWith, fromListWithKey
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

null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = let (v,p,m) = tParts tr
           in noValue v && Map.null m && assert (Prelude.null p) True

size :: (Boolable (st a), Trie trie st map k) => trie map k a -> Int
size tr = Map.foldValues ((+) . size) (fromEnum.hasValue.tVal $ tr) (tMap tr)

member :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
member k tr = hasValue (lookup k tr)

notMember :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> Bool
notMember k tr = not (member k tr)

lookup :: (Alt st a, Trie trie st map k) => [k] -> trie map k a -> st a
lookup k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> v
            PostFix (Right (x:xs)) -> maybe altEmpty (lookup xs)
                                            (Map.lookup m x)
            _                      -> altEmpty

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

empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty [] Map.empty

singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton k v = mkTrie (pure v) k Map.empty

insert :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

insertWith :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith f k new tr =
   let (old,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same -> mkTrie ((f new <$> old) <|> pure new) prefix m

            PostFix (Left (p:pr)) -> mkTrie (pure new) k
                                            (Map.singleton p (mkTrie old pr m))
            PostFix (Right (x:xs)) ->
               -- Minor optimization: instead of tryCompress we just check for
               -- the case of an empty trie
               if null tr
                  then singleton k new
                  else mkTrie old prefix $
                          Map.insertWith (\_ oldt -> insertWith f xs new oldt)
                                         m x (singleton xs new)

            DifferedAt pr' (p:pr) (x:xs) ->
               mkTrie altEmpty pr' $ Map.doubleton x (singleton xs new)
                                                   p (mkTrie old pr m)

            _ -> error "Data.Trie.Patricia.Base.insertWith :: internal error"

delete :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> trie map k a -> trie map k a
delete = alter (const altEmpty)

adjust :: Trie trie st map k
       => (a -> a) -> [k] -> trie map k a -> trie map k a
adjust f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> mkTrie (fmap f v) prefix m
            PostFix (Right (x:xs)) ->
               mkTrie v prefix $ Map.adjust (adjust f xs) m x
            _                      -> tr


-- The given function returns the looked-up value and the updated value in a
-- pair.
--
-- TODO: since altEmpty exists I think this can be done with a function of type
-- (a -> st a)
updateLookup :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (st a -> (st a, st a))
             -> [k]
             -> trie map k a
             -> (st a, trie map k a)
updateLookup f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> let (ret, upd) = f v
                                       in (ret, mkTrie upd prefix m)
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

alter :: (Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter f k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> tryCompress (mkTrie (f v) prefix m)
            PostFix (Right (x:xs)) ->
               tryCompress . mkTrie v prefix $
                  Map.update (\t -> let new = alter f xs t
                                     in if null new then Nothing else Just new)
                             m x
            _ -> tr

-- * Combination

-- I think these are O(min(n1,n2)).

-- The *Key versions are mostly rewritten from the basic ones: they have an
-- additional O(m) cost from keeping track of the key, which is why the basic
-- ones can't just call them.

unionWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith j tr1 tr2 =
   let (v1,pre1,m1) = tParts tr1
       (v2,pre2,m2) = tParts tr2
    in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
            Same ->
               tryCompress $ mkTrie (unionVals j v1 v2) pre1 (mapUnion j m1 m2)

            PostFix remainder ->
               tryCompress $
                  either (mkTrie v2 pre2 . mapUnion j m2 . decompress m1 v1)
                         (mkTrie v1 pre1 . mapUnion j m1 . decompress m2 v2)
                         remainder

            DifferedAt pr (x:xs) (y:ys) ->
               mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                  y (mkTrie v2 ys m2)

            _ -> can'tHappen
 where
   mapUnion = Map.unionWith . unionWith

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Base.unionWith :: internal error"

unionWithKey :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
             => ([k] -> a -> a -> a)
             -> trie map k a
             -> trie map k a
             -> trie map k a
unionWithKey = go DL.empty
 where
   go k j tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               Same ->
                  let k' = DL.toList $ k `DL.append` DL.fromList pre1
                   in tryCompress $
                         mkTrie (unionVals (j k') v1 v2)
                                pre1
                                (mapUnion j k pre1 m1 m2)

               PostFix remainder ->
                  tryCompress $
                     either
                        (mk v2 pre2 . mapUnion j k pre2 m2 . decompress m1 v1)
                        (mk v1 pre1 . mapUnion j k pre1 m1 . decompress m2 v2)
                        remainder

               DifferedAt pr (x:xs) (y:ys) ->
                  mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                     y (mkTrie v2 ys m2)

               _ -> can'tHappen

   mk = mkTrie

   mapUnion j k p =
      Map.unionWithKey (\x -> go (k `DL.append` DL.fromList p `DL.snoc` x) j)

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Base.unionWithKey :: internal error"

unionsWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
           => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith j = foldl' (unionWith j) empty

unionsWithKey :: ( Alt st a, Boolable (st a)
                 , Unionable st a, Trie trie st map k
                 )
              => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey j = foldl' (unionWithKey j) empty

differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith j_ tr1 tr2 =
   let (v1,pre1,m1) = tParts tr1
       (v2,pre2,m2) = tParts tr2
    in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
            DifferedAt _ _ _   -> tr1
            Same               -> mk j_ v1 v2 pre1 m1 m2
            PostFix (Left  xs) -> goRight j_ tr1 m2  xs
            PostFix (Right xs) -> goLeft  j_ tr1 tr2 xs
 where
   mapDifference = Map.differenceWith . differenceWith'
   differenceWith' j a b =
      let c = differenceWith j a b
       in if null c then Nothing else Just c

   mk j v v' p m m' = tryCompress $ mkTrie (differenceVals j v v') p
                                           (mapDifference  j m m')

   -- See the comment in 'intersection' for a longish example of the idea
   -- behind this, which is basically that if we see two prefixes like "foo"
   -- and "foobar", we traverse the "foo" trie looking for "bar". Then if we
   -- find "barbaz", we traverse the "foobar" trie looking for "baz", and so
   -- on.
   --
   -- We have two functions for the two tries because set difference is a
   -- noncommutative operation.
   goRight j left rightMap (x:xs) =
      let (v,pre,m) = tParts left
       in case Map.lookup rightMap x of
               Nothing     -> left
               Just right' ->
                  let (v',pre',m') = tParts right'
                   in case comparePrefixes (Map.eqCmp m) xs pre' of
                           DifferedAt _ _ _   -> left
                           Same               -> mk j v v' pre m m'
                           PostFix (Left  ys) -> goRight j left m'     ys
                           PostFix (Right ys) -> goLeft  j left right' ys

   goRight _ _ _ [] = can'tHappen

   goLeft j left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.adjust f ml x
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> left'
                  Same               -> mk j v vr pre m mr
                  PostFix (Left  ys) -> goRight j left' mr    ys
                  PostFix (Right ys) -> goLeft  j left' right ys

   goLeft _ _ _ [] = can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.differenceWith :: internal error"

differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey = go DL.empty
 where
   go k j_ tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               DifferedAt _ _ _   -> tr1
               Same               -> mk j_ k v1 v2 pre1 m1 m2
               PostFix (Left  xs) -> goRight k j_ tr1 m2  xs
               PostFix (Right xs) -> goLeft  k j_ tr1 tr2 xs

   mapDifference k j =
      Map.differenceWithKey (\x -> differenceWithKey' (k `DL.snoc` x) j)

   differenceWithKey' k j a b =
      let c = go k j a b
       in if null c then Nothing else Just c

   mk j k v v' p m m' =
      let k' = k `DL.append` DL.fromList p
       in tryCompress $ mkTrie (differenceVals (j $ DL.toList k') v v')
                               p
                               (mapDifference k' j m m')

   goRight k j left rightMap (x:xs) =
      let (v,pre,m) = tParts left
       in case Map.lookup rightMap x of
               Nothing     -> left
               Just right' ->
                  let (v',pre',m') = tParts right'
                   in case comparePrefixes (Map.eqCmp m) xs pre' of
                           DifferedAt _ _ _   -> left
                           Same               -> mk j k v v' pre m m'
                           PostFix (Left  ys) -> goRight k j left m'     ys
                           PostFix (Right ys) -> goLeft  k j left right' ys

   goRight _ _ _ _ [] = can'tHappen

   goLeft k j left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.adjust f ml x
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> left'
                  Same               -> mk j k v vr pre m mr
                  PostFix (Left  ys) -> goRight k j left' mr    ys
                  PostFix (Right ys) -> goLeft  k j left' right ys

   goLeft _ _ _ _ [] = can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Base.differenceWithKey :: internal error"

intersectionWith :: forall a b c k map st trie.
                    ( Alt st c, Boolable (st c)
                    , Intersectable st a b c, Intersectable st b a c
                    , Trie trie st map k
                    )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith j_ trl trr =
   let (vl,prel,ml) = tParts trl
       (vr,prer,mr) = tParts trr
    in case comparePrefixes (Map.eqCmp ml) prel prer of
            DifferedAt _ _ _  -> empty
            Same              -> mk j_ vl vr prel ml mr
            PostFix remainder ->
               -- use the one with a longer prefix as the base for the
               -- intersection, and descend into the map of the one with a
               -- shorter prefix
               either (go       j_  mr vl ml (DL.fromList prel))
                      (go (flip j_) ml vr mr (DL.fromList prer))
                      remainder
 where
   -- Can't pointlessify due to the monomorphism restriction
   mapIntersect j = Map.intersectionWith (intersectionWith j)

   mk j v v' p m m' = tryCompress $ mkTrie (intersectionVals j v v') p
                                           (mapIntersect j m m')

   -- Polymorphic recursion in 'go' (j :: a -> b -> c  to  b -> a -> c) means
   -- that it has to be explicitly typed in order to compile.
   --
   -- I don't think there's a way of making the type system realize that
   -- whenever we call mk, j will have type (a -> b -> c). Thus we need
   -- Intersectable st b a c (and y x z) as well. Doesn't matter, though.

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
   go :: ( Alt st z, Boolable (st z)
         , Intersectable st x y z, Intersectable st y x z
         )
      => (x -> y -> z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go j ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               -> mk j v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in                go (flip j) mb v' m' nextPre ys
                       PostFix (Left  ys) -> go       j  m' v  mb pre     ys

   go _ _ _ _ _ [] =
      error "Data.Trie.Patricia.Map.intersectionWith :: internal error"

intersectionWithKey :: forall a b c k map st trie.
                       ( Alt st c, Boolable (st c)
                       , Intersectable st a b c, Intersectable st b a c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey = f DL.empty
 where
   f k j_ trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _ -> empty
               Same             -> mk k j_ vl vr prel ml mr
               PostFix remainder ->
                  either (go k       j_  mr vl ml (DL.fromList prel))
                         (go k (flip.j_) ml vr mr (DL.fromList prer))
                         remainder

   mapIntersect k j = Map.intersectionWithKey (\x -> f (k `DL.snoc` x) j)

   mk k j v v' p m m' =
      let k' = k `DL.append` DL.fromList p
       in tryCompress $ mkTrie (intersectionVals (j $ DL.toList k') v v')
                               p
                               (mapIntersect k' j m m')

   -- See intersectionWith: this explicit type is necessary
   go :: ( Alt st z, Boolable (st z)
         , Intersectable st x y z, Intersectable st y x z
         )
      => DList k
      -> ([k] -> x -> y -> z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go k j ma v mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               -> mk k j v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in                go k (flip.j) mb v' m' nextPre ys
                       PostFix (Left  ys) -> go k       j  m' v  mb pre     ys

   go _ _ _ _ _ _ [] =
      error "Data.Trie.Patricia.Map.intersectionWithKey :: internal error"

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

mapKeysWith :: (Boolable (st a), Trie trie st map k1, Trie trie st map k2)
            => ([([k2],a)] -> trie map k2 a)
            -> ([k1] -> [k2])
            -> trie map k1 a
            -> trie map k2 a
mapKeysWith fromlist f = fromlist . Prelude.map (first f) . toList

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

foldWithKey :: (Boolable (st a), Trie trie st map k)
            => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldWithKey f x = Prelude.foldr (uncurry f) x . toList

foldAscWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
               => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldAscWithKey f x = Prelude.foldr (uncurry f) x . toAscList

foldDescWithKey :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => ([k] -> a -> b -> b) -> b -> trie map k a -> b
foldDescWithKey f x = Prelude.foldr (uncurry f) x . toDescList

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

fromList :: (Alt st a, Boolable (st a), Trie trie st map k)
         => [([k],a)] -> trie map k a
fromList = fromListWith const

fromListWith :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> a -> a) -> [([k],a)] -> trie map k a
fromListWith f = foldl' (flip . uncurry $ insertWith f) empty

fromListWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
                => ([k] -> a -> a -> a) -> [([k],a)] -> trie map k a
fromListWithKey f = foldl' (\tr (k,v) -> insertWith (f k) k v tr) empty


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

addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix xs tr =
   let (v,pre,m) = tParts tr
    in mkTrie v (xs ++ pre) m

splitPrefix :: (Alt st a, Trie trie st map k)
            => trie map k a -> ([k], trie map k a)
splitPrefix tr =
   let (v,pre,m) = tParts tr
    in (pre, mkTrie v [] m)

lookupPrefix :: (Alt st a, Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
lookupPrefix = go DL.empty
 where
   go pr xs tr =
      let (_,pre,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) pre xs of
               Same                   -> addPrefix (DL.toList pr) tr
               PostFix (Left _)       -> addPrefix (DL.toList pr) tr
               DifferedAt _ _ _       -> empty
               PostFix (Right (y:ys)) ->
                  case Map.lookup m y of
                       Nothing  -> empty
                       Just tr' ->
                          go (pr `DL.append` DL.fromList pre `DL.snoc` y)
                             ys tr'

               _ -> error
                       "Data.Trie.Patricia.Base.lookupPrefix :: internal error"

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
