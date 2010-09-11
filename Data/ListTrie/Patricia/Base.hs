-- File created: 2008-12-28 17:20:14

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
           , FlexibleContexts, ScopedTypeVariables, Rank2Types
           , NoMonomorphismRestriction #-}

module Data.ListTrie.Patricia.Base
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
   , addPrefix, splitPrefix, deletePrefix, children, children1
   , showTrieWith
   , eqComparePrefixes, ordComparePrefixes
   ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow       ((***), first)
import Control.Exception   (assert)
import qualified Data.DList as DL
import Data.DList          (DList)
import Data.Foldable       (foldr, foldl')
import Data.List           (foldl1', partition)
import Data.Maybe          (fromJust, isJust)
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

-- * Construction

-- O(1)
empty :: (Alt st a, Trie trie st map k) => trie map k a
empty = mkTrie altEmpty [] Map.empty

-- O(1)
singleton :: (Alt st a, Trie trie st map k) => [k] -> a -> trie map k a
singleton k v = mkTrie (pure v) k Map.empty

-- O(min(m,s))
insert :: (Alt st a, Boolable (st a), Trie trie st map k)
       => [k] -> a -> trie map k a -> trie map k a
insert = insertWith const

-- O(min(m,s))
insert' :: (Alt st a, Boolable (st a), Trie trie st map k)
        => [k] -> a -> trie map k a -> trie map k a
insert' = insertWith' const

-- O(min(m,s))
insertWith :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith = genericInsertWith ($) (<$>)

-- O(min(m,s))
insertWith' :: (Alt st a, Boolable (st a), Trie trie st map k)
            => (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
insertWith' = (seq <*>) .: genericInsertWith ($!) (<$!>)

genericInsertWith :: (Alt st a, Boolable (st a), Trie trie st map k)
                  => (forall x y. (x -> y) -> x -> y)
                  -> ((a -> a) -> st a -> st a)
                  -> (a -> a -> a) -> [k] -> a -> trie map k a -> trie map k a
genericInsertWith ($$) (<$$>) f = go
 where
   mkTrie' = ($$) mkTrie
   go k new tr =
      let (old,prefix,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) prefix k of
               Same -> mkTrie' ((f new <$$> old) <|> pure new) prefix m

               PostFix (Left (p:pr)) ->
                  mkTrie' (pure new) k
                          (Map.singleton p (mkTrie old pr m))
               PostFix (Right (x:xs)) ->
                  -- Minor optimization: instead of tryCompress we just check
                  -- for the case of an empty trie
                  if null tr
                     then singleton k new
                     else mkTrie old prefix $
                             Map.insertWith (\_ oldt -> go xs new oldt)
                                            x (singleton xs new) m

               DifferedAt pr' (p:pr) (x:xs) ->
                  mkTrie altEmpty pr' $ Map.doubleton x (singleton xs new)
                                                      p (mkTrie old pr m)

               _ -> error
                  "Data.ListTrie.Patricia.Base.insertWith :: internal error"

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
genericAdjust ($$) myFmap f = go
 where
   go k tr =
      let (v,prefix,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) prefix k of
               Same                   -> (mkTrie $$ myFmap f v) prefix m
               PostFix (Right (x:xs)) ->
                  mkTrie v prefix $ Map.adjust (go xs) x m
               _                      -> tr

-- O(min(m,s))
updateLookup :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (a -> st a) -> [k] -> trie map k a -> (st a, trie map k a)
updateLookup f = go
 where
   go k tr =
      let (v,prefix,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) prefix k of
               Same                   -> let v' = if hasValue v
                                                     then f (unwrap v)
                                                     else v
                                          in (v, safeMkTrie v' prefix m)
               PostFix (Right (x:xs)) ->
                  case Map.lookup x m of
                       Nothing  -> (altEmpty, tr)
                       Just tr' ->
                          let (ret, upd) = go xs tr'
                           in ( ret
                              , safeMkTrie v prefix $
                                   if null upd
                                      then Map.delete x m
                                      else Map.adjust (const upd) x m
                              )
               _ -> (altEmpty, tr)

-- O(min(m,s))
--
-- This can be lazy in exactly one case: the key is a prefix of more than one
-- key in the trie. In that case, we know that the resulting trie continues to
-- contain those children.
--
-- In all other cases we have to check whether the function removed a key or
-- not, in order to be able to keep the trie in an internally valid state.

-- (I.e. we need to try to compress it.)
alter :: (Alt st a, Boolable (st a), Trie trie st map k)
      => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter = genericAlter (flip const)

-- O(min(m,s))
alter' :: (Alt st a, Boolable (st a), Trie trie st map k)
       => (st a -> st a) -> [k] -> trie map k a -> trie map k a
alter' = genericAlter seq

genericAlter :: (Alt st a, Boolable (st a), Trie trie st map k)
             => (st a -> trie map k a -> trie map k a)
             -> (st a -> st a) -> [k] -> trie map k a -> trie map k a
genericAlter seeq f = go
 where
   go k tr =
      let (v,prefix,m) = tParts tr
       in case comparePrefixes (Map.eqCmp m) prefix k of
               Same                   ->
                  let v' = f v
                   in -- We need to compress if the map was empty or a
                      -- singleton and the value was removed
                      if    (Map.null m || isJust (Map.singletonView m))
                         && not (hasValue v')
                         then tryCompress (mkTrie v' prefix m)
                         else v' `seeq` mkTrie v' prefix m

               PostFix (Right (x:xs)) ->
                  mkTrie v prefix $
                     Map.alter
                        (\mt ->
                           case mt of
                                Nothing ->
                                   let v' = f altEmpty
                                    in if hasValue v'
                                          then Just (singleton xs (unwrap v'))
                                          else Nothing
                                Just t ->
                                   let new = go xs t
                                    in if null new then Nothing else Just new)
                        x m

               PostFix (Left (p:ps)) ->
                  let v' = f altEmpty
                   in if hasValue v'
                         then mkTrie v' k $ Map.singleton p (mkTrie v ps m)
                         else tr

               DifferedAt pr (p:ps) (x:xs) ->
                  let v' = f altEmpty
                   in if hasValue v'
                         then mkTrie altEmpty pr $
                                 Map.doubleton p (mkTrie v  ps m)
                                               x (mkTrie v' xs Map.empty)
                         else tr

               _ -> error
                  "Data.ListTrie.Patricia.Base.genericAlter :: internal error"

-- * Querying

-- O(1)
--
-- Test the strict field last for maximal laziness
null :: (Boolable (st a), Trie trie st map k) => trie map k a -> Bool
null tr = let (v,p,m) = tParts tr
           in Map.null m && noValue v && assert (Prelude.null p) True

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
lookup k tr =
   let (v,prefix,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) prefix k of
            Same                   -> v
            PostFix (Right (x:xs)) -> maybe altEmpty (lookup xs)
                                            (Map.lookup x m)
            _                      -> altEmpty

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
isSubmapOfBy f = go0
 where
   go0 trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _  -> False

               -- Special case here: if the left trie is empty we return True.
               PostFix (Right _) -> null trl
               PostFix (Left xs) -> go mr vl ml xs
               Same              -> same vl vr ml mr

   go mr vl ml (x:xs) =
      case Map.lookup x mr of
           Nothing -> False
           Just tr ->
              let (vr,pre,mr') = tParts tr
               in case comparePrefixes (Map.eqCmp mr) xs pre of
                     DifferedAt _ _ _  -> False
                     PostFix (Right _) -> False
                     PostFix (Left ys) -> go mr' vl ml ys
                     Same              -> same vl vr ml mr'

   go _ _ _ [] =
      error "Data.ListTrie.Patricia.Base.isSubmapOfBy :: internal error"

   same vl vr ml mr =
      let hvl = hasValue vl
          hvr = hasValue vr
       in and [ not (hvl && not hvr)
              , (not hvl && not hvr) || f (unwrap vl) (unwrap vr)
              , Map.isSubmapOfBy go0 ml mr
              ]

-- O(min(n1 m1,n2 m2))
isProperSubmapOfBy :: (Boolable (st a), Boolable (st b), Trie trie st map k)
                   => (a -> b -> Bool)
                   -> trie map k a
                   -> trie map k b
                   -> Bool
isProperSubmapOfBy g = f False
 where
   f proper trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _  -> False

              -- Special case, as in isSubsetOf.
              --
              -- Note that properness does not affect this: if we hit this
              -- case, we already know that the right trie is nonempty.
               PostFix (Right _) -> null trl
               PostFix (Left xs) -> go proper mr vl ml xs
               Same              -> same proper vl vr ml mr

   go proper mr vl ml (x:xs) =
      case Map.lookup x mr of
           Nothing -> False
           Just tr ->
              let (vr,pre,mr') = tParts tr
               in case comparePrefixes (Map.eqCmp mr) xs pre of
                       DifferedAt _ _ _  -> False
                       PostFix (Right _) -> False
                       PostFix (Left ys) -> go proper mr' vl ml ys
                       Same              -> same proper vl vr ml mr'

   go _ _ _ _ [] =
      error "Data.ListTrie.Patricia.Base.isProperSubmapOfBy :: internal error"

   same proper vl vr ml mr =
      let hvl = hasValue vl
          hvr = hasValue vr

          -- As the non-Patricia version, so does this seem suboptimal.
          proper' = or [ proper
                       , not hvl && hvr
                       , not (Map.null $ Map.difference mr ml)
                       ]

       in and [ not (hvl && not hvr)
              , (not hvl && not hvr) || g (unwrap vl) (unwrap vr)
              , if Map.null ml
                   then proper'
                   else Map.isSubmapOfBy (f proper') ml mr
              ]

-- * Combination

-- The *Key versions are mostly rewritten from the basic ones: they have an
-- additional O(m) cost from keeping track of the key, which is why the basic
-- ones can't just call them.

-- O(min(n1 m1,n2 m2))
unionWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith f = genericUnionWith (flip const) (unionVals f)

-- O(min(n1 m1,n2 m2))
unionWith' :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
          => (a -> a -> a) -> trie map k a -> trie map k a -> trie map k a
unionWith' f = genericUnionWith seq (unionVals' f)

genericUnionWith :: (Alt st a, Boolable (st a), Trie trie st map k)
                 => (st a -> trie map k a -> trie map k a)
                 -> (st a -> st a -> st a)
                 -> trie map k a
                 -> trie map k a
                 -> trie map k a
genericUnionWith seeq = go
 where
   go valUnion tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               Same ->
                  let v = valUnion v1 v2

                      -- safeMkTrie not needed: if pre1 is not null then m1 or
                      -- v won't be and hence the union won't be.
                   in v `seeq` (tryCompress.mkTrie v pre1 $
                                               mapUnion valUnion m1 m2)

               PostFix remainder ->
                  -- As above, mkTrie is fine
                  --
                  -- The flip is important to retain left-biasedness
                  tryCompress $
                     either
                        (mkTrie v2 pre2 . mapUnion (flip valUnion) m2 .
                           decompress m1 v1)
                        (mkTrie v1 pre1 . mapUnion       valUnion  m1 .
                           decompress m2 v2)
                        remainder

               DifferedAt pr (x:xs) (y:ys) ->
                  -- As above, mkTrie is fine
                  mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                     y (mkTrie v2 ys m2)

               _ -> can'tHappen

   mapUnion = Map.unionWith . go

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen =
      error "Data.ListTrie.Patricia.Base.unionWith :: internal error"

-- O(min(n1 m1,n2 m2))
unionWithKey :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
             => ([k] -> a -> a -> a)
             -> trie map k a
             -> trie map k a
             -> trie map k a
unionWithKey = genericUnionWithKey (flip const) unionVals

-- O(min(n1 m1,n2 m2))
unionWithKey' :: ( Alt st a, Boolable (st a), Unionable st a
                 , Trie trie st map k
                 )
              => ([k] -> a -> a -> a)
              -> trie map k a
              -> trie map k a
              -> trie map k a
unionWithKey' = genericUnionWithKey seq unionVals'

genericUnionWithKey :: (Alt st a, Boolable (st a), Trie trie st map k)
                    => (st a -> trie map k a -> trie map k a)
                    -> ((a -> a -> a) -> st a -> st a -> st a)
                    -> ([k] -> a -> a -> a)
                    -> trie map k a
                    -> trie map k a
                    -> trie map k a
genericUnionWithKey seeq = go DL.empty
 where
   go k valUnion j tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               Same ->
                  let k' = DL.toList $ k `DL.append` DL.fromList pre1
                      v  = valUnion (j k') v1 v2
                   in v `seeq`
                         (tryCompress.mkTrie v pre1 $
                            mapUnion valUnion j k pre1 m1 m2)

               PostFix remainder ->
                  tryCompress $
                     either
                        (mk v2 pre2 . mapUnion (flip.valUnion) j k pre2 m2
                           . decompress m1 v1)
                        (mk v1 pre1 . mapUnion       valUnion  j k pre1 m1
                           . decompress m2 v2)
                        remainder

               DifferedAt pr (x:xs) (y:ys) ->
                  mkTrie altEmpty pr $ Map.doubleton x (mkTrie v1 xs m1)
                                                     y (mkTrie v2 ys m2)

               _ -> can'tHappen

   mk = mkTrie

   mapUnion v j k p =
      Map.unionWithKey $
         \x -> go (k `DL.append` DL.fromList p `DL.snoc` x) v j

   decompress m v (x:xs) = Map.singleton x (mkTrie v xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen =
      error "Data.ListTrie.Patricia.Base.unionWithKey :: internal error"

-- O(sum(n))
unionsWith :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
           => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith j = foldl' (unionWith j) empty

-- O(sum(n))
unionsWith' :: (Alt st a, Boolable (st a), Unionable st a, Trie trie st map k)
            => (a -> a -> a) -> [trie map k a] -> trie map k a
unionsWith' j = foldl' (unionWith' j) empty

-- O(sum(n))
unionsWithKey :: ( Alt st a, Boolable (st a)
                 , Unionable st a, Trie trie st map k
                 )
              => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey j = foldl' (unionWithKey j) empty

-- O(sum(n))
unionsWithKey' :: ( Alt st a, Boolable (st a)
                  , Unionable st a, Trie trie st map k
                  )
               => ([k] -> a -> a -> a) -> [trie map k a] -> trie map k a
unionsWithKey' j = foldl' (unionWithKey' j) empty

-- O(min(n1 m1,n2 m2))
differenceWith :: (Boolable (st a), Differentiable st a b, Trie trie st map k)
               => (a -> b -> Maybe a)
               -> trie map k a
               -> trie map k b
               -> trie map k a
differenceWith j = go
 where
   go tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               DifferedAt _ _ _   -> tr1
               Same               -> mk v1 v2 pre1 m1 m2
               PostFix (Left  xs) -> goRight tr1 m2  xs
               PostFix (Right xs) -> goLeft  tr1 tr2 xs

   dw a b =
      let c = differenceWith j a b
       in if null c then Nothing else Just c

   mk v v' p m m' =
      let vd = differenceVals j v v'
       in tryCompress.mkTrie vd p $ Map.differenceWith dw m m'

   -- See the comment in 'intersection' for a longish example of the idea
   -- behind this, which is basically that if we see two prefixes like "foo"
   -- and "foobar", we traverse the "foo" trie looking for "bar". Then if we
   -- find "barbaz", we traverse the "foobar" trie looking for "baz", and so
   -- on.
   --
   -- We have two functions for the two tries because set difference is a
   -- noncommutative operation.
   goRight left rightMap (x:xs) =
      let (v,pre,m) = tParts left
       in case Map.lookup x rightMap of
               Nothing     -> left
               Just right' ->
                  let (v',pre',m') = tParts right'
                   in case comparePrefixes (Map.eqCmp m) xs pre' of
                           DifferedAt _ _ _   -> left
                           Same               -> mk v v' pre m m'
                           PostFix (Left  ys) -> goRight left m'     ys
                           PostFix (Right ys) -> goLeft  left right' ys

   goRight _ _ [] = can'tHappen

   goLeft left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.update f x ml
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> Just left'
                  Same               -> tryNull $ mk v vr pre m mr
                  PostFix (Left  ys) -> tryNull $ goRight left' mr    ys
                  PostFix (Right ys) -> tryNull $ goLeft  left' right ys

      tryNull t = if null t then Nothing else Just t

   goLeft _ _ [] = can'tHappen

   can'tHappen =
      error "Data.ListTrie.Patricia.Base.differenceWith :: internal error"

-- O(min(n1 m1,n2 m2))
differenceWithKey :: ( Boolable (st a), Differentiable st a b
                     , Trie trie st map k
                     )
                  => ([k] -> a -> b -> Maybe a)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k a
differenceWithKey j = go DL.empty
 where
   go k tr1 tr2 =
      let (v1,pre1,m1) = tParts tr1
          (v2,pre2,m2) = tParts tr2
       in case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
               DifferedAt _ _ _   -> tr1
               Same               -> mk k v1 v2 pre1 m1 m2
               PostFix (Left  xs) -> goRight (key k pre2) tr1 m2  xs
               PostFix (Right xs) -> goLeft  (key k pre1) tr1 tr2 xs

   key k p = k `DL.append` DL.fromList p

   dw k a b =
      let c = go k a b
       in if null c then Nothing else Just c

   mk k v v' p m m' =
      let k' = k `DL.append` DL.fromList p
          vd = differenceVals (j $ DL.toList k') v v'
       in tryCompress.mkTrie vd p $
             Map.differenceWithKey (dw . (k' `DL.snoc`)) m m'

   goRight k left rightMap (x:xs) =
      let (vl,_,ml) = tParts left
       in case Map.lookup x rightMap of
               Nothing    -> left
               Just right ->
                  let (vr,pre,mr) = tParts right
                      k'          = k `DL.snoc` x
                   in case comparePrefixes (Map.eqCmp ml) xs pre of
                           DifferedAt _ _ _   -> left
                           Same               -> mk k' vl vr pre ml mr
                           PostFix (Left  ys) -> goRight (key k' pre)
                                                         left mr    ys
                           PostFix (Right ys) -> goLeft  (key k' xs)
                                                         left right ys

   goRight _ _ _ [] = can'tHappen

   goLeft k left right (x:xs) =
      tryCompress . mkTrie vl prel $ Map.update f x ml
    where
      (vl,prel,ml) = tParts left
      (vr,   _,mr) = tParts right

      k' = k `DL.snoc` x

      f left' =
         let (v,pre,m) = tParts left'
          in case comparePrefixes (Map.eqCmp m) pre xs of
                  DifferedAt _ _ _   -> Just left'
                  Same               -> tryNull $ mk k' v vr pre m mr
                  PostFix (Left  ys) -> tryNull $ goRight (key k' xs)
                                                          left' mr    ys
                  PostFix (Right ys) -> tryNull $ goLeft  (key k' pre)
                                                          left' right ys

      tryNull t = if null t then Nothing else Just t

   goLeft _ _ _ [] = can'tHappen

   can'tHappen =
      error "Data.ListTrie.Patricia.Base.differenceWithKey :: internal error"

-- O(min(n1 m1,n2 m2))
intersectionWith :: ( Alt st c, Boolable (st c)
                    , Intersectable st a b c, Intersectable st b a c
                    , Trie trie st map k
                    )
                 => (a -> b -> c)
                 -> trie map k a
                 -> trie map k b
                 -> trie map k c
intersectionWith f = genericIntersectionWith (flip const) (intersectionVals f)

-- O(min(n1 m1,n2 m2))
intersectionWith' :: ( Alt st c, Boolable (st c)
                     , Intersectable st a b c, Intersectable st b a c
                     , Trie trie st map k
                     )
                  => (a -> b -> c)
                  -> trie map k a
                  -> trie map k b
                  -> trie map k c
intersectionWith' f = genericIntersectionWith seq (intersectionVals' f)

genericIntersectionWith :: forall a b c k map st trie.
                           ( Alt st c, Boolable (st c)
                           , Trie trie st map k
                           )
                        => (forall x. st x -> trie map k x -> trie map k x)
                        -> (st a -> st b -> st c)
                        -> trie map k a
                        -> trie map k b
                        -> trie map k c
genericIntersectionWith seeq = go0
 where
   go0 valIsect trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _  -> empty
               Same              -> mk valIsect vl vr prel ml mr
               PostFix remainder ->
                  -- use the one with a longer prefix as the base for the
                  -- intersection, and descend into the map of the one with a
                  -- shorter prefix
                  either (go       valIsect  mr vl ml (DL.fromList prel))
                         (go (flip valIsect) ml vr mr (DL.fromList prer))
                         remainder

   mapIntersect valIsect =
      Map.filter (not.null) .:
         Map.intersectionWith (go0 valIsect)

   mk valIsect v v' p m m' =
      let vi = valIsect v v'
       in vi `seeq` (tryCompress.mkTrie vi p $ mapIntersect valIsect m m')

   -- Polymorphic recursion in 'go' (valIsect :: st a -> st b -> st c ---> st b
   -- -> st a -> st c) means that it has to be explicitly typed in order to
   -- compile.
   --
   -- The repeated "Trie trie st map k" constraint is for Hugs.

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
   go :: (Alt st z, Boolable (st z), Trie trie st map k)
      => (st x -> st y -> st z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go valIsect ma v mb pre (x:xs) =
      case Map.lookup x ma of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               ->
                          mk valIsect v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in go (flip valIsect) mb v' m' nextPre ys
                       PostFix (Left  ys) ->
                              go       valIsect  m' v  mb pre     ys

   go _ _ _ _ _ [] =
      error "Data.ListTrie.Patricia.Map.intersectionWith :: internal error"

-- O(min(n1 m1,n2 m2))
intersectionWithKey :: ( Alt st c, Boolable (st c)
                       , Intersectable st a b c, Intersectable st b a c
                       , Trie trie st map k
                       )
                    => ([k] -> a -> b -> c)
                    -> trie map k a
                    -> trie map k b
                    -> trie map k c
intersectionWithKey = genericIntersectionWithKey (flip const) intersectionVals

-- O(min(n1 m1,n2 m2))
intersectionWithKey' :: ( Alt st c, Boolable (st c)
                        , Intersectable st a b c, Intersectable st b a c
                        , Trie trie st map k
                        )
                     => ([k] -> a -> b -> c)
                     -> trie map k a
                     -> trie map k b
                     -> trie map k c
intersectionWithKey' = genericIntersectionWithKey seq intersectionVals'

genericIntersectionWithKey :: forall a b c k map st trie.
                              (Alt st c, Boolable (st c), Trie trie st map k)
                           => (forall x. st x -> trie map k x -> trie map k x)
                           -> ((a -> b -> c) -> st a -> st b -> st c)
                           -> ([k] -> a -> b -> c)
                           -> trie map k a
                           -> trie map k b
                           -> trie map k c
genericIntersectionWithKey seeq = main DL.empty
 where
   main k valIsect j trl trr =
      let (vl,prel,ml) = tParts trl
          (vr,prer,mr) = tParts trr
       in case comparePrefixes (Map.eqCmp ml) prel prer of
               DifferedAt _ _ _ -> empty
               Same             -> mk k valIsect j vl vr prel ml mr
               PostFix remainder ->
                  let prel' = DL.fromList prel
                      prer' = DL.fromList prer
                   in either
                         (go k       valIsect        j  mr vl ml prel')
                         (go k (flop valIsect) (flip.j) ml vr mr prer')
                         remainder

   mk k valIsect j v v' p m m' =
      let k' = k `DL.append` DL.fromList p
          vi = valIsect (j $ DL.toList k') v v'
       in vi `seeq` (tryCompress.mkTrie vi p $
                                    mapIntersect k' valIsect j m m')

   mapIntersect k valIsect j =
      Map.filter (not.null) .:
         Map.intersectionWithKey (\x -> main (k `DL.snoc` x) valIsect j)

   flop :: ((x -> y -> z) -> st x -> st y -> st z)
         -> ((y -> x -> z) -> st y -> st x -> st z)
   flop f = flip . f . flip

   -- See intersectionWith: this explicit type is necessary
   go :: (Alt st z, Boolable (st z), Trie trie st map k)
      => DList k
      -> ((x -> y -> z) -> st x -> st y -> st z)
      -> ([k] -> x -> y -> z)
      -> CMap trie map k y
      -> st x
      -> CMap trie map k x
      -> DList k
      -> [k]
      -> trie map k z
   go k valIsect j ma v mb pre (x:xs) =
      case Map.lookup x ma of
           Nothing -> empty
           Just tr ->
              let (v',pre',m') = tParts tr
               in case comparePrefixes (Map.eqCmp ma) xs pre' of
                       DifferedAt _ _ _   -> empty
                       Same               ->
                          mk k valIsect j v v' (DL.toList pre) mb m'
                       PostFix (Right ys) ->
                          let nextPre = pre `DL.append` DL.fromList ys
                           in go k (flop valIsect) (flip.j) mb v' m' nextPre ys
                       PostFix (Left  ys) ->
                              go k        valIsect       j  m' v  mb pre     ys

   go _ _ _ _ _ _ _ [] =
      error "Data.ListTrie.Patricia.Map.intersectionWithKey :: internal error"

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
mapInKeysWith :: ( Alt st a, Boolable (st a), Unionable st a
                 , Trie trie st map k1, Trie trie st map k2
                 )
              => (a -> a -> a)
              -> (k1 -> k2)
              -> trie map k1 a
              -> trie map k2 a
mapInKeysWith = genericMapInKeysWith (flip const) (const ()) unionWith

-- O(n m)
mapInKeysWith' :: ( Alt st a, Boolable (st a), Unionable st a
                  , Trie trie st map k1, Trie trie st map k2
                  )
               => (a -> a -> a)
               -> (k1 -> k2)
               -> trie map k1 a
               -> trie map k2 a
mapInKeysWith' =
   genericMapInKeysWith
      seq
      (\xs -> if Prelude.null xs then () else foldl1' seq xs `seq` ())
      unionWith'

genericMapInKeysWith :: ( Alt st a, Boolable (st a), Unionable st a
                        , Trie trie st map k1, Trie trie st map k2
                        )
                     => (() -> trie map k2 a -> trie map k2 a)
                     -> ([k2] -> ())
                     -> (f -> trie map k2 a -> trie map k2 a -> trie map k2 a)
                     -> f
                     -> (k1 -> k2)
                     -> trie map k1 a
                     -> trie map k2 a
genericMapInKeysWith seeq listSeq unionW j f = go
 where
   go tr =
      let (v,p,m) = tParts tr
          p'      = map f p
       in listSeq p' `seeq`
             (mkTrie v p' $
                 Map.fromListWith (unionW j) . map (f *** go) . Map.toList $ m)

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
toList = genericToList Map.toList DL.cons

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
   go l tr =
      let (v,p,m) = tParts tr
          l'      = l `DL.append` DL.fromList p
          xs      =
             DL.concat .
             map (\(x,t) -> go (l' `DL.snoc` x) t) .
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
minView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> (Maybe ([k], a), trie map k a)
minView = minMaxView (hasValue.tVal) (fst . Map.minViewWithKey)

-- O(m)
maxView :: (Alt st a, Boolable (st a), Trie trie st map k, OrdMap map k)
        => trie map k a -> (Maybe ([k], a), trie map k a)
maxView = minMaxView (Map.null.tMap) (fst . Map.maxViewWithKey)

minMaxView :: (Alt st a, Boolable (st a), Trie trie st map k)
           => (trie map k a -> Bool)
           -> (CMap trie map k a -> Maybe (k, trie map k a))
           -> trie map k a
           -> (Maybe ([k], a), trie map k a)
minMaxView _        _       tr_ | null tr_ = (Nothing, tr_)
minMaxView isWanted mapView tr_ = first Just (go tr_)
 where
   go tr =
      let (v,pre,m) = tParts tr
       in if isWanted tr
             then ((pre, unwrap v), safeMkTrie altEmpty pre m)

             else let (k,      tr')  = fromJust (mapView m)
                      (minMax, tr'') = go tr'
                   in ( first (prepend pre k) minMax
                      , mkTrie v pre $ if null tr''
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
      let (v,pre,m) = tParts tr
          xs'       = xs `DL.append` DL.fromList pre
       in if isWanted tr
             then (DL.toList xs', unwrap v)
             else let (k, tr') = fromJust . mapView $ m
                   in go (xs' `DL.snoc` k) tr'

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
               let (ml, maybeTr, mg) = Map.splitLookup y m
                in case maybeTr of
                        -- Prefix goes in left side of split since it's shorter
                        -- than the given key and thus lesser
                        Nothing  -> (mk v pre ml, altEmpty, mk altEmpty pre mg)
                        Just tr' ->
                           let (tl, v', tg) = splitLookup ys tr'
                               ml' = if null tl then ml else Map.insert y tl ml
                               mg' = if null tg then mg else Map.insert y tg mg
                            in (mk v pre ml', v', mk altEmpty pre mg')
            _ -> can'tHappen
 where
   mk v pre = tryCompress . mkTrie v pre
   can'tHappen =
      error "Data.ListTrie.Patricia.Base.splitLookup :: internal error"

-- O(m)
findPredecessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
                => [k] -> trie map k a -> Maybe ([k], a)
findPredecessor _   tr | null tr = Nothing
findPredecessor xs_ tr_          = go xs_ tr_
 where
   go xs tr =
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
                  let predecessor = Map.findPredecessor y m
                   in (first (prepend pre y)<$>(Map.lookup y m >>= go ys))
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
      error "Data.ListTrie.Patricia.Base.findPredecessor :: internal error"

-- O(m)
findSuccessor :: (Boolable (st a), Trie trie st map k, OrdMap map k)
              => [k] -> trie map k a -> Maybe ([k], a)
findSuccessor _   tr | null tr = Nothing
findSuccessor xs_ tr_          = go xs_ tr_
 where
   go xs tr =
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
                  let successor = Map.findSuccessor y m
                   in (first (prepend pre y)<$>(Map.lookup y m >>= go ys))
                      <|>
                      (successor >>= \(best,btr) ->
                         first (prepend pre best) <$> findMin btr)

               _ -> can'tHappen

   can'tHappen =
      error "Data.ListTrie.Patricia.Base.findSuccessor :: internal error"

-- * Trie-only operations

-- O(s)
addPrefix :: (Alt st a, Trie trie st map k)
          => [k] -> trie map k a -> trie map k a
addPrefix xs tr =
   let (v,pre,m) = tParts tr
    in mkTrie v (xs ++ pre) m

-- O(m)
deletePrefix :: (Alt st a, Boolable (st a), Trie trie st map k)
             => [k] -> trie map k a -> trie map k a
deletePrefix xs tr =
   let (v,pre,m) = tParts tr
    in case comparePrefixes (Map.eqCmp m) pre xs of
            Same                   -> tryCompress (mkTrie v [] m)
            PostFix (Left ys)      -> mkTrie v ys m
            DifferedAt _ _ _       -> empty
            PostFix (Right (y:ys)) ->
               case Map.lookup y m of
                    Nothing  -> empty
                    Just tr' -> deletePrefix ys tr'

            _ ->
               error
                  "Data.ListTrie.Patricia.Base.deletePrefix :: internal error"

-- O(1)
splitPrefix :: (Alt st a, Boolable (st a), Trie trie st map k)
            => trie map k a -> ([k], st a, trie map k a)
splitPrefix tr =
   let (v,pre,m) = tParts tr
    in (pre, v, tryCompress $ mkTrie altEmpty [] m)

-- O(1)
children :: Trie trie st map k => trie map k a -> CMap trie map k a
children = tMap

-- O(1)
children1 :: Trie trie st map k => trie map k a -> CMap trie map k a
children1 tr =
   let (v,pre,m) = tParts tr
    in case pre of
            []   -> m
            p:ps -> Map.singleton p (mkTrie v ps m)

-- * Visualization

-- O(n m)
showTrieWith :: (Show k, Trie trie st map k)
             => (st a -> ShowS) -> trie map k a -> ShowS
showTrieWith = go 0
 where
   go indent f tr =
      let (v,pre,m) = tParts tr
          spre      = shows pre
          lpre      = length (spre [])
          sv        = f v
          lv        = length (sv [])
       in spre . showChar ' '
        . sv . showChar ' '
        . (foldr (.) id . zipWith (flip ($)) (False : repeat True) $
              map (\(k,t) -> \b -> let sk = shows k
                                       lk = length (sk [])
                                       i  = indent + lpre + lv + 2
                                    in (if b
                                           then showChar '\n'
                                              . showString (replicate i ' ')
                                           else id)
                                     . showString "-> "
                                     . sk . showChar ' '
                                     . go (i + lk + 4) f t)
                  (Map.toList m))

-- helpers

-- mkTrie, but makes sure that empty tries don't have nonempty prefixes
-- intentionally strict in the value: gives update its semantics
safeMkTrie :: (Alt st a, Boolable (st a), Trie trie st map k)
           => st a -> [k] -> CMap trie map k a -> trie map k a
safeMkTrie v p m =
   if noValue v && Map.null m
      then empty
      else mkTrie v p m

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

-- Exported for Eq/Ord instances
eqComparePrefixes :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqComparePrefixes eq xs ys = case comparePrefixes eq xs ys of
                                  Same -> True
                                  _    -> False

ordComparePrefixes :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
ordComparePrefixes ord xs ys =
   case comparePrefixes (\x y -> ord x y == EQ) xs ys of
        Same                     -> EQ
        PostFix r                -> either (const GT) (const LT) r
        DifferedAt _ (x:_) (y:_) -> ord x y
        _                        -> error
           "Data.ListTrie.Patricia.Base.ordComparePrefixes :: internal error"

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
