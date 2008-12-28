-- File created: 2008-11-08 19:22:07

-- The base implementation of a Patricia trie representing a set of lists,
-- generalized over any type of map from element values to tries.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

module Data.Trie.Patricia.Set where

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Monad (join)
import qualified Data.DList as DL
import Data.DList (DList)
import qualified Data.List as List
import Data.List (foldl')
import Data.Maybe (fromJust)
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

import qualified Data.Trie.Base.Map as Map
import Data.Trie.Base.Map (Map, OrdMap)

-- Invariant: any (Tr False _ _) has at least two children, all of which are
-- True or have a True descendant.
data TrieSet map a = Tr !Bool ![a] !(CMap map a)

type CMap map a = map a (TrieSet map a)

-- instances: Eq, Monoid, Foldable, Ord, Show, Read

-- * Querying

-- O(1)
null :: Map map a => TrieSet map a -> Bool
null (Tr False p m) | Map.null m = assert (Prelude.null p) True
null _                           = False

-- O(n). The number of elements in the set.
size :: Map map a => TrieSet map a -> Int
size (Tr b _ m) = Map.foldValues ((+) . size) (fromEnum b) m

-- O(m).
member :: Map map a => [a] -> TrieSet map a -> Bool
member k (Tr b prefix m) =
   case comparePrefixes (Map.eqCmp m) prefix k of
        Same                   -> b
        PostFix (Right (x:xs)) ->
           case Map.lookup m x of
                Nothing -> False
                Just t  -> member xs t

        _ -> False

-- O(?)
isSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isSubsetOf tr1@(Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
        DifferedAt _ _ _  -> False

        -- Special case here: if the left trie is empty we return True.
        PostFix (Right _) -> null tr1

        PostFix (Left xs) -> go m2 b1 m1 xs
        Same              -> not (b1 && not b2)
                          && Map.isSubmapOfBy isSubsetOf m1 m2
 where
   go mr bl ml (x:xs) =
      case Map.lookup mr x of
           Nothing              -> False
           Just (Tr br pre mr') ->
              case comparePrefixes (Map.eqCmp mr) xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go mr' bl ml ys
                   Same              -> not (bl && not br)
                                     && Map.isSubmapOfBy isSubsetOf ml mr'

   go _ _ _ _ =
      error "Data.Trie.Patricia.Set.isSubsetOf :: internal error"

-- O(?)
isProperSubsetOf :: Map map a => TrieSet map a -> TrieSet map a -> Bool
isProperSubsetOf = f False
 where
   f proper (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
      case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
           DifferedAt _ _ _  -> False

           -- Special case, as in isSubsetOf.
           --
           -- Note that properness does not affect this: if we hit this case,
           -- we already know that the right trie is nonempty.
           PostFix (Right _) -> not b1 && Map.null m1

           PostFix (Left xs) -> go proper m2 b1 m1 xs
           Same              -> same proper b1 b2 m1 m2

   go proper mr bl ml (x:xs) =
      case Map.lookup mr x of
           Nothing              -> False
           Just (Tr br pre mr') ->
              case comparePrefixes (Map.eqCmp mr) xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go proper mr' bl ml ys
                   Same              -> same proper bl br ml mr'

   go _ _ _ _ _ =
      error "Data.Trie.Patricia.Set.isProperSubsetOf :: internal error"

   same _      True False _  _  = False
   same proper bl   br    ml mr =
      -- As the non-Patricia version, so does this seem suboptimal
      let proper' = or [ proper
                       , not bl && br
                       , not (Map.null $ Map.difference mr ml)
                       ]
       in if Map.null ml
             then proper'
             else Map.isSubmapOfBy (f proper') ml mr

-- * Construction

-- O(1)
empty :: Map map a => TrieSet map a
empty = Tr False [] Map.empty

-- O(1)
singleton :: Map map a => [a] -> TrieSet map a
singleton s = Tr True s Map.empty

-- O(m)
insert :: Map map a => [a] -> TrieSet map a -> TrieSet map a
insert k tr@(Tr b prefix m) =
   case comparePrefixes (Map.eqCmp m) prefix k of
        Same                   -> Tr True prefix m
        PostFix (Left  (p:pr)) -> Tr True k $ Map.singleton p (Tr b pr m)
        PostFix (Right (x:xs)) ->
           -- Minor optimization: instead of tryCompress we just check for the
           -- case of an empty trie
           if null tr
              then Tr True k m
              else Tr b prefix $ Map.insertWith (\_ old -> insert xs old)
                                                m x (singleton xs)

        DifferedAt pr' (p:pr) (x:xs) ->
           Tr False pr' $ Map.doubleton x (singleton xs) p (Tr b pr m)

        _ -> error "Data.Trie.Patricia.Set.insert :: internal error"

-- O(m)
delete :: Map map a => [a] -> TrieSet map a -> TrieSet map a
delete k tr@(Tr b prefix m) =
   case comparePrefixes (Map.eqCmp m) prefix k of
        Same                   -> tryCompress (Tr False prefix m)
        PostFix (Right (x:xs)) ->
           tryCompress . Tr b prefix $
              Map.update (\old -> let new = delete xs old
                                   in if null new
                                         then Nothing
                                         else Just new)
                         m x
        _ -> tr

-- * Combination

-- I think all the properly-written ones here are O(min(n1,n2)).

union :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
union (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
        Same              -> tryCompress $ Tr (b1 || b2) pre1 (mapUnion m1 m2)
        PostFix remainder ->
           tryCompress $ either (Tr b2 pre2 . mapUnion m2 . decompress m1 b1)
                                (Tr b1 pre1 . mapUnion m1 . decompress m2 b2)
                                remainder

        DifferedAt pr (x:xs) (y:ys) ->
           Tr False pr $ Map.doubleton x (Tr b1 xs m1) y (Tr b2 ys m2)

        _ -> can'tHappen
 where
   mapUnion = Map.unionWith union

   decompress m b (x:xs) = Map.singleton x (Tr b xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Set.union :: internal error"

unions :: Map map a => [TrieSet map a] -> TrieSet map a
unions = foldl' union empty

difference :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
difference tr1@(Tr b1 pre1 m1) tr2@(Tr b2 pre2 m2) =
   case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
        DifferedAt _ _ _   -> tr1
        Same               -> tr b1 b2 pre1 (mapDifference m1 m2)
        PostFix (Left  xs) -> goRight tr1 m2  xs
        PostFix (Right xs) -> goLeft  tr1 tr2 xs

 where
   mapDifference = Map.differenceWith difference'
   difference' a b =
      let c = difference a b
       in if null c then Nothing else Just c

   tr b b' p m = tryCompress $ Tr (b && not b') p m

   -- See the comment in 'intersection' for a longish example of the idea
   -- behind this, which is basically that if we see two prefixes like "foo"
   -- and "foobar", we traverse the "foo" trie looking for "bar". Then if we
   -- find "barbaz", we traverse the "foobar" trie looking for "baz", and so
   -- on.
   --
   -- We have two functions for the two tries because set difference is a
   -- noncommutative operation.
   goRight left@(Tr b pre m) rightMap (x:xs) =
      case Map.lookup rightMap x of
           Nothing                     -> left
           Just right'@(Tr b' pre' m') ->
              case comparePrefixes (Map.eqCmp m') xs pre' of
                   DifferedAt _ _ _   -> left
                   Same               -> tr b b' pre (mapDifference m m')
                   PostFix (Left  ys) -> goRight left m'     ys
                   PostFix (Right ys) -> goLeft  left right' ys

   goRight _ _ _ = error "Data.Trie.Patricia.Set.difference :: internal error"

   goLeft (Tr bl prel ml) right@(Tr br _ mr) (x:xs) =
      tryCompress . Tr bl prel $ Map.adjust f ml x
    where
      f left@(Tr b pre m) =
         case comparePrefixes (Map.eqCmp m) pre xs of
              DifferedAt _ _ _   -> left
              Same               -> tr b br pre (mapDifference m mr)
              PostFix (Left  ys) -> goRight left mr    ys
              PostFix (Right ys) -> goLeft  left right ys

   goLeft _ _ _ = error "Data.Trie.Patricia.Set.difference :: internal error"

intersection :: Map map a => TrieSet map a -> TrieSet map a -> TrieSet map a
intersection (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes (Map.eqCmp m1) pre1 pre2 of
        DifferedAt _ _ _ -> empty
        Same             -> tr b1 b2 pre1 (mapIntersect m1 m2)

        -- use the one with a longer prefix as the base for the intersection,
        -- and descend into the map of the one with a shorter prefix
        PostFix remainder ->
           either (go m2 b1 m1 pre1) (go m1 b2 m2 pre2) remainder

 where
   mapIntersect = Map.intersectionWith intersection
   tr b b' p m = tryCompress $ Tr (b && b') p m

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
   -- 1. Say we've got the following two tries:
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
   go ma b mb pre (x:xs) =
      case Map.lookup ma x of
           Nothing              -> empty
           Just (Tr b' pre' m') ->
              case comparePrefixes (Map.eqCmp ma) xs pre' of
                   DifferedAt _ _ _   -> empty
                   Same               -> tr b b' pre (mapIntersect mb m')
                   PostFix (Right ys) -> go mb b' m' (pre ++ ys) ys
                   PostFix (Left  ys) -> go m' b mb pre ys

   go _ _ _ _ _ = error "Data.Trie.Patricia.Set.intersect :: internal error"

-- * Filtering

-- O(n)
filter :: Map map a => ([a] -> Bool) -> TrieSet map a -> TrieSet map a
filter p = fromList . Prelude.filter p . toList

-- O(n)
partition :: Map map a
          => ([a] -> Bool) -> TrieSet map a -> (TrieSet map a, TrieSet map a)
partition p = join (***) fromList . List.partition p . toList

split :: OrdMap map a => [a] -> TrieSet map a -> (TrieSet map a, TrieSet map a)
split x tr = let (l,_,g) = splitMember x tr in (l,g)

splitMember :: OrdMap map a
            => [a] -> TrieSet map a -> (TrieSet map a, Bool, TrieSet map a)
splitMember xs orig@(Tr b pre m) =
   case comparePrefixes (Map.eqCmp m) pre xs of
        Same                     -> (empty, b, tr False pre m)
        DifferedAt _ (p:_) (x:_) ->
           case Map.ordCmp m p x of
                LT -> (orig, False, empty)
                GT -> (empty, False, orig)
                EQ -> can'tHappen

        PostFix (Left       _) -> (empty, False, orig)
        PostFix (Right (y:ys)) ->
           let (ml, maybeTr, mg) = Map.splitLookup m y
            in case maybeTr of
                    Nothing  -> (tr b pre ml, False, tr False pre mg)
                    Just tr' ->
                       let (tl, b', tg) = splitMember ys tr'
                           ml' = if null tl
                                    then ml
                                    else Map.insert ml y tl
                           mg' = if null tg
                                    then mg
                                    else Map.insert mg y tg
                        in (tr b pre ml', b', tr False pre mg')

        _ -> can'tHappen
 where
   tr p q r = tryCompress (Tr p q r)
   can'tHappen = error "Data.Trie.Patricia.Set.splitMember :: internal error"

-- * Mapping

-- O(n)
map :: (Map map a, Map map b) => ([a] -> [b]) -> TrieSet map a -> TrieSet map b
map f = fromList . Prelude.map f . toList

-- O(n)
-- needs a name!
map' :: (Map map a, Map map b) => (a -> b) -> TrieSet map a -> TrieSet map b
map' f (Tr b p m) =
   Tr b (Prelude.map f p) $
      Map.fromListWith union .
         Prelude.map (f *** map' f) .
      Map.toList $ m

-- * Folding

-- O(n)
fold :: Map map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
fold f x = Prelude.foldr f x . toList

-- O(n)
foldAsc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldAsc f x = Prelude.foldr f x . toAscList

-- O(n)
foldDesc :: OrdMap map a => ([a] -> b -> b) -> b -> TrieSet map a -> b
foldDesc f x = Prelude.foldr f x . toDescList

-- * Conversion between lists

-- O(n)
toList :: Map map a => TrieSet map a -> [[a]]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: OrdMap map a => TrieSet map a -> [[a]]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: OrdMap map a => TrieSet map a -> [[a]]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Map map a => (CMap map a -> [(a, TrieSet map a)])
                           -> ([a] -> DList [a] -> DList [a])
                           -> TrieSet map a
                           -> [[a]]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go l f g (Tr b p m) =
      let
         l' = l `DL.append` DL.fromList p
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l' `DL.snoc` x) f g t) .
            f $ m
       in if b
             then g (DL.toList l') xs
             else                  xs

-- O(n)
fromList :: Map map a => [[a]] -> TrieSet map a
fromList = foldl' (flip insert) empty

-- * Min/max

-- O(m log b)
findMin :: OrdMap map a => TrieSet map a -> Maybe [a]
findMin = findMinMax (\(Tr b _ _) -> b)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
findMax :: OrdMap map a => TrieSet map a -> Maybe [a]
findMax = findMinMax (\(Tr _ _ m) -> Map.null m)
                     (\(Tr b _ _) -> assert b)
                     (fst . Map.maxViewWithKey)

findMinMax :: Map map a => (TrieSet map a -> Bool)
                        -> (TrieSet map a -> [a] -> [a])
                        -> (CMap map a -> Maybe (a, TrieSet map a))
                        -> TrieSet map a
                        -> Maybe [a]
findMinMax _ _ _ tr_ | null tr_ = Nothing
findMinMax f g h tr_ = Just (go f g h tr_)
 where
   go cond base mapView tr@(Tr _ pre m) =
      if cond tr
         then base tr pre
         else let (k,t) = fromJust (mapView m)
               in prepend pre k (go cond base mapView t)

-- O(m log b)
deleteMin :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMin = maybe empty snd . minView

-- O(m log b)
deleteMax :: OrdMap map a => TrieSet map a -> TrieSet map a
deleteMax = maybe empty snd . maxView

-- O(m log b)
minView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
minView = minMaxView (\(Tr b _ _) -> b)
                     (flip const)
                     (fst . Map.minViewWithKey)

-- O(m log b)
maxView :: OrdMap map a => TrieSet map a -> Maybe ([a], TrieSet map a)
maxView = minMaxView (\(Tr _ _ m) -> Map.null m)
                     (\(Tr b _ _) -> assert b)
                     (fst . Map.maxViewWithKey)

minMaxView :: Map map a
           => (TrieSet map a -> Bool)
           -> (TrieSet map a -> ([a], TrieSet map a) -> ([a], TrieSet map a))
           -> (CMap map a -> Maybe (a, TrieSet map a))
           -> TrieSet map a
           -> Maybe ([a], TrieSet map a)
minMaxView _ _ _ tr_ | null tr_ = Nothing
minMaxView f g h tr_ = Just (go f g h DL.empty tr_)
 where
   go cond base mapView xs tr@(Tr b pre m) =
      let xs' = xs `DL.append` DL.fromList pre
       in if cond tr
             then base tr (DL.toList xs', Tr False pre m)
             else let (k,   t)  = fromJust (mapView m)
                      xs''      = xs' `DL.snoc` k
                      (res, t') = go cond base mapView xs'' t
               in ( res
                  , Tr b pre $ if null t'
                                  then Map.delete            m k
                                  else Map.adjust (const t') m k
                  )

-- O(m b)
findPredecessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
findPredecessor tr  _ | null tr = Nothing
findPredecessor tr_ xs_         = go tr_ xs_
 where
   go tr@(Tr b pre m) xs =
      case comparePrefixes (Map.eqCmp m) pre xs of
           Same             -> Nothing
           PostFix (Left _) -> Nothing

           DifferedAt _ (p:_) (x:_) ->
              case Map.ordCmp m p x of
                   LT -> findMax tr
                   GT -> Nothing
                   EQ -> can'tHappen

           -- See comment in non-Patricia version for explanation of algorithm
           PostFix (Right (y:ys)) ->
              let predecessor = Map.findPredecessor m y
               in fmap (prepend pre y) (Map.lookup m y >>= flip go ys)
                  <|>
                  case predecessor of
                       Nothing         -> if b then Just pre else Nothing
                       Just (best,btr) -> fmap (prepend pre best) (findMax btr)

           _ -> can'tHappen

   can'tHappen =
      error "Data.Trie.Patricia.Set.findPredecessor :: internal error"

-- O(m b)
findSuccessor :: OrdMap map a => TrieSet map a -> [a] -> Maybe [a]
findSuccessor tr  _ | null tr = Nothing
findSuccessor tr_ xs_         = go tr_ xs_
 where
   go tr@(Tr _ pre m) xs =
      case comparePrefixes (Map.eqCmp m) pre xs of
           Same -> do (k,t) <- fst $ Map.minViewWithKey m
                      fmap (prepend pre k) (findMin t)

           DifferedAt _ (p:_) (x:_) ->
              case Map.ordCmp m p x of
                   LT -> Nothing
                   GT -> findMin tr
                   EQ -> can'tHappen

           PostFix (Left  _)      -> findMin tr
           PostFix (Right (y:ys)) ->
              let successor = Map.findSuccessor m y
               in fmap (prepend pre y) (Map.lookup m y >>= flip go ys)
                  <|>
                  (successor >>= \(best,btr) ->
                      fmap (prepend pre best) (findMin btr))

           _ -> can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Set.findSuccessor :: internal error"

-- our private helpers
-- TODO: move into a Util module if common among multiple modules

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
-- 'insert' but not elsewhere.
tryCompress :: Map map a => TrieSet map a -> TrieSet map a
tryCompress tr@(Tr b pre m) =
   case Map.singletonView m of

        -- We can compress the trie if there is only one child
        Just (x, Tr b' pre' subM)

           -- If the parent is false, we can collapse it into the child
           | not b ->
              tryCompress $ Tr b' (prepend pre x pre') subM

           -- If the parent is true and the child is false and has no children,
           -- the child is irrelevant
           | not b' && Map.null subM ->
              Tr b pre subM

        -- If the trie is empty, make sure the prefix is as well.
        --
        -- This case can arise in 'intersection', at least.
        Nothing | not b -> Tr b [] m

        -- Otherwise, leave it unchanged.
        _ -> tr
