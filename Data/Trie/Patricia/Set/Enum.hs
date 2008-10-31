-- File created: 2008-10-22 20:44:46

-- An efficient implementation of sets of lists of enumerable elements, based
-- on Patricia tries.
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

module Data.Trie.Patricia.Set.Enum where

import Control.Arrow ((***))
import Control.Exception (assert)
import qualified Data.DList as DL
import Data.DList (DList)
import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import qualified Data.List as List
import Data.List (foldl')
import Data.Maybe (fromJust)
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

-- Invariant: any (Tr False _ _) has at least two children, all of which are
-- True or have a True descendant.
data TrieSet a = Tr !Bool [a] !(IntMap (TrieSet a)) deriving (Show,Eq)

-- instances: Monoid, Foldable, Ord, Show, Read

-- * Querying

-- O(1)
null :: TrieSet a -> Bool
null (Tr False p m) | Map.null m = assert (Prelude.null p) True
null _                           = False

-- O(n). The number of elements in the set.
size :: TrieSet a -> Int
size (Tr b _ m) = Map.fold ((+) . size) (fromEnum b) m

-- O(m).
member :: (Eq a, Enum a) => [a] -> TrieSet a -> Bool
member k (Tr b prefix m) =
   case comparePrefixes prefix k of
        Same                   -> b
        PostFix (Right (x:xs)) ->
           case Map.lookup (fromEnum x) m of
                Nothing -> False
                Just t  -> member xs t

        _ -> False

-- O(?)
isSubsetOf :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> Bool
isSubsetOf (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        DifferedAt _ _ _  -> False

        -- Special case here: if the left trie is empty we return True.
        PostFix (Right _) -> not b1 && Map.null m1

        PostFix (Left xs) -> go m2 b1 m1 xs
        Same              -> not (b1 && not b2)
                          && Map.isSubmapOfBy isSubsetOf m1 m2
 where
   go mr bl ml (x:xs) =
      case Map.lookup (fromEnum x) mr of
           Nothing              -> False
           Just (Tr br pre mr') ->
              case comparePrefixes xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go mr' bl ml ys
                   Same              -> not (bl && not br)
                                     && Map.isSubmapOfBy isSubsetOf ml mr'

   go _ _ _ _ =
      error "Data.Trie.Patricia.Set.Enum.isSubsetOf :: internal error"

-- O(?)
isProperSubsetOf :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> Bool
isProperSubsetOf = f False
 where
   f proper (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
      case comparePrefixes pre1 pre2 of
           DifferedAt _ _ _  -> False

           -- Special case, as in isSubsetOf.
           --
           -- Note that properness does not affect this: if we hit this case,
           -- we already know that the right trie is nonempty.
           PostFix (Right _) -> not b1 && Map.null m1

           PostFix (Left xs) -> go proper m2 b1 m1 xs
           Same              -> same proper b1 b2 m1 m2

   go proper mr bl ml (x:xs) =
      case Map.lookup (fromEnum x) mr of
           Nothing              -> False
           Just (Tr br pre mr') ->
              case comparePrefixes xs pre of
                   DifferedAt _ _ _  -> False
                   PostFix (Right _) -> False
                   PostFix (Left ys) -> go proper mr' bl ml ys
                   Same              -> same proper bl br ml mr'

   go _ _ _ _ _ =
      error "Data.Trie.Patricia.Set.Enum.isProperSubsetOf :: internal error"

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
empty :: TrieSet a
empty = Tr False [] Map.empty

-- O(m)
singleton :: [a] -> TrieSet a
singleton s = Tr True s Map.empty

-- O(m)
insert :: (Eq a, Enum a) => [a] -> TrieSet a -> TrieSet a
insert k (Tr b prefix m) =
   case comparePrefixes prefix k of
        Same                   -> Tr True prefix m
        PostFix (Left  (p:pr)) -> Tr True k (mapSingleton p pr b m)
        PostFix (Right (x:xs)) ->
           -- Minor optimization: instead of tryCompress we just check for the
           -- case of an empty trie
           if not b && Map.null m
              then Tr True k m
              else Tr b prefix (mapInsert x xs m)

        DifferedAt pr' (p:pr) (x:xs) ->
           Tr False pr' $ mapDoubleton x (singleton xs) p (Tr b pr m)

        _ -> error "Data.Trie.Patricia.Set.Enum.insert :: internal error"
 where
   mapInsert c cs = Map.insertWith (\_ old -> insert cs old)
                                   (fromEnum c)
                                   (singleton cs)

   mapSingleton x xs b' m' = Map.singleton (fromEnum x) (Tr b' xs m')

-- O(m)
delete :: (Eq a, Enum a) => [a] -> TrieSet a -> TrieSet a
delete k tr@(Tr b prefix m) =
   case comparePrefixes prefix k of
        Same                   -> tryCompress (Tr False prefix m)
        PostFix (Right (x:xs)) ->
           tryCompress . Tr b prefix $
              Map.update (\old -> let new = delete xs old
                                   in if null new
                                         then Nothing
                                         else Just new)
                         (fromEnum x) m
        _ -> tr

-- * Combination

-- I think all the properly-written ones here are O(min(n1,n2)).

union :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
union (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        Same              -> tryCompress $ Tr (b1 || b2) pre1 (mapUnion m1 m2)
        PostFix remainder ->
           tryCompress $ either (Tr b2 pre2 . mapUnion m2 . decompress m1 b1)
                                (Tr b1 pre1 . mapUnion m1 . decompress m2 b2)
                                remainder

        DifferedAt pr (x:xs) (y:ys) ->
           Tr False pr $ mapDoubleton x (Tr b1 xs m1) y (Tr b2 ys m2)

        _ -> can'tHappen
 where
   mapUnion = Map.unionWith union

   decompress m b (x:xs) = Map.singleton (fromEnum x) (Tr b xs m)
   decompress _ _ []     = can'tHappen

   can'tHappen = error "Data.Trie.Patricia.Set.Enum.union :: internal error"

unions :: (Eq a, Enum a) => [TrieSet a] -> TrieSet a
unions = foldl' union empty

difference :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
difference tr1@(Tr b1 pre1 m1) tr2@(Tr b2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
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
      case Map.lookup (fromEnum x) rightMap of
           Nothing                     -> left
           Just right'@(Tr b' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> left
                   Same               -> tr b b' pre (mapDifference m m')
                   PostFix (Left  ys) -> goRight left m'     ys
                   PostFix (Right ys) -> goLeft  left right' ys

   goRight _ _ _ =
      error "Data.Trie.Patricia.Set.Enum.difference :: internal error"

   goLeft (Tr bl prel ml) right@(Tr br _ mr) (x:xs) =
      tryCompress . Tr bl prel $ Map.adjust f (fromEnum x) ml
    where
      f left@(Tr b pre m) =
         case comparePrefixes pre xs of
              DifferedAt _ _ _   -> left
              Same               -> tr b br pre (mapDifference m mr)
              PostFix (Left  ys) -> goRight left mr    ys
              PostFix (Right ys) -> goLeft  left right ys

   goLeft _ _ _ =
      error "Data.Trie.Patricia.Set.Enum.difference :: internal error"

intersection :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
intersection (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
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
      case Map.lookup (fromEnum x) ma of
           Nothing              -> empty
           Just (Tr b' pre' m') ->
              case comparePrefixes xs pre' of
                   DifferedAt _ _ _   -> empty
                   Same               -> tr b b' pre (mapIntersect mb m')
                   PostFix (Right ys) -> go mb b' m' (pre ++ ys) ys
                   PostFix (Left  ys) -> go m' b mb pre ys

   go _ _ _ _ _ =
      error "Data.Trie.Patricia.Set.Enum.intersect :: internal error"

-- * Filtering

-- O(n)
filter :: (Eq a, Enum a) => ([a] -> Bool) -> TrieSet a -> TrieSet a
filter p = fromList . Prelude.filter p . toList

-- O(n)
partition :: (Eq a, Enum a) => ([a] -> Bool)
                            -> TrieSet a
                            -> (TrieSet a, TrieSet a)
partition p = (fromList *** fromList) . List.partition p . toList

-- * Mapping

-- O(n)
map :: (Enum a, Eq b, Enum b) => ([a] -> [b]) -> TrieSet a -> TrieSet b
map f = fromList . Prelude.map f . toList

-- O(n)
-- needs a name!
map' :: (Enum a, Enum b) => (a -> b) -> TrieSet a -> TrieSet b
map' f (Tr b p m) =
   Tr b (Prelude.map f p) $
      Map.fromDistinctAscList .
         Prelude.map (fromEnum.f.toEnum *** map' f) .
      Map.toAscList $ m

-- * Folding

-- O(n)
fold :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
fold f x = Prelude.foldr f x . toList

-- O(n)
foldAsc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldAsc f x = Prelude.foldr f x . toAscList

-- O(n)
foldDesc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldDesc f x = Prelude.foldr f x . toDescList

-- * Conversion between lists

-- O(n)
toList :: Enum a => TrieSet a -> [[a]]
toList = genericToList Map.toList DL.cons

-- O(n)
toAscList :: Enum a => TrieSet a -> [[a]]
toAscList = genericToList Map.toAscList DL.cons

-- O(n)
toDescList :: Enum a => TrieSet a -> [[a]]
toDescList = genericToList (reverse . Map.toAscList) (flip DL.snoc)

genericToList :: Enum a => (IntMap (TrieSet a) -> [(Int, TrieSet a)])
                        -> ([a] -> DList [a] -> DList [a])
                        -> TrieSet a
                        -> [[a]]
genericToList f_ g_ = DL.toList . go DL.empty f_ g_
 where
   go l f g (Tr b p m) =
      let
         l' = l `DL.append` DL.fromList p
         xs =
            DL.concat .
            Prelude.map (\(x,t) -> go (l' `DL.snoc` toEnum x) f g t) .
            f $ m
       in if b
             then g (DL.toList l') xs
             else                  xs

-- O(n)
fromList :: (Eq a, Enum a) => [[a]] -> TrieSet a
fromList = foldl' (flip insert) empty

-- * Min/max

-- O(m log b)
findMin :: (Ord a, Enum a) => TrieSet a -> Maybe [a]
findMin tr | null tr = Nothing
findMin tr = go tr
 where
   go (Tr b pre m) =
      if b
         then Just pre
         else let (k,t) = fst . fromJust . Map.minViewWithKey $ m
               in fmap (prepend pre k) (go t)

-- O(m log b)
findMax :: (Ord a, Enum a) => TrieSet a -> Maybe [a]
findMax tr | null tr = Nothing
findMax tr = go tr
 where
   go (Tr b pre m) =
      if Map.null m
         then assert b $ Just pre
         else let (k,t) = fst . fromJust . Map.maxViewWithKey $ m
               in fmap (prepend pre k) (go t)

-- our private helpers
-- TODO: move into a Util module if common among multiple modules

prepend :: Enum a => [a] -> Int -> [a] -> [a]
prepend prefix key = (prefix++) . (toEnum key:)

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
tryCompress :: Enum a => TrieSet a -> TrieSet a
tryCompress tr@(Tr b pre m) =
   case Map.minViewWithKey m of

        -- We can compress the trie if there is only one child

        Just ((x, Tr b' pre' subM), otherChildren)

           -- If the parent is false, we can collapse it into the child
           | Map.null otherChildren && not b ->
              tryCompress $ Tr b' (prepend pre x pre') subM

           -- If the parent is true and the child is false and has no children,
           -- the child is irrelevant
           | Map.null otherChildren && not b' && Map.null subM ->
              Tr b pre subM

        -- If the trie is empty, make sure the prefix is as well.
        --
        -- This case can arise in 'intersection', at least.

        Nothing | not b -> Tr b [] m

        -- Otherwise, leave it unchanged.
        _ -> tr

mapDoubleton :: Enum a => a -> b -> a -> b -> IntMap b
mapDoubleton k1 v1 k2 v2 =
   Map.insert (fromEnum k1) v1 (Map.singleton (fromEnum k2) v2)
