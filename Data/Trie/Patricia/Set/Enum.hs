-- File created: 2008-10-22 20:44:46

-- An efficient implementation of sets of lists of enumerable elements, based
-- on Patricia tries.
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length.

module Data.Trie.Patricia.Set.Enum where

import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import Data.List (foldl')
import Prelude hiding (lookup, filter, foldl, foldr, null, map)
import qualified Prelude

data TrieSet a = Tr !Bool [a] !(IntMap (TrieSet a)) deriving Show

-- instances: Eq, Monoid, Foldable, Ord, Show, Read

-- * Querying

-- O(1)
null :: TrieSet a -> Bool
null (Tr False [] m) | Map.null m = True
null _                            = False

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

isSubsetOf :: TrieSet a -> TrieSet a -> Bool
isSubsetOf = undefined

isProperSubsetOf :: TrieSet a -> TrieSet a -> Bool
isProperSubsetOf = undefined

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
           Tr False pr' $ mapInsert x xs (mapSingleton p pr b m)

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


-- After deletion, compress a trie node into the prefix if possible
tryCompress :: Enum a => TrieSet a -> TrieSet a
tryCompress tr@(Tr b pre m) =
   case Map.minViewWithKey m of

        -- We can compress the trie if both of the following hold:
        --   Either node is empty    --- not b || not b'
        --   There is only one child --- Map.null m'

        Just ((x, Tr b' pre' subM), otherChildren)
           | (not b || not b') && Map.null otherChildren ->
              Tr (b || b') (pre ++ toEnum x:pre') subM

        -- Otherwise we leave it unchanged.
        _ -> tr


-- * Combination

-- TODO: improve these

-- O(m2*n2)... I think...
union :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
union tr1 = foldl' (flip insert) tr1 . toList

unions :: (Eq a, Enum a) => [TrieSet a] -> TrieSet a
unions = foldl' union empty

-- O(m2*n2)... I think...
difference :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
difference tr1 = foldl' (flip delete) tr1 . toList

-- Something like O(min(n1,n2))?
intersection :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
intersection (Tr b1 pre1 m1) (Tr b2 pre2 m2) =
   case comparePrefixes pre1 pre2 of
        DifferedAt _ _ _ -> empty
        Same             -> tr (b1 && b2) pre1 (mapIntersect m1 m2)

        -- use the one with a longer prefix as the base for the intersection,
        -- and descend into the map of the one with a shorter prefix
        PostFix remainder ->
           either (go m2 b1 m1 pre1) (go m1 b2 m2 pre2) remainder

 where
   mapIntersect = Map.intersectionWith intersection
   tr b p m = tryCompress (Tr b p m)

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
			          Same               -> tr (b && b') pre (mapIntersect mb m')
			          PostFix (Right ys) -> go mb b' m' (pre ++ ys) ys
			          PostFix (Left  ys) -> go m' b mb pre ys

   go _ _ _ _ [] =
      error "Data.Trie.Patricia.Set.Enum.intersect :: internal error"

-- * Filtering

-- O(n)
filter :: (Eq a, Enum a) => ([a] -> Bool) -> TrieSet a -> TrieSet a
filter = undefined

-- O(n)
partition :: (Eq a, Enum a) => (a -> Bool) -> TrieSet a -> (TrieSet a, TrieSet a)
partition = undefined

-- * Mapping

-- O(n)
map :: (Eq a, Enum a, Eq b, Enum b) => ([a] -> [b]) -> TrieSet a -> TrieSet b
map = undefined

-- O(n)
-- needs a name!
map' :: (Eq a, Enum a, Eq b, Enum b) => (a -> b) -> TrieSet a -> TrieSet b
map' = undefined

-- * Folding

fold :: ([a] -> b -> b) -> b -> TrieSet a -> b
fold = undefined

foldAsc :: ([a] -> b -> b) -> b -> TrieSet a -> b
foldAsc = undefined

foldDesc :: ([a] -> b -> b) -> b -> TrieSet a -> b
foldDesc = undefined

-- * Conversion between lists

-- O(n)
toList :: Enum a => TrieSet a -> [[a]]
toList = genericToList Map.toList

-- O(n)
toAscList :: Enum a => TrieSet a -> [[a]]
toAscList = genericToList Map.toAscList

genericToList :: Enum a => (IntMap (TrieSet a) -> [(Int, TrieSet a)])
                        -> TrieSet a
                        -> [[a]]
genericToList = go []
 where
   go l f (Tr b p m) =
      let xs = concatMap (\(x,t) -> go (toEnum x:reverse p ++ l) f t) (f m)
       in if b
             then (reverse l ++ p) : xs
             else                    xs

-- O(n)
fromList :: (Eq a, Enum a) => [[a]] -> TrieSet a
fromList = foldl' (flip insert) empty


-- our private helpers
-- TODO: move into a Util module if common among multiple modules

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
