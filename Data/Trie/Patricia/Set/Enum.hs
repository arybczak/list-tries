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
   case isPrefix prefix k of
        Nothing     -> False -- didn't match prefix
        Just []     -> b     -- k == prefix
        Just (x:xs) ->       -- prefix was a prefix of k
           case Map.lookup (fromEnum x) m of
                Nothing -> False
                Just t  -> member xs t

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
   case prefixCheck [] prefix k of
        -- k == prefix
        Right (Right [])       -> Tr True prefix m

        -- k was a prefix of prefix
        Right (Left  (p:pr))   ->
           Tr True k (mapSingleton p pr b m)

        -- prefix was a prefix of k
        Right (Right (x:xs))   ->
           if not b && Map.null m
              then Tr True k m
              else Tr b prefix (mapInsert x xs m)

        -- none of the above
        Left (pr', p:pr, x:xs) ->
           Tr False pr' (mapInsert x xs $ mapSingleton p pr b m)

        _ -> error "Data.Trie.Patricia.Set.Enum.insert :: internal error"
 where
   prefixCheck _  []       []       = Right (Right [])
   prefixCheck _  []       cs       = Right (Right cs)
   prefixCheck _  pr       []       = Right (Left pr)
   prefixCheck pr x@(p:ps) y@(c:cs) =
      if p == c
         then prefixCheck (p:pr) ps cs
         else Left (reverse pr, x, y)

   mapInsert c cs = Map.insertWith (\_ old -> insert cs old)
                                   (fromEnum c)
                                   (singleton cs)

   mapSingleton x xs b' m' = Map.singleton (fromEnum x) (Tr b' xs m')

-- O(m)
delete :: (Eq a, Enum a) => [a] -> TrieSet a -> TrieSet a
delete k tr@(Tr b prefix m) =
   case isPrefix prefix k of
        -- k == prefix
        Just [] -> tryCompress (Tr False prefix m)

        -- prefix was a prefix of k
        Just (x:xs) ->
           tryCompress . Tr b prefix $
              Map.update (\old -> let new = delete xs old
                                   in if null new
                                         then Nothing
                                         else Just new)
                         (fromEnum x) m

        -- none of the above: k is not in the Trie
        Nothing -> tr

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

-- O((n1+n2)*(m1+m2))... I think...
intersection :: (Eq a, Enum a) => TrieSet a -> TrieSet a -> TrieSet a
intersection tr1 tr2 =
   foldl' (\t x -> if member x tr1 && member x tr2
                      then insert x t
                      else t)
          empty (toList tr1 ++ toList tr2)

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
      let xs = concatMap (\(x,t) -> go (toEnum x:p ++ l) f t) (f m)
       in if b
             then (reverse l ++ p) : xs
             else                    xs

-- O(n)
fromList :: (Eq a, Enum a) => [[a]] -> TrieSet a
fromList = foldl' (flip insert) empty


-- our private helpers
-- TODO: move into a Util module if common among multiple modules

isPrefix :: Eq a => [a] -> [a] -> Maybe [a]
isPrefix []     xs     = Just xs
isPrefix _      []     = Nothing
isPrefix (p:ps) (x:xs) =
   if p == x
      then isPrefix ps xs
      else Nothing
