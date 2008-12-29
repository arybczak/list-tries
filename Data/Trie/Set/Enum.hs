-- File created: 2008-10-18 21:33:40

-- A set of lists of enumerable elements, based on a trie.
--
-- Note that those operations which require an ordering, such as 'toAscList',
-- do not compare the elements themselves, but rather their Int representation
-- after 'fromEnum'.
--
-- Complexities are given; @n@ refers to the number of elements in the set, @m@
-- to their maximum length, @b@ to the trie's branching factor.

module Data.Trie.Set.Enum where

import qualified Data.Trie.Base.Map as Map
import qualified Data.Trie.Set      as Base

type TrieSet = Base.TrieSet Map.IMap

-- * Querying

-- O(1)
null :: Enum a => TrieSet a -> Bool
null = Base.null

-- O(n). The number of elements in the set.
size :: Enum a => TrieSet a -> Int
size = Base.size

-- O(m).
member :: Enum a => [a] -> TrieSet a -> Bool
member = Base.member

-- O(?)
isSubsetOf :: Enum a => TrieSet a -> TrieSet a -> Bool
isSubsetOf = Base.isSubsetOf

-- O(?)
isProperSubsetOf :: Enum a => TrieSet a -> TrieSet a -> Bool
isProperSubsetOf = Base.isProperSubsetOf

-- * Construction

-- O(1)
empty :: Enum a => TrieSet a
empty = Base.empty

-- O(m)
singleton :: Enum a => [a] -> TrieSet a
singleton = Base.singleton

-- O(m)
insert :: Enum a => [a] -> TrieSet a -> TrieSet a
insert = Base.insert

-- O(m)
delete :: Enum a => [a] -> TrieSet a -> TrieSet a
delete = Base.delete

-- * Combination

-- O(n1+n2)
union :: Enum a => TrieSet a -> TrieSet a -> TrieSet a
union = Base.union

unions :: Enum a => [TrieSet a] -> TrieSet a
unions = Base.unions

-- O(n1+n2)
difference :: Enum a => TrieSet a -> TrieSet a -> TrieSet a
difference = Base.difference

-- O(n1+n2)
intersection :: Enum a => TrieSet a -> TrieSet a -> TrieSet a
intersection = Base.intersection

-- * Filtering

-- O(n)
filter :: Enum a => ([a] -> Bool) -> TrieSet a -> TrieSet a
filter = Base.filter

-- O(n)
partition :: Enum a => ([a] -> Bool) -> TrieSet a -> (TrieSet a, TrieSet a)
partition = Base.partition

split :: Enum a => [a] -> TrieSet a -> (TrieSet a, TrieSet a)
split = Base.split

splitMember :: Enum a => [a] -> TrieSet a -> (TrieSet a, Bool, TrieSet a)
splitMember = Base.splitMember

-- * Mapping

-- O(n)
map :: (Enum a, Enum b) => ([a] -> [b]) -> TrieSet a -> TrieSet b
map = Base.map

-- O(n)
-- needs a name!
map' :: (Enum a, Enum b) => (a -> b) -> TrieSet a -> TrieSet b
map' = Base.map'

-- * Folding

-- O(n)
fold :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
fold = Base.fold

-- O(n)
foldAsc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldAsc = Base.foldAsc

-- O(n)
foldDesc :: Enum a => ([a] -> b -> b) -> b -> TrieSet a -> b
foldDesc = Base.foldDesc

-- * Conversion between lists

-- O(n)
toList :: Enum a => TrieSet a -> [[a]]
toList = Base.toList

-- O(n)
toAscList :: Enum a => TrieSet a -> [[a]]
toAscList = Base.toAscList

-- O(n)
toDescList :: Enum a => TrieSet a -> [[a]]
toDescList = Base.toDescList

-- O(n)
fromList :: Enum a => [[a]] -> TrieSet a
fromList = Base.fromList

-- * Min/max

-- O(m log b)
findMin :: Enum a => TrieSet a -> Maybe [a]
findMin = Base.findMin

-- O(m log b)
findMax :: Enum a => TrieSet a -> Maybe [a]
findMax = Base.findMax

-- O(m log b)
deleteMin :: Enum a => TrieSet a -> TrieSet a
deleteMin = Base.deleteMin

-- O(m log b)
deleteMax :: Enum a => TrieSet a -> TrieSet a
deleteMax = Base.deleteMax

-- O(m log b)
minView :: Enum a => TrieSet a -> Maybe ([a], TrieSet a)
minView = Base.minView

-- O(m log b)
maxView :: Enum a => TrieSet a -> Maybe ([a], TrieSet a)
maxView = Base.maxView

-- O(m b)
findPredecessor :: Enum a => TrieSet a -> [a] -> Maybe [a]
findPredecessor = Base.findPredecessor

-- O(m b)
findSuccessor :: Enum a => TrieSet a -> [a] -> Maybe [a]
findSuccessor = Base.findSuccessor

-- * Trie-only operations

addPrefix :: Enum a => [a] -> TrieSet a -> TrieSet a
addPrefix = Base.addPrefix

splitPrefix :: Enum a => TrieSet a -> ([a], TrieSet a)
splitPrefix = Base.splitPrefix

lookupPrefix :: Enum a => [a] -> TrieSet a -> TrieSet a
lookupPrefix = Base.lookupPrefix
