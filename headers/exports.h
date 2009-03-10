-- File created: 2008-12-30 18:33:18

#define SET_EXPORTS \
	{- * Set type -} \
	TrieSet, \
	{- * Construction -} \
	empty, singleton, \
	\
	{- * Modification -} \
	insert, delete, \
	\
	{- * Querying -} \
	null, size, size', member, notMember, \
	\
	{- ** Subsets -} \
	isSubsetOf, isProperSubsetOf, \
	\
	{- * Combination -} \
	union, unions, difference, intersection, \
	\
	{- * Filtering -} \
	filter, partition, \
	\
	{- * Mapping -} \
	map, mapIn, \
	\
	{- * Folding -} \
	foldr, foldrAsc, foldrDesc, \
	foldl, foldlAsc, foldlDesc, \
	foldl', foldlAsc', foldlDesc', \
	\
	{- * Conversion to and from lists -} \
	toList, toAscList, toDescList, fromList, \
	\
	{- * Ordering-sensitive operations -} \
	{- ** Minimum and maximum -} \
	minView, maxView, findMin, findMax, deleteMin, deleteMax, \
	\
	{- ** Predecessor and successor -} \
	split, splitMember, \
	findPredecessor, findSuccessor, \
	\
	{- * Trie-specific operations -} \
	{- $trie-only-ops -} \
	addPrefix, deletePrefix, splitPrefix, children, \
	\
	{- * Visualization -} \
	showTrie

#define MAP_EXPORTS \
	{- * Map type -} \
	TrieMap, \
	\
	{- * Construction -} \
	empty, singleton, \
	\
	{- * Modification -} \
	insert, insert', insertWith, insertWith', \
	delete, \
	update, updateLookup, \
	adjust, adjust', alter, alter', \
	\
	{- * Querying -} \
	null, size, size', member, notMember, \
	lookup, lookupWithDefault, \
	\
	{- ** Submaps -} \
	isSubmapOf, isSubmapOfBy, \
	isProperSubmapOf, isProperSubmapOfBy, \
	\
	{- * Combination -} \
	{- ** Union -} \
	union, union', unions, unions', \
	unionWith,  unionWithKey,  unionsWith,  unionsWithKey, \
	unionWith', unionWithKey', unionsWith', unionsWithKey', \
	\
	{- ** Difference -} \
	difference, differenceWith, differenceWithKey, \
	\
	{- ** Intersection -} \
	intersection, intersection', \
	intersectionWith,  intersectionWithKey, \
	intersectionWith', intersectionWithKey', \
	\
	{- * Filtering -} \
	filter, filterWithKey, partition, partitionWithKey, \
	mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey, \
	\
	{- * Mapping -} \
	{- ** Values -} \
	map, map', mapWithKey, mapWithKey', \
	\
	{- ** Keys -} \
	mapKeys, mapKeysWith, \
	mapInKeys, mapInKeys', mapInKeysWith, mapInKeysWith', \
	\
	{- ** With accumulation -} \
	mapAccum,      mapAccumWithKey, \
	mapAccum',     mapAccumWithKey', \
	mapAccumAsc,   mapAccumAscWithKey, \
	mapAccumAsc',  mapAccumAscWithKey', \
	mapAccumDesc,  mapAccumDescWithKey, \
	mapAccumDesc', mapAccumDescWithKey', \
	\
	{- * Folding -} \
	foldr, foldrWithKey, \
	foldrAsc, foldrAscWithKey, \
	foldrDesc, foldrDescWithKey, \
	foldl, foldlWithKey, \
	foldlAsc, foldlAscWithKey, \
	foldlDesc, foldlDescWithKey, \
	foldl', foldlWithKey', \
	foldlAsc', foldlAscWithKey', \
	foldlDesc', foldlDescWithKey', \
	\
	{- * Conversion to and from lists -} \
	toList, toAscList, toDescList, fromList, \
	fromListWith,  fromListWithKey, \
	fromListWith', fromListWithKey', \
	\
	{- * Ordering-sensitive operations -} \
	{- ** Minimum and maximum -} \
	minView, maxView, findMin, findMax, deleteMin, deleteMax, \
	\
	{- ** Predecessor and successor -} \
	split, splitLookup, \
	findPredecessor, findSuccessor, \
	\
	{- * Trie-specific operations -} \
	{- $trie-only-ops -} \
	addPrefix, deletePrefix, splitPrefix, children, \
	\
	{- * Visualization -} \
	showTrie, showTrieWith
