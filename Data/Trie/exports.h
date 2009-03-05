-- File created: 2008-12-30 18:33:18

#define COMMON_EXPORTS \
	null, size, member, notMember, \
	\
	empty, singleton, insert, delete, \
	\
	union, unions, difference, intersection, \
	\
	filter, partition, split, \
	\
	toList, toAscList, toDescList, fromList, \
	\
	findMin, findMax, deleteMin, deleteMax, minView, maxView, \
	findPredecessor, findSuccessor, \
	\
	addPrefix, splitPrefix, lookupPrefix, children, \
	\
	showTrie

#define SET_EXPORTS TrieSet, COMMON_EXPORTS, \
	isSubsetOf, isProperSubsetOf, \
	\
	splitMember, \
	\
	map, mapIn, \
	\
	foldr, foldrAsc, foldrDesc, \
	foldl', foldlAsc', foldlDesc'

#define MAP_EXPORTS TrieMap, COMMON_EXPORTS, \
	lookup, lookupWithDefault, \
	\
	isSubmapOf, isSubmapOfBy, \
	isProperSubmapOf, isProperSubmapOfBy, \
	\
	insertWith, insertWith', \
   adjust, adjust', update, updateLookup, alter, alter', \
	\
	union', unions', \
	unionWith,  unionWithKey,  unionsWith,  unionsWithKey, \
	unionWith', unionWithKey', unionsWith', unionsWithKey', \
	differenceWith,  differenceWithKey, \
	intersection', \
	intersectionWith,  intersectionWithKey, \
	intersectionWith', intersectionWithKey', \
	\
	filterWithKey, partitionWithKey, splitLookup, \
	\
	mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey, \
	\
	map, map', mapWithKey, mapWithKey', \
	mapAccum,      mapAccumWithKey, \
	mapAccum',     mapAccumWithKey', \
	mapAccumAsc,   mapAccumAscWithKey, \
	mapAccumAsc',  mapAccumAscWithKey', \
	mapAccumDesc,  mapAccumDescWithKey, \
	mapAccumDesc', mapAccumDescWithKey', \
	mapKeys, mapKeysWith, \
	mapInKeys, mapInKeys', mapInKeysWith, mapInKeysWith', \
	\
	foldr, foldrWithKey, \
	foldrAsc, foldrAscWithKey, \
	foldrDesc, foldrDescWithKey, \
	foldl', foldlWithKey', \
	foldlAsc', foldlAscWithKey', \
	foldlDesc', foldlDescWithKey', \
	\
	fromListWith,  fromListWithKey, \
	fromListWith', fromListWithKey', \
	\
	showTrieWith
