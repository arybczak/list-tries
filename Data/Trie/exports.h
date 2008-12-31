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
	addPrefix, splitPrefix, lookupPrefix

#define SET_EXPORTS TrieSet, COMMON_EXPORTS, \
	isSubsetOf, isProperSubsetOf, \
	\
	splitMember, \
	\
	map, map', \
	\
	fold, foldAsc, foldDesc

#define MAP_EXPORTS TrieMap, COMMON_EXPORTS, \
	lookup, \
	\
	isSubmapOf, isSubmapOfBy, \
	isProperSubmapOf, isProperSubmapOfBy, \
	\
	insertWith, insertWithKey, adjust, update, updateLookup, alter, \
	\
	unionWith, unionWithKey, unionsWith, \
	differenceWith, differenceWithKey, intersectionWith, intersectionWithKey, \
	\
	filterWithKey, partitionWithKey, splitLookup, \
	\
	mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey, \
	\
	map, mapWithKey, mapAccum, mapAccumWithKey, \
	mapAccumAsc, mapAccumAscWithKey, mapAccumDesc, mapAccumDescWithKey, \
	mapKeys, mapKeysWith, mapKeys', mapKeys'With, \
	\
	fold, foldWithKey, foldAsc, foldAscWithKey, foldDesc, foldDescWithKey, \
	\
	fromListWith, fromListWithKey
