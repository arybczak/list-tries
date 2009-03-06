-- File created: 2009-03-06 12:40:42

-- Base.Map plus stuff we don't want to export
module Data.Trie.Base.Map.Internal
   (module Data.Trie.Base.Map, difference) where

import Data.Trie.Base.Map

-- Moved this outside Map because it's an odd one out: union and intersection
-- aren't needed
difference :: Map m k => m k a -> m k b -> m k a
difference = differenceWith (\_ _ -> Nothing)
