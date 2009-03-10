-- File created: 2009-03-08 20:36:00

-- $trie-only-ops
--
-- Functions which utilize the unique structure of tries.
--
-- 'addPrefix' and 'deletePrefix' allow fast adding and removing of prefixes
-- to/from all keys of a trie.
--
-- 'splitPrefix' and 'children' allow traversing of a trie in a manner suitable
-- for its structure.

-- I would have most of the docs here but using #defines and relying on cpphs's
-- --layout flag is a pain due to
-- http://hackage.haskell.org/trac/hackage/ticket/519, and Haddock can't help
-- me until http://trac.haskell.org/haddock/ticket/97 gets attention.
