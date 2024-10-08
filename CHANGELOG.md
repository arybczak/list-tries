# list-tries-0.6.7.1 (2024-09-04)
* Make tests compile with GHC >= 9.0.
* Drop support for GHC < 8.10.

# list-tries-0.6.7 (2020-05-12)
* Updated dependencies and code for GHC 8.8 and 8.10.

# list-tries-0.6.6 (2018-11-20)
* Updated dependencies and code for GHC 8.6.1, thanks as usual to Mikhail
  Glushenkov.

# list-tries-0.6.5 (2018-03-20)
* Updated dependencies and code for GHC 8.4.1 and QuickCheck-2.11,
  thanks again to Mikhail Glushenkov.

# list-tries-0.6.4 (2017-05-27)
* Updated dependencies for GHC 8.2.1 (release candidate 2) and
  binary-0.9, thanks to Mikhail Glushenkov for helping with this.
* Also a code change due to a newly problematic type inference.

# list-tries-0.6.3 (2016-07-18)
* Dependency update to allow dlist-0.8.

# list-tries-0.6.2 (2016-06-28)
* Updated dependencies for GHC 8.0.1.
* Added Semigroup instances, bringing in a new dependency on semigroups
  on pre-8.0 GHC versions.

# list-tries-0.6.1 (2015-04-03)
* Fixed build on base < 4.8.

# list-tries-0.6.0 (2015-03-28)
* Updated dependencies for GHC 7.10.
* Fixed library vs. test executable dlist dependency mismatch.
* Renamed `Map.toList` to `toListKV` to avoid conflicts with the new
  `Foldable` class. Also renamed `Map.fromList` and `Map.fromListWith`
  to `fromListKV` and `fromListKVWith` to match. Thanks to davean for
  the patch.
* Added Cabal source-repository metadata, pointing to GitHub.

# list-tries-0.5.2 (2014-03-20)
* Updated dependencies, for GHC 7.8 and other new packages.

# list-tries-0.5.1 (2013-05-10)
* Fix cabal build.
* Minor documentation clarification.
* Update `binary` dependency.

# list-tries-0.5 (2013-05-09)
* Added the `lookupPrefix` and `deleteSuffixes` functions, of which especially
  the former was an embarrassing omission:

        lookupPrefix   :: [k] -> trie map k a -> trie map k a
        deleteSuffixes :: [k] -> trie map k a -> trie map k a

* Fixed the documentation headers to refer to 's' instead of 'k' as what we
  use for the length of the given key.
* Fixed documentation of 'deletePrefix': its complexity is O(s), not O(m).
* Some dependency updates.

# list-tries-0.4.3 (2012-10-18)
* Dependency updates for GHC 7.6 and otherwise.

# list-tries-0.4.2 (2012-05-23)
* Dependency updates for GHC 7.4, thanks to Anders Kaseorg.

# list-tries-0.4.1 (2011-03-17)
* Dependency update and Cabalization of the test executable, thanks to JP
  Moresmau.

# list-tries-0.4 (2010-09-11)
* Fixed documentation of the `deletePrefix` function: if the given key is not
  a prefix of any key, an empty, not unchanged, map/set is returned. Thanks to
  Brian Bloniarz for the bug report.
* Fixed bug in the Patricia version of `deletePrefix` causing it to not delete
  anything if the prefix to be deleted was a proper prefix of the common
  prefix.
* Changed `children` to return the map as-is instead of converting it into a
  list first:

        children :: Trie trie st map k => trie map k a -> CMap trie map k a

* Added the `children1` function as a single-level equivalent of `children`,
  more directly reflecting the structure of the non-Patricia tries. Requested
  by Brian Bloniarz.

        children1 :: Trie trie st map k => trie map k a -> CMap trie map k a

# list-tries-0.3 (2010-09-09)
* Fixed strictness of the strict versions of the following
  non-Patricia functions: `insert`, `adjust`, `alter`, `union`,
  `difference`, `intersection`, `mapInKeys`; as well as the Patricia
  versions of `insert` and `adjust`. Thanks to Brian Bloniarz for the
  bug report.
* Applied the static argument transformation throughout, improving
  performance.
* Dropped support for containers < 0.3; GHC 6.12 has been out long
  enough, and support for older versions is too crippled to make it
  worthwhile.

# list-tries-0.2 (2010-04-06)
* Dependency update, nothing more.

# list-tries-0.1 (2009-07-05)
* All tries are now instances of `Binary`, thanks to Gregory Crosswhite. Adds a
  dependency on the `binary` library as well as the following two methods to
  the `Map` class in `Base.Map`:

        serializeToList     :: m k a -> [(k,a)]
        deserializeFromList :: [(k,a)] -> m k a

# list-tries-0.0 (2009-04-21)
* Initial release.
