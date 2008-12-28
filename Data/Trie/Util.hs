-- File created: 2008-12-27 22:04:52

module Data.Trie.Util ((.:), (.:.), both) where

infixr 9 .:, .:.

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f .: g) x y = f (g x y)

(.:.) :: (a -> b -> c) -> (d -> b) -> (a -> d -> c)
(f .:. g) x y = f x (g y)

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a, f b)
