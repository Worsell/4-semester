

{-# LANGUAGE DeriveFoldable #-}
data Foo a = Leaf a | Branch [Foo a] deriving(Show, Foldable)
order tree = foldr (:) [] tree