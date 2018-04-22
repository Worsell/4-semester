-- :set -XderiveFoldable не работает при загрузке из файла, потому пишу тот код, который надо вбить в консоль.

Prelude> :set -XDeriveFoldable
Prelude> data Foo a = Leaf a | Branch [Foo a] deriving(Show, Foldable)
Prelude> order = foldr (:) []
-- Пример вызова
Prelude> order $ Branch[Leaf 2, Leaf 3, Branch[Leaf 4, Leaf 5, Leaf 6]
[2,3,4,5,6]