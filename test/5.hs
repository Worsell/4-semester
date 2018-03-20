filter' :: (a -> Bool) -> ([a] -> [a])
filter' = \p -> \xs -> case xs of
    []     -> []
    (x:xs) -> let rest = filter' p xs
              in  if   p x
                  then x : rest
                  else rest

