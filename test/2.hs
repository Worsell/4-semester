matrix :: Integer -> [[Integer]]
matrix n = map (\x -> matrix' 1 x n) [1..n]

matrix' a x n
	| a <= x = x : matrix' (a+1) x n
	| a == (n+1) = []
	| otherwise = a : matrix' (a + 1) x n