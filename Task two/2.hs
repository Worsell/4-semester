f n  = [2^^x |x <-[1..n]]

function n = d (function' (2^^n, []))

d (n,l) = l

function' (n, l)
	| n == 1.0 = (0,(1:l))
	| otherwise = function' ((n/2), (n:l))