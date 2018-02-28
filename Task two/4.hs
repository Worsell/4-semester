function n l = function' (n, l, 0)
		
function' (n, l, k)
		| head l == n = (n ,l ,k)
		| otherwise = function' (n, tail l, (k+1))