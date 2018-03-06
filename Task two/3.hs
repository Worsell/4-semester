f n  = mod n 10
g n = div n 10

function n  |  length (show n) == 1 = n
			|  otherwise = f n + (function (g n)) 