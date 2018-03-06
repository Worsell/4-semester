mod' n  = mod n 10
div' n = div n 10


summNumber n  |  length (show n) == 1 = n
			|  otherwise = div' n + (summNumber (mod' n)) 