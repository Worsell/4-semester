

summNumber n  |  length (show n) == 1 = n
			|  otherwise = (div n 10) + (summNumber (mod n 10)) 