-- поддерживаемые типы скобок {[()]}


parseStr string = parse string (0, 0, 0, True) 
				where parse (l:ls) (x, y, z, b)
					| length ls == 0 = finalControl (check l (x, y, z, b))
					| otherwise = parse ls (check l (x, y, z ,b))

check l (x, y, z, b) 
		| not b = (x,y,z,b)
		| otherwise = case l of
						']' -> if (x-1) < 0 then (x-1, y, z, False) else (x-1, y, z, b)
						')' -> if (y-1) < 0 then (x, y-1, z, False) else (x, y-1, z, b)
						'}' -> if (z-1) < 0 then (x, y, z-1, False) else (x, y, z-1, b)
						'[' -> (x+1, y, z, b)
						'(' -> (x, y+1, z, b)
						'{' -> (z, y, z+1, b)
						
finalControl (x, y, z, b)
			| x /= 0 = False
			| y /= 0 = False
			| z /= 0 = False
			| otherwise = b