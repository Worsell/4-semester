





evenNumbers1 list = length (filter (\x -> (mod x 2 == 0)) list) 

add ass x = if ( mod x 2 == 0 )
		then ass + 1
		else ass

evenNumbers2 list  = foldl add 0 list

evenNumbers3 list = sum(map (\x -> mod (x+1) 2) list)