first (x,_,_) = x

findFirstPositionMaxSumm :: [Int] -> Int
findFirstPositionMaxSumm list = first $ foldl find (0, head list, 0) list where 
		find (position, summ, base) next = if newSumm > summ
					then (position+1, newSumm, next)
					else (position, summ, next) where
					newSumm = base + next