summNumberOfDigit number' l = summNumberOfDigit' (number', l, 0)
		
summNumberOfDigit' (number, list, k)
		| head list == number = (number, list ,k)
		| otherwise = summNumberOfDigit' (number, tail list, (k+1))