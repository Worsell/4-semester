tail' l1
	| length l1 == 0 = []
	| otherwise = tail l1

head' l
	| length l == 0 = 0
	| otherwise = head l

function' (l1, l2, l3, r) 
	| (l1 == [] && l2 == [] && l3 == []) = ([],[],[], r)
	| otherwise  = function' (tail' l1, tail' l2, tail' l3, ((head' l1) + (head' l2) + (head' l3)) : r)


elementary (list1, list2, list3, r) = r


summOfList list11 list22 list33 = reverse (elementary (function' (list11, list22, list33 , []) ))