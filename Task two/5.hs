added l1
	| length l1 == 0 = []
	| otherwise = tail l1

added' l
	| length l == 0 = 0
	| otherwise = head l

function' (l1, l2, l3, r) 
	| (l1 == [] && l2 == [] && l3 == []) = ([],[],[], r)
	| otherwise  = function' (added l1, added l2, added l3, ((added' l1) + (added' l2) + (added' l3)) : r)


e (l1,l2,l3,r) = r


function l11 l22 l33 = reverse (e (function' (l11, l22, l33, [])))