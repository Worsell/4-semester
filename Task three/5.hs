fibonaci :: Integer -> Integer
fibonaci number = fibonaci' number 0 1 
							where fibonaci' n one two   | n > 0 = fibonaci' (n-1) (two+one) one
														| n < 0 = fibonaci' (n+1) two  (one-two) 
														| n == 0 = one