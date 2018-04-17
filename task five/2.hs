import System.IO

generated n = [1..n] >>= (\x -> [x*y | y <-[1..n]])

--Òî æå ñàìîå, íî ğàçâ¸ğíóòî
generated' n = [1..n] >>= (\x -> ([1..n] >>= (\y -> return (x*y))))