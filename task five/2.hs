import System.IO

generated n = [1..n] >>= (\x -> [x*y | y <-[1..n]])

--То же самое, но развёрнуто
generated' n = [1..n] >>= (\x -> ([1..n] >>= (\y -> return (x*y))))