
thread :: [Integer]
thread = 1 : 7 : 9 : thread1

thread1 :: [Integer]
thread1 = concatMap (\x -> [x+1, x+7, x+9]) $ thread2

thread2 :: [Integer]
thread2 = map (*10) thread
