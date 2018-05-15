import System.Random
import System.IO.Unsafe  -- be careful!                                         


newRand = randomIO :: IO Int
c :: Int
c = unsafePerformIO (newRand)

randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]

task :: [Int] -> [Int]

task list = take (length list) (randomList c)

