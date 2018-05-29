import System.Random
import System.IO.Unsafe  -- be careful!                                         


randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]


task list = randomIO >>= (\x -> (print $ take (length list) (randomList x)))

