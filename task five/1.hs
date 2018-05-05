--     1 2 3 4 5 6 7
--7 = (1,1,1,1,1,1,1)
--7 = (2,1,1,1,1,1,0)
--7 = (2,2,1,1,1,0,0)
--7 = (3,1,1,1,1,0,0)
--7 = (3,2,1,1,0,0,0)
--7 = (3,3,1,0,0,0,0)
--7 = (4,1,1,1,0,0,0)
--7 = (4,2,1,0,0,0,0)
--7 = (4,3,0,0,0,0,0)
--7 = (5,1,1,0,0,0,0)
--7 = (5,2,0,0,0,0,0)
--7 = (6,1,0,0,0,0,0)
--7 = (7,0,0,0,0,0,0)

-- Ну вроде красиво выводит, только сами функции вывода рекурсивные.
getCombinaton :: Int -> [[Int]]
getCombinaton 0 = [[]]
getCombinaton n = do
	x <- [1..n]
	xs <- getCombinaton(n-x)
	[x:xs]

-- Красивый вывод без фильтра.
goodPrint :: [[Int]] -> [Char]
goodPrint [] = []
goodPrint (x:xs) = (goodPrinthelper x) ++ "\n" ++ goodPrint xs	

goodPrinthelper :: [Int] -> [Char]
goodPrinthelper (x:xs) = (show x) ++ goodPrinthelper' xs

goodPrinthelper' :: [Int] -> [Char]
goodPrinthelper' [] = []
goodPrinthelper' (x:xs) = "+" ++ (show x) ++ goodPrinthelper' xs

--Красивый вывод с повторениям.
gp :: [[Int]] -> [Char]
gp [] = []
gp (x:xs) = (gph' x 0) ++ "\n" ++ gp xs 


gph' :: [Int] -> Int -> [Char]
gph' [] n = []
gph' (x:xs) n = if (x == 0) 
	then "" ++ gph' xs (n+1) 
	else (p n x) ++ gph' xs (n+1)

p :: Int -> Int -> [Char]
p k 0 = []
p k n = "+" ++ show k ++ (p k (n-1))

-- Число размерность
getEvector :: Int -> Int -> [Int]
getEvector _ 0 = []
getEvector 0 y = 1 : getEvector (-1) (y-1)
getEvector x y = 0 : getEvector (x-1) (y-1)

-- Два e вектора
fromEvector :: [Int] -> [Int] -> [Int]
fromEvector [] [] = [] 
fromEvector (x:xs) (y:ys) = (x+y) : fromEvector xs ys

-- Раскладывает сумму по базису 0..n
getVector :: [Int] -> Int -> [Int]
getVector [] n = [0 | x <- [0..n]]
getVector (x:xs) n = fromEvector (getEvector x (n+1)) (getVector xs n)

-- Проверка на то, один ли это вектор
isEquivalent :: [Int] -> [Int] -> Bool
isEquivalent [] [] = True
isEquivalent _ [] = False
isEquivalent (x:xs) (y:ys) = if (x==y) then isEquivalent xs ys else False

-- Проверка на то, нужно ли добавлять вектор в список на печать
needToAdd :: [Int] -> [[Int]] -> Bool -> Bool
needToAdd _ [] c = (not c)
needToAdd x (y:ys) c = needToAdd x ys (c || (isEquivalent x y))

list :: [[Int]]
list = [[]]

getPrintCombination :: [[Int]] -> [[Int]] -> [[Int]]
getPrintCombination [] c = c
getPrintCombination (x:[]) y = if (needToAdd (getVector x (sum x)) y False) 
	then getPrintCombination [] ((getVector x (sum x)) : y) 
	else getPrintCombination [] y
getPrintCombination (x:xs) y = if (needToAdd (getVector x (sum x)) y False) 
	then getPrintCombination xs ((getVector x (sum x)) : y) 
	else getPrintCombination xs y
