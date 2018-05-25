-- Формат представления полинома таков
-- 5 4 3 2 1 0
--[a,b,c,d,e,f]
-- 21x^232+44x^15
-- 44x^232

{-
3 0
[5,4,3,2,1,0] 
[5,4,3] [2,1,0]
-}


derivative :: [Int] -> [Int]
derivative (x:[]) = []
derivative (x:xs) = ((length (x:xs) - 1) * x) : derivative xs



-- help for insert
-- берем n с конца элемент
f :: Int -> [Int] -> Int
f n list = last $ take (length list - n) list

-- берем список состоящий из (n+1) элемента с конца включительно
g  :: Int -> [Int] -> [Int]
g 0 list = [f 0 list]
g n list
	| n < 0 = []
	| n >= 0 = (f n list) : (g (n-1) list)


-- элемент, текущий элемент, всего элементов, список
insertHelper :: Int -> Int -> Int -> [Int] -> [Int]
insertHelper element n k list 
	| k == (length list - 1) = list
	| n == k = element : insertHelper element n (k-1) list
	| n /= k = 0 : insertHelper element n (k-1) list

{-

3 5 [5,4,3,2,1,0]






-}
	
-- end help for insert
insert :: Int -> Int -> [Int] -> [Int]
insert element n list 
	| n < (length list-1) = (take ((length list) -n -1 ) list) ++ [element] ++ g (n-1) list
	| n == (length list-1) = element : g (n-1) list
	| n > (length list-1) = insertHelper element n n list

	
	
	
-- Значение степень строка буфер
parseofMonom :: (Int, Int, [Char], [Char]) -> (Int, Int, [Char], [Char])
parseofMonom (a, b, [], []) = (a,b,[],[])
parseofMonom (a, b, [], c) = if (a == 0) then (read c :: Int, b, [], []) else (a, read c :: Int, [], [])
parseofMonom (a, b, (x:xs), tmp) = case x of
	'0' -> parseofMonom (a, b, xs, tmp ++ [x])
	'1' -> parseofMonom (a, b, xs, tmp ++ [x])
	'2' -> parseofMonom (a, b, xs, tmp ++ [x])
	'3' -> parseofMonom (a, b, xs, tmp ++ [x])
	'4' -> parseofMonom (a, b, xs, tmp ++ [x])
	'5' -> parseofMonom (a, b, xs, tmp ++ [x])
	'6' -> parseofMonom (a, b, xs, tmp ++ [x])
	'7' -> parseofMonom (a, b, xs, tmp ++ [x])
	'8' -> parseofMonom (a, b, xs, tmp ++ [x])
	'9' -> parseofMonom (a, b, xs, tmp ++ [x])
	'^' -> parseofMonom (if (tmp /= "") then (read tmp :: Int) else 1, b, xs, "")
	'-' -> parseofMonom (a, b, xs, tmp ++ [x])
	'x' -> parseofMonom (a, 1, xs, tmp)
	_ -> parseofMonom (a,b,xs, tmp)

	
fs :: (Int,Int,[Char],[Char]) -> Int
fs (x,y,z,k) = x
sd :: (Int,Int,[Char],[Char]) -> Int
sd (x,y,z,k) = y

-- Строка буфер=моном полином -> полином	
parseOfPolynomial :: [Char] -> [Char] -> [Int] -> [Int]
parseOfPolynomial [] tmp polynomial = (insert (fs $ parseofMonom (0, 0, tmp, "")) (sd $ parseofMonom (0, 0, tmp, "")) polynomial)
parseOfPolynomial (x:xs) tmp polynomial = case x of
	'-' -> parseOfPolynomial xs [x] (insert (fs $  parseofMonom (0, 0, tmp, "")) (sd $ parseofMonom (0, 0, tmp, "")) polynomial)
	'+' -> parseOfPolynomial xs "" (insert (fs $  parseofMonom (0, 0, tmp, "")) (sd $ parseofMonom (0, 0, tmp, "")) polynomial)
	_ -> parseOfPolynomial xs (tmp ++[x]) polynomial

sign :: Int -> Bool -> [Char]
sign x b 
	| b = ""
	| x < 0 = ""
	| x == 0 = ""
	| x > 0 = "+"
		 
polynomialToString :: [Int] -> Bool -> [Char]
polynomialToString [] _ = "0"
polynomialToString (x:[]) b = if (x /=0) then (sign x b) ++ show x else ""
polynomialToString (x:xs) b
	| x == 0 = polynomialToString xs False
	| (x > 0) = if ( x == 1 ) 
		then if (b) 
			then if (length (x:xs)-1) == 1
				then "x" ++ (polynomialToString xs False)
				else "x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
			else if (length (x:xs)-1) == 1
				then "+x" ++ (polynomialToString xs False)
				else "+x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
		else if (b) 
			then if (length (x:xs)-1) == 1
				then (show x) ++ "x" ++ (polynomialToString xs False)
				else (show x) ++ "x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
			else if (length (x:xs)-1) == 1
				then "+" ++ (show x) ++ "x" ++ (polynomialToString xs False)
				else "+" ++ (show x) ++ "x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False) 
	| (x < 0) = if ( x == (-1) ) 
		then if (b) 
			then if (length (x:xs)-1) == 1
				then "-x" ++ (polynomialToString xs False)
				else "-x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
			else if (length (x:xs)-1) == 1
				then "+x" ++ (polynomialToString xs False)
				else "+x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
		else if (b) 
			then if (length (x:xs)-1) == 1
				then (show x) ++ "x" ++ (polynomialToString xs False)
				else (show x) ++ "x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
			else if (length (x:xs)-1) == 1
				then (show x) ++ "x" ++ (polynomialToString xs False)
				else (show x) ++ "x^" ++ show (length (x:xs)-1) ++ (polynomialToString xs False)
	
task :: [Char] -> [Char]
task polynomial = polynomialToString (derivative $ parseOfPolynomial polynomial "" []) True 

