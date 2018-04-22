findPhone :: (Int -> Bool) -> [(Int,[Char])] -> (Int, [Char])
findPhone _ [] = (-1, "Not find")
findPhone f ((x,y) : xs) = if (f x) then (x,y) else findPhone f xs

findName :: ([Char] -> Bool) -> [(Int,[Char])] -> (Int, [Char])
findName _ [] = (-1, "Not find")
findName f ((x,y) : xs) = if (f y) then (x, y) else findName f xs

getString :: [(Int, [Char])] -> [Char]
getString [] = ""
getString ((x,y) : xs) = (show x) ++ " " ++ y ++ "\n" ++ getString xs 

parseString :: [Char] -> [(Int, [Char])]
parseString [] = []
parseString s = (read (fst $ pSH "" s) :: Int , fst $ pSH "" $ snd $ pSH "" s) : (parseString $ pSH2 s)

pSH2 :: [Char] -> [Char]
pSH2 s = snd $ pSH "" $ snd $ pSH "" s

pSH :: [Char] -> [Char] -> ([Char] , [Char])
pSH tmp (x:xs) = case x of 
	'\n' -> (tmp, xs)
	' ' -> (tmp, xs)
	_ -> pSH (tmp ++ [x]) xs
		
-- Файл должен заканчиваться \n
		
doLoop :: [(Int, [Char])] -> IO()
doLoop list = do
	print "0 - exit"
	print "1 - add data"
	print "2 - find the phone on the name"
	print "3 - find the name on the phone"
	print "4 - save data in file"
	print "5 - read data from file"
	command <- getLine
	case command of
		"0" -> do 
			return ()
		"1" -> do 
			print "input phone"
			phone <- getLine
			print "input name"
			name <- getLine
			list1 <- return ((read phone :: Int, name) : list)
			print list1
			doLoop list1
		"2" -> do 
			print "input name"
			name <- getLine
			phone <- return (fst $ findName (\x -> x == name) list)
			print phone
			doLoop list			
		"3" -> do 
			print "input phone"
			phone <- getLine
			name <- return (snd $ findPhone (\x -> x == (read phone :: Int)) list)
			print name
			doLoop list
		"4" -> do 
			print "input filename for save"
			filename <- getLine
			writeFile filename $ getString list
			doLoop list
		"5" -> do 
			print "input filename for read"
			filename <- getLine
			string <- readFile filename
			list1 <- return $ parseString string
			print list1
			doLoop list1
		_ -> doLoop list