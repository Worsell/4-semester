

		
		
list :: (Ord a, Read a, Show a) => [a]
list = []


addValueToSortedlist :: (Ord a, Read a, Show a) => a -> [a] -> [a]
addValueToSortedlist value (x:xs) = if (x < value) then  x : addValueToSortedlistHelp value xs else value:x:xs 


removeValueFromSortedList :: (Ord a, Read a, Show a) => a -> [a] -> [a]
removeValueFromSortedList value list = if ( head (filter (\x -> (x >= value)) list) == value) 
										then (filter (\x -> (x < value)) list)  ++ tail (filter (\x -> (x >= value)) list)
										else list

											

changeList :: (Ord a, Read a, Show a) => (a -> [a] -> [a]) -> [a] -> IO ()
changeList f list = do
			putStrLn "\n Please enter value"
			value <- getLine
			doLoop ( f (read value) list )

			
printLoop :: IO ()
printLoop = do
		putStrLn "0 - exit"
		putStrLn "1 - add value to sorted list"
		putStrLn  "2 - remove value from list"
		putStrLn "3 - print list"

printList :: (Ord a, Read a, Show a) => [a] -> IO ()
printList list = do
			print list
			doLoop list

doLoop :: (Ord a, Read a, Show a) => [a] -> IO ()
doLoop list = do
			printLoop
			command <- getLine
			case command of
				"0" -> return ()
				"1" -> changeList addValueToSortedlist list
				"2" -> changeList removeValueFromSortedList list
				"3" -> printList list
				_ -> doLoop list