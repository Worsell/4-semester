-- поддерживаемые типы скобок {[()]}
-- ( )


parseStr ::[Char] -> Bool

parseStr string = parseHelp string []

parseHelp :: [Char] -> [Char] -> Bool
parseHelp string stack
	| (length string == 0) = length stack == 0
	| otherwise = case head string of
						'[' -> parseHelp (tail string) (head string : stack)
						'(' -> parseHelp (tail string) (head string : stack)
						'{' -> parseHelp (tail string) (head string : stack)
						']' -> if (head stack == '[') then parseHelp (tail string) (tail stack) else False
						')' -> if (head stack == '(') then parseHelp (tail string) (tail stack) else False
						'}' -> if (head stack == '{') then parseHelp (tail string) (tail stack) else False