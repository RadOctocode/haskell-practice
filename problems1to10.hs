myLast :: [a] -> a --do not add spaces they will assume you need a let
myLast [] = error "empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

myNextToLast :: [a] -> a
myNextToLast = last . init
-- init returns list w/o last item
--last takes the last part 
-- . function composition

isPalindrome :: String -> Bool
isPalindrome s1
	| s1 == rs2 = True
	| otherwise = False
	where rs2 = reverse s1
	--use where to 
