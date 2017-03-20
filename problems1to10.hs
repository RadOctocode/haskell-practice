myLast :: [a] -> a --do not add spaces they will assume you need a let
myLast [] = error "empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

myNextToLast :: [a] -> a
myNextToLast [] = error "empty list!"
myNextToLast [x] = error "list with one element" 
myNextToLast (_:xs) = myNextToLast xs