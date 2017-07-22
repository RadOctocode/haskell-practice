data NestedList a = Elem a | List [NestedList a] deriving Show
--[Elem 1, List [Elem 2]]


myLast :: [a] -> a --do not add spaces they will assume you need a let
myLast [] = error "empty list!"
myLast [x] = x
myLast (_:xs) = myLast xs

myNextToLast :: [a] -> a
myNextToLast = last . init
-- init returns list w/o last item
--last takes the last part 
-- . function composition

myIndex :: [a] -> Int -> a
myIndex list i = list!!(i-1) --list is the name of the list and i is the name of the int

myReverse :: [a] -> [a]
myReverse list = reverse list

myLength :: [a] -> Int
myLength list = length list 

myAnyKindPal :: (Eq a) => [a] -> Bool--
myAnyKindPal s1
	| s1 == rs2 = True
	| otherwise = False
	where rs2 = reverse s1

myFlatten :: NestedList a -> [a]
myFlatten x = case x of
                   -- List [b] -> myFlatten b
                    [Elem b] -> [b]

