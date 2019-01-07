data NestedList a = Elem a | List [NestedList a] deriving Show
--List[Elem 1, List [Elem 2]]


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
myFlatten (Elem z) = [z]
myFlatten (List (z:zs)) = myFlatten z ++ myFlatten (List zs)
--must use ++ instead of : otherwise it will throw a type error
myFlatten (List []) = []


myCompress :: String -> String
--type declaration for this cannot be String a -> String a because String is not a typeclass
--go through string if the next element is the same as this one then get rid of this pop this element
myCompress "" = ""
myCompress (x:y:xs)
    |x==y = myCompress(y:xs)  --edit me
    |otherwise = [x] ++myCompress(y:xs)
myCompress [a] = [a]

myPack :: Eq a => [a] -> [[a]]
myPack [] = []
myPack (x:xs) = pack' x [x] xs 

pack' :: Eq a => a -> [a] -> [a] -> [[a]]
pack' a b [] = [b]
pack' a b (x:xs) 
    |a==x =  pack' a (b++[x]) xs 
    |otherwise = (b : pack' x [x] xs)


myPackC :: Eq a => [a] -> [(Int,a)]
myPackC [] = []
myPackC (x:xs) = packc' x 1 xs

packc' :: Eq a => a -> Int -> [a] -> [(Int,a)]
packc' a b [] = [(b,a)]
packc' a b (x:xs) 
    |a==x =  packc' a (b+1) xs 
    |otherwise = ((b,a) : packc' x 1 xs)

--myAnnotate :: Eq a =>[a] -> [(Int,a)]
--myAnnotate []=[]
--myAnnotate (x:xs) = [(1,x)] ++ myAnnotate xs

--myCount :: Eq a => [(Int,a)] -> [(Int,a)]
--myCount [] = []
--myCount ((x,a):(y,b):xs)
--    | a==b = [(x+y,a)] ++ myCount ((x+y,a):xs) 
--    | otherwise = [(x,a),(y,b)] ++ myCount (xs)
