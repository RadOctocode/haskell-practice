data Elem a = Single a | Multiple Int a deriving Show


myPackCE :: Eq a => [a] -> [Elem a]
myPackCE [] = []
myPackCE (x:xs) = packc x 1 xs

packc :: Eq a => a -> Int -> [a] -> [Elem a]
packc a b [] 
    |b==1 = [Single a]
    |otherwise = [Multiple b a]
packc a b (x:xs)
    |a==x = packc a (b+1) xs
    |a/=x && b==1 = (Single a : packc x 1 xs) 
    |otherwise = (Multiple b a : packc x 1 xs)

myUnpackCE :: Eq a => [Elem a] -> [a]
myUnpackCE [] = []
myUnpackCE ((Single a): xs) = [a] ++ myUnpackCE xs
myUnpackCE ((Multiple b a ): xs) = (replicate b a) ++ myUnpackCE xs

myDup :: [a] -> [a]
myDup [] = []
myDup [a] = [a] ++ [a]
myDup (x:xs) = [x]++[x] ++ myDup xs

myRep :: [a] -> Int -> [a]
myRep [] b = []
myRep (x:xs) b = replicate b x ++ myRep xs b 

myDrop :: [a] -> Int -> [a]
myDrop [] b = []
myDrop a 1 = []
myDrop a b = drop' a b 1

drop' :: [a] -> Int -> Int -> [a]
drop' [] b c = []
drop' (x:xs) b c
    | b==c = drop' xs b 1
    | otherwise = [x] ++ drop' xs b (c+1)

mySplit :: [a] -> Int -> [[a]]
mySplit a b = let k = take b a
                  in [k,diff a b]

diff :: [a] -> Int -> [a]
diff [] y  = []
diff (x:xs) y
   | y==0 = (x:xs)
   | otherwise = diff xs (y - 1)

mySlice :: [a] -> Int -> Int -> [a]
mySlice x y z = drop y . take z $ x

myRotate :: [a] -> Int -> [a]
myRotate a b
   | c > 0 = turnRight a c
   | c < 0 = turnLeft a c
   | c == 0 = a 
   where
     c = b % length $ a
turnLeft :: [a] -> Int -> [a]
turnLeft a 0 = a
turnLeft a b = turnLeft tail a ++ head a (b - 1)


turnRight :: [a] -> Int -> [a]
turnRight a 0 = a
turnRight a b = a

