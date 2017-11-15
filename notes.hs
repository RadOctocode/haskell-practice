--fizzBuzz xs = [if x `mod` 3==0 then "FIZZ" else if x `mod` 5==0 then "BUZZ" | x <- xs]
boomBangs xs = [if x `mod` 3==0 && x `mod` 5==0 then "boombang" 
                 else 
				  if x `mod` 5==0 then "bang"
				 else
				  if x `mod` 3==0 then "boom"
				 else show x
sayMe :: (Integral a) => a -> String
--integral is a typeclass 
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" --x is a base case where its for anything that is not accounted for 

charName :: Char -> String  
--checks if type is a char
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil" 
charName x = "not a char I know"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
--the last arrow leading to things is the return 
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

bmiTell :: (RealFloat a) => a -> a -> String
--guards sub for if/else only evaluate a thing 
--above the pipeline the function name and parameters  
bmiTell weight height  
    | bmi<= skinny = "You're underweight, you emo, you!"  
    | bmi<= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi<= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise  = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2 
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
          --(skinny, normal, fat)=(18.5, 25.0, 30.0) 
          --pattern matching 

initials :: String -> String -> String --strings as parameters string return 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname --make f the variable name of the first letter in the first name.
          (l:_) = lastname  --make l the variable name of the first letter in the last name. 

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--RealFloat is a typeclass  
--parameter a tuple of realfloats
--return a realfloat
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 

cylinderSurface :: (RealFloat a) => a -> a -> a 
--2 real float parameters
--realFloat return 
cylinderSurface r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

cubeSurface::(Integral a)=> a -> a
cubeSurface x=(x^2)*6

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 
    --let <bindings> in <expression>

maximum' :: (Ord a) => [a] -> a  --a is an ordered data type it takes a list of ordered data types and returns one of them
maximum' [] = error "maximum of empty list"  --cant call max on empty list 
maximum' [x] = x  --the max of a single elemental list is that single element
maximum' (x:xs)
--for every other case let x = the beginning of the list   
    | x > maxTail = x --if x is bigger then maxtail then respond with x
    | otherwise = maxTail  
    where maxTail = maximum' xs  --recursion happens here


minimum::(Ord a)=> [a]-> a
minimum' [] = error "min of empty list"
minimum' [x] = x
minimum' (x:xs)
    |x < minTail = x
    | otherwise = minTail
    where minTail = minimum' xs

