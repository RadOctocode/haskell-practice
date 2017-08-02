data Node a = Cons a (Node a) (Node a)| Null deriving (Show, Ord, Eq)
 --PREPEND TO A LIST (let y = Cons 1 Null) or (let x = Null)


emptyBin:: Node a -> Bool
emptyBin Null = True
emptyBin x = False

insert :: Ord a => a -> Node a -> Node a
insert a Null = Cons a (Null) (Null)
insert a (Cons x y z) 
		| a > x = Cons x (y) (insert a z)
		| a < x = Cons x (insert a y) (z)
		| a == x = error "you cant put in the same value"


--find the smallest thing in the right subtree
smallest :: Node a -> a
smallest (Cons a (Null) (Null)) = a 
smallest (Cons a (Null) (b)) = a
smallest (Cons a (b) (Null)) = smallest b
smallest (Cons a (b) (c)) = smallest b
smallest Null = error "can't get smallest of null"


remove :: Ord a => a -> Node a -> Node a
remove x (Cons a (Null) (Null))
    |a == x = Null
    |otherwise = (Cons a (Null) (Null))
remove x (Cons a (Null) (b))
    |a == x = b
    |a < x = (Cons a (Null) (remove x b)) 
    |otherwise = (Cons a (Null) (b))
remove x (Cons a (b) (Null))
    |a == x = b
    |a > x = (Cons a (remove x b) (Null))
    |otherwise = (Cons a (b) (Null))
remove x (Cons a (b) (c))
    |a == x = (Cons (smallest c) (b) (remove (smallest c) c) )
    |a > x = (Cons a (remove x b) (c))
    |a < x = (Cons a (b) (remove x c))
remove x Null = Null


inOrder :: Ord a => Node a -> [a]
inOrder (Cons a (Null) (Null)) = [a] 
inOrder (Cons a (b) (c))= inOrder b ++ [a] ++ inOrder c
inOrder (Cons a (b) (Null))=[a] ++ inOrder b
inOrder (Cons a (Null) (c))=[a] ++ inOrder c
inOrder Null = error "theres no order"
