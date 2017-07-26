
--PREPEND TO A LIST (let y = Cons 1 Null) or (let x = Null)
data Node a = Cons a (Node a) | Null deriving Show 

myHead :: Node a -> a
myHead Null = error "its empty"
myHead (Cons a _) = a

myLast :: Node a -> a
myLast Null = error "its empty"
myLast (Cons a Null) = a
myLast (Cons a b) = myTail b

addNode :: Node a -> Node a -> Node a 
addNode Null y = error "you cant add a empty value"
addNode x Null = x 
addNode x (Cons a Null) = Cons a x 
addNode x (Cons a b) = Cons a(addNode x b)

removeNode :: Eq a => a -> Node a -> Node a
removeNode x Null = Null
removeNode x (Cons a (Null))
    |a == x = Null
    |otherwise = Cons a (Null)
removeNode x (Cons a b)
    |a == x = b
    |otherwise = Cons a (removeNode x b)

addNodeHead :: Eq a => a -> Node a -> Node a
addNodeHead x Null = Cons x Null
addNodeHead x y = Cons x (y)
