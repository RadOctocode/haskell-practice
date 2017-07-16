
data Node a = Cons a (Node a) | Null deriving Show --PREPEND TO A LIST (let y = Cons 1 Null) or (let x = Null)

myHead :: Node a -> a
myHead Null = error "its empty"
myHead x = case x of 
	                (Cons a (_)) -> a

myTail:: Node a -> a
myTail Null = error "its empty"
myTail x = case x of 
					(Cons a (Null)) -> a
					(Cons a (b)) -> myTail b

addNode:: Node a -> Node a -> Node a 
--go through the list similar to myTail and add the node
-- add to end
--x is node to add y is list to add too 
addNode Null y = error "you cant add a empty value"
addNode x Null = x 
addNode x y = case x y of
						