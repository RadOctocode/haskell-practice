
data Node a = Cons a (Node a) | Null deriving Show --PREPEND TO A LIST (let y = Cons 1 Null) or (let x = Null)

myHead :: Node a -> a
myHead Null = error "its empty"
myHead x = case x of 
	                Cons a (_) -> a

myTail:: Node a -> a
myTail Null = error "its empty"
myTail x = case x of 
					Cons a (Null) -> a
					Cons a (b) -> myTail b


addNode:: Node a -> Node a -> Node a 
addNode Null y = error "you cant add a empty value"
addNode x Null = x 
addNode x y = case y of
					Cons a Null -> Cons a x 
					Cons a b -> Cons a(addNode x b)

removeNode:: Eq a => a -> Node a -> Node a
removeNode x y = case y of
					Null -> Null
					Cons a (Null) 
						|a == x -> Null
						|otherwise -> Cons a (Null)
					Cons a b
						|a == x -> b
						|otherwise -> Cons a (removeNode x b)

addNodeHead:: Node a -> Node a -> Node a
addNodeHead Null y = error "you cant add an empty value"
addNodeHead x Null = x
addNodeHead x y =
