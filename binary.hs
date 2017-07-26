data Node a = Cons a (Node a) (Node a)| Null deriving (Show, Ord, Eq) --PREPEND TO A LIST (let y = Cons 1 Null) or (let x = Null)
emptyBin:: Node a -> Bool
emptyBin Null = True
emptyBin x = False

insert :: Ord a => a -> Node a -> Node a
insert a Null = Cons a (Null) (Null)
insert a (Cons x y z) 
		| a > x = Cons x (y) (insert a z)
		| a < x = Cons x (insert a y) (z)
		| a == x = error "you cant put in the same value"

