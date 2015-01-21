module BinarySearchTree where

data KeyValuePair key value = KeyValuePair key value

instance (Eq key) => Eq (KeyValuePair key value) where
	(KeyValuePair x _) == (KeyValuePair y _) = x == y

instance (Ord key) => Ord (KeyValuePair key value) where
	(KeyValuePair x _) `compare` (KeyValuePair y _) = x `compare` y

data BinarySearchTree a = Empty | Leaf a | Node a (BinarySearchTree a) (BinarySearchTree a)

insert :: (Eq a, Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
insert element (Empty) = Leaf element
insert element (Leaf x)
	| element == x = error "Proba wstawienia tego samego klucza"
	| element < x = Node x (Leaf element) (Empty)
	| otherwise = Node x (Empty) (Leaf element)
insert element (Node x l r)
	| element == x = error "Proba wstawienia tego samego klucza"
	| element < x = Node x (insert element l) r
	| otherwise = Node x l (insert element r)

contains :: (Eq a, Ord a) => a -> BinarySearchTree (KeyValuePair a value) -> Bool
contains x (Empty) = False
contains x (Leaf (KeyValuePair key value)) = x == key
contains x (Node (KeyValuePair key value) l r)
	| x == key = True
	| x < key = contains x l
	| otherwise = contains x r
	
get :: (Eq a, Ord a) => a -> BinarySearchTree (KeyValuePair a value) -> value
get x (Empty) = error "Brak elementu w drzewie"
get x (Leaf (KeyValuePair key value))
	| x == key = value
	| otherwise = error "Brak element w drzewie"
get x (Node (KeyValuePair key value) l r)
	| x == key = value
	| x < key = get x l
	| otherwise = get x r
	
preorder (Empty) = []
preorder (Leaf kvp) = [kvp]
preorder (Node kvp l r) = [kvp] ++ (preorder l) ++ (preorder r)

create x = create' (Empty) x
create' tree [] = tree
create' tree (x:xs) = create' (insert x tree) xs