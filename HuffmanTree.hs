module HuffmanTree where

import Util(sort)

data HuffmanTree a = Leaf a Int | Node (HuffmanTree a) (HuffmanTree a) Int deriving (Show)

instance Eq (HuffmanTree a) where
	x == y = (weight x) == (weight y)

instance Ord (HuffmanTree a) where
	x `compare` y = (weight x) `compare` (weight y)

weight :: HuffmanTree a -> Int
weight (Leaf _ w) = w
weight (Node _ _ w) = w

merge :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
merge x y = Node x y (weight x + weight y)

buildTree :: [HuffmanTree a] -> HuffmanTree a
buildTree = buildTree' . sort

buildTree' :: [HuffmanTree a] -> HuffmanTree a
buildTree' (x:[]) = x
buildTree' (x:y:[]) = merge x y
buildTree' (x:y:xs) = buildTree ((merge x y):xs) 