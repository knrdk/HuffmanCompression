module Util (
	sort
	, splitString
	) where

sort :: (Ord a)=>[a]->[a]
sort [] = []
sort (x:xs) = (sort [y|y<-xs,y<x]) ++ [x] ++ [y|y<-xs,y==x] ++ (sort [y|y<-xs,y>x])

{-START: splitString -}
splitString s sep = splitString' s sep ""	
			
splitString' "" sep start = (start,"")
splitString' (x:xs) sep start
	| x /= sep = splitString' xs sep (start++[x])
	|otherwise = (start,xs)	
{-END: splitString -}