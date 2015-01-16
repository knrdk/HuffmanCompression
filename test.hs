import Data.Binary
import Data.ByteString.Lazy as BSL
import Control.Applicative       ((<$>),(<*>),(<|>))

stream = [0,1,1,1,0,1,1,0,1,0,1,0,0,1,0,0]

data Tree a = Empty | Leaf a

instance Show x => Show (Tree x) where
	show (Empty) = "[]"
	show (Leaf x) = "["++ show x ++"]"

putTree :: Binary a => Tree a -> Put
putTree (Empty) = do
	put False
putTree (Leaf a) = do
	put True
	put a
	
getTree :: Binary a => Get (Tree a)
getTree = do
	isLeaf <- get
	if isLeaf
		then Leaf <$> get
		else return Empty
		
instance Binary a => Binary (Tree a) where
	put = putTree
	get = getTree
	
main = do 
	t2 <- decodeFile "1.txt" :: IO (Tree Int)
	Prelude.putStrLn $ show t2