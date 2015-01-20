import Data.ByteString.Lazy as BS (readFile, writeFile, unpack, pack, hPut)
import Data.Word
import Data.Bits
import System.IO

main = encode

copy = do	
	b <- fileToBitArray "foo.txt"
	BS.writeFile "bar.txt" $ bitArrayToByteString b		

makeCodemap b = do
	return $ getCodemap b
	
makeCompressedData b = do
	cm <- makeCodemap b
	return $ compress b cm
	
encode = do
	b <- fileToWordList "foo.txt"
	compressed <- makeCompressedData b
	fhandleOut <- openFile "bar.txt" WriteMode
	hPutStrLn fhandleOut $ show $ length compressed
	BS.hPut fhandleOut $ bitArrayToByteString compressed
	hClose fhandleOut

compress [] codemap = []
compress (x:xs) codemap = (getCode x codemap)  ++ (compress xs codemap)
---
	
data Bit = Zero | One

instance Show Bit where
	show One = "1"
	show Zero = "0"

getBit :: Bool -> Bit
getBit False = Zero
getBit True = One

zeroes = Zero : zeroes
		
-----
	
fileToBitArray :: String -> IO [Bit]
fileToBitArray fp = do
	contents <- fileToWordList fp
	return $ concat $ map word8ToBitArray contents
	
fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ unpack contents
		
word8ToBitArray :: Word8 -> [Bit]	
word8ToBitArray = word8ToBitArray' 7
	
word8ToBitArray' :: Int -> Word8 -> [Bit]
word8ToBitArray' (-1) x = []
word8ToBitArray' i y = (getBit $ testBit y i) : (word8ToBitArray' (i-1) y)

-----

bitArrayToByteString x = BS.pack $ bitArrayToWord8Array x

bitArrayToWord8Array :: [Bit] -> [Word8]
bitArrayToWord8Array x = map byteToWord8 $ chop 8 x

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

byteToWord8 :: [Bit] -> Word8
byteToWord8 = (byteToWord8' 7) . bitArrayToByte

byteToWord8' :: Int -> [Bit] -> Word8
byteToWord8' (-1) _ = clearBit (bit 0) 0
byteToWord8' n (One:xs) = setBit (byteToWord8' (n-1) xs) n
byteToWord8' n (Zero:xs) = byteToWord8' (n-1) xs

bitArrayToByte :: [Bit] -> [Bit]
bitArrayToByte x = take 8 (x++zeroes)

-----

getHuffmanTrees :: [Word8] -> [HuffmanTree Word8]
getHuffmanTrees = (map getHuffmanTree) . getFrequencies 

getHuffmanTree :: (Word8, Int) -> HuffmanTree Word8
getHuffmanTree (x,weight) = Leaf x weight

getFrequencies :: (Eq a, Ord a) => [a] -> [(a,Int)]
getFrequencies = getFrequencies' . sort

getFrequencies' :: (Eq a) => [a] -> [(a,Int)]
getFrequencies' [] = []
getFrequencies' (w:ws) = getFrequencies'' ws (w,1)

getFrequencies'' :: (Eq a) => [a] -> (a,Int) -> [(a,Int)]
getFrequencies'' [] current = [current]
getFrequencies'' (w:ws) current
	| w == fst current = getFrequencies'' ws (w, ((snd current)+1))
	| otherwise = [current] ++ (getFrequencies'' ws (w,1))


sort :: (Ord a)=>[a]->[a]
sort [] = []
sort (x:xs) = (sort [y|y<-xs,y<x]) ++ [x] ++ [y|y<-xs,y==x] ++ (sort [y|y<-xs,y>x])	

-----

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

buildCodemapFromTree :: HuffmanTree a -> [(a,[Bit])]
buildCodemapFromTree (Leaf x w) = [(x,[])]
buildCodemapFromTree (Node left right w) = (addToEach [Zero] (buildCodemapFromTree left)) ++ (addToEach [One] (buildCodemapFromTree right))
	where addToEach x = map (\y->(fst y, x ++ snd y))

getCodemap = buildCodemapFromTree . buildTree . getHuffmanTrees
	
getCode :: (Eq a) => a -> [(a,[Bit])] -> [Bit]
getCode _ [] = []
getCode a (x:xs)
	| a == fst x = snd x
	|otherwise = getCode a xs
	
showCode :: (Show a) => (a,[Bit]) -> String
showCode (word,code) = (show word)++"="++(showBitArray code)

showBitArray :: [Bit] -> String
showBitArray [] = ""
showBitArray (x:xs) = (show x) ++ showBitArray xs

-----
