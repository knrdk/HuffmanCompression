import Data.ByteString.Lazy as BS (readFile, writeFile, unpack, pack, hPut)
import Data.Word
import Data.Bits
import System.IO as SIO
import BinarySearchTree as BST (get, insert, preorder, BinarySearchTree(Empty), KeyValuePair(KeyValuePair))

main = encode

makeCodemap b = do
	return $ getCodemap b
	
makeCompressedData b codemap = do
	return $ compress b codemap
	
encode = do
	b <- fileToWordList "foo.txt"
	codemap <- makeCodemap b	
	compressed <- makeCompressedData b codemap
	fhandleOut <- openFile "bar.txt" WriteMode	
	BS.hPut fhandleOut $ bitArrayToByteString compressed
	hClose fhandleOut	
	fhCm <- openFile "cm.txt" WriteMode
	hPutStrLn fhCm $ show $ length compressed
	writeCodemap fhCm codemap
	hClose fhCm
	
	
writeCodemap fhandle codemape = do
	hPutStr fhandle $ unlines $ map showCode (convertTreeToCodemap codemape)
	
decode = do
	fhandleCm <- openFile "cm.txt" ReadMode
	length <- hGetLine fhandleCm
	codemape <- readCodemape fhandleCm
	hClose fhandleCm
	bits <- fileToBitArray "bar.txt"
	BS.writeFile "decoded.txt" $ BS.pack $ dcd (take (read length :: Int) bits) codemape

dcd = dcd' 0

dcd' n [] codemap = []	
dcd' n bits codemap = if containsCode (take n bits) codemap 
	then (getWordForCode (take n bits) codemap):(dcd' 1 (drop n bits) codemap)
	else dcd' (n+1) bits codemap
	
containsCode code [] = False
containsCode code (c:cs)
	| code == snd(c) = True
	| otherwise = containsCode code cs

getWordForCode code [] = error "Brak podanego kodu"
getWordForCode code (c:cs)
	| code == snd(c) = fst(c)
	| otherwise = getWordForCode code cs
	
{-START: readCodemap -}	
readCodemape fhandle = do
	eof <- hIsEOF fhandle
	if eof then return []
	else
		do
			line <- hGetLine fhandle
			if ((length line) < 2) then return [] 
			else 
				do
					x <- readCodemape fhandle
					return $ (codeStringToCode line):x
			--else return (codeStringToCode line) ++ 

codeStringToCode cs = (stringToWord8 (fst (splitString cs '=')), stringToBitArray (snd (splitString cs '=')))
			
splitString' "" sep start = (start,"")
splitString' (x:xs) sep start
	| x /= sep = splitString' xs sep (start++[x])
	|otherwise = (start,xs)		
{-END: readCodemap -}
			
compress [] codemap = []
compress (x:xs) codemap = (getCode x codemap)  ++ (compress xs codemap)

splitString s sep = splitString' s sep ""


---
	
data Bit = Zero | One

instance Show Bit where
	show One = "1"
	show Zero = "0"

instance Eq Bit where
	One == One = True
	One == Zero = False
	Zero == Zero = True
	Zero == One = False
	
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
	where bitArrayToByte x = take 8 (x++zeroes)

byteToWord8' :: Int -> [Bit] -> Word8
byteToWord8' (-1) _ = clearBit (bit 0) 0
byteToWord8' n (One:xs) = setBit (byteToWord8' (n-1) xs) n
byteToWord8' n (Zero:xs) = byteToWord8' (n-1) xs

---START: stringToWord8

intToBitArray :: Int -> [Bit]
intToBitArray 0 = [Zero]
intToBitArray 1 = [One]
intToBitArray n = (intToBitArray' (div n 2)) ++ (intToBitArray (mod n 2))

intToBitArray' :: Int -> [Bit]
intToBitArray' 0 = []
intToBitArray' n = intToBitArray n

intToWord8 :: Int -> Word8
intToWord8 = byteToWord8 . addZeroesAtStart . intToBitArray
	where addZeroesAtStart x = (take (8 - length x) zeroes) ++ x

stringToWord8 :: String -> Word8
stringToWord8 x = intToWord8 (read x :: Int)
---END: stringToWord8

stringToBitArray :: String -> [Bit]
stringToBitArray [] = []
stringToBitArray ('1':xs) = One : (stringToBitArray xs)
stringToBitArray ('0':xs) = Zero : (stringToBitArray xs)

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

getCodemap = convertCodemapToTree . buildCodemapFromTree . buildTree . getHuffmanTrees

convertCodemapToTree codemap = convertCodemapToTree' (BST.Empty) codemap
convertCodemapToTree' tree [] = tree
convertCodemapToTree' tree (c:cs) = convertCodemapToTree' (insert (KeyValuePair (fst c) (snd c)) tree) cs

convertTreeToCodemap tree = map project (preorder tree)
	where project (KeyValuePair key value) = (key,value)

getCode :: (Eq key, Ord key) => key -> BinarySearchTree (KeyValuePair key value)  -> value
getCode key tree = get key tree
	
showCode :: (Show a) => (a,[Bit]) -> String
showCode (word,code) = (show word)++"="++(showBitArray code)

showBitArray :: [Bit] -> String
showBitArray [] = ""
showBitArray (x:xs) = (show x) ++ showBitArray xs

-----
