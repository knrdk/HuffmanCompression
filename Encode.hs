import Data.ByteString.Lazy as BS (hPut)
import Data.Word (Word8())
import System.IO (openFile, hClose, hPutStr, hPutStrLn, IOMode(WriteMode))
import System.Environment (getArgs)

import Util (sort)
import HuffmanTree
import BinarySearchTree as BST (get, insert, preorder, BinarySearchTree(Empty), KeyValuePair(KeyValuePair))
import Bit (bitArrayToByteString, Bit(Zero,One))
import LowLevelIO (fileToWordList)

main = do
	(a:b:_) <- getArgs
	encode a b
	print $ "Plik " ++ a ++ " skompresowano do " ++ b ++ ".dat, " ++ b ++ ".cod"

encode inputPath outputName = do
	inputBits <- fileToWordList inputPath
	codemap <- makeCodemap inputBits	
	compressed <- compressData inputBits codemap
	fhOut <- openFile (outputName++".dat") WriteMode	
	BS.hPut fhOut $ bitArrayToByteString compressed
	hClose fhOut	
	fhCm <- openFile (outputName++".cod") WriteMode
	hPutStrLn fhCm $ show $ length compressed
	writeCodemap fhCm codemap
	hClose fhCm

makeCodemap inputBits = do
	return $ getCodemap inputBits
	
compressData inputBits codemap = do
	return $ compress inputBits codemap

writeCodemap fhandle codemape = do
	hPutStr fhandle $ unlines $ map showCode (convertTreeToCodemap codemape)
	
compress x codemap = concat $ map (getCode codemap) x
	
{-START: getHuffmanTrees-}
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
{-END: getHuffmanTrees-}

{-START: getCodemap-}
getCodemap = convertCodemapToTree . buildCodemapFromTree . buildTree . getHuffmanTrees

buildCodemapFromTree :: HuffmanTree a -> [(a,[Bit])]
buildCodemapFromTree (Leaf x w) = [(x,[])]
buildCodemapFromTree (Node left right w) = (addToEach [Zero] (buildCodemapFromTree left)) ++ (addToEach [One] (buildCodemapFromTree right))
	where addToEach x = map (\y->(fst y, x ++ snd y))

convertCodemapToTree codemap = convertCodemapToTree' (BST.Empty) codemap
convertCodemapToTree' tree [] = tree
convertCodemapToTree' tree (c:cs) = convertCodemapToTree' (insert (KeyValuePair (fst c) (snd c)) tree) cs

convertTreeToCodemap tree = map project (preorder tree)
	where project (KeyValuePair key value) = (key,value)

{-END: getCodemap -}
	
getCode :: (Eq key, Ord key) => BinarySearchTree (KeyValuePair key value) -> key -> value
getCode tree key = get key tree
	
{-START: showCode -}	
showCode :: (Show a) => (a,[Bit]) -> String
showCode (word,code) = (show word)++"="++(showBitArray code)

showBitArray :: [Bit] -> String
showBitArray [] = ""
showBitArray (x:xs) = (show x) ++ showBitArray xs
{-END: showCode -}	