import Data.ByteString.Lazy as BS (writeFile, pack)
import System.IO
import System.Environment (getArgs)

import Util (splitString)
import BinarySearchTree as BST (create, contains, get, KeyValuePair(KeyValuePair))
import Bit (stringToWord8, stringToBitArray)
import LowLevelIO
	
main = do
	(a:b:_) <- getArgs
	decode a b
	print $ "Wypakowano do " ++ b
	
decode inputName outputPath = do
	fhandleCm <- openFile (inputName++".cod") ReadMode
	length <- hGetLine fhandleCm
	codemape <- readCodemape fhandleCm
	hClose fhandleCm
	bits <- fileToBitArray (inputName++".dat")
	BS.writeFile outputPath $ BS.pack $ dcd (take (read length :: Int) bits) (create (convertCodemapeToKeyValuePair codemape))

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
{-END: readCodemap -}
	
{-START: dcd-}
dcd = dcd' 0

dcd' n [] codemap = []	
dcd' n bits codemap = if contains (take n bits) codemap 
	then (get (take n bits) codemap):(dcd' 1 (drop n bits) codemap)
	else dcd' (n+1) bits codemap
	
{-END: dcd-}
		
containsCode code [] = False
containsCode code (c:cs)
	| code == snd(c) = True
	| otherwise = containsCode code cs

getWordForCode code [] = error "Brak podanego kodu"
getWordForCode code (c:cs)
	| code == snd(c) = fst(c)
	| otherwise = getWordForCode code cs
	
---------------------------------
convertCodemapeToKeyValuePair codemape = map convert codemape
	where convert (a,b) = (KeyValuePair b a)

