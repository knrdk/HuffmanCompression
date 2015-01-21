import Data.ByteString.Lazy as BS (writeFile, pack)
import System.IO

import Util (splitString)
import Bit (stringToWord8, stringToBitArray)
import LowLevelIO
	
main = do
	fhandleCm <- openFile "file.cod" ReadMode
	length <- hGetLine fhandleCm
	codemape <- readCodemape fhandleCm
	hClose fhandleCm
	bits <- fileToBitArray "file.dat"
	BS.writeFile "file.txt" $ BS.pack $ dcd (take (read length :: Int) bits) codemape

{-START: dcd-}
dcd = dcd' 0

dcd' n [] codemap = []	
dcd' n bits codemap = if containsCode (take n bits) codemap 
	then (getWordForCode (take n bits) codemap):(dcd' 1 (drop n bits) codemap)
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
			




