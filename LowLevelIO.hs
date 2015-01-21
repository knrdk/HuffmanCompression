module LowLevelIO (
	fileToWordList
	, fileToBitArray
	) where

import Data.ByteString.Lazy as BS (readFile, unpack)
import Data.Word (Word8())
import Bit (Bit(), word8ToBitArray)

fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ unpack contents	
	
fileToBitArray :: String -> IO [Bit]
fileToBitArray fp = do
	contents <- fileToWordList fp
	return $ concat $ map word8ToBitArray contents

