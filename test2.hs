import Data.ByteString as BS (readFile, writeFile, unpack, pack)
import Data.Word
import Data.Bits

main = do	
	b <- getBits "foo.txt"	
	BS.writeFile "copy.txt" $ bitArrayToByteString b

data Bit = Zero | One

instance Show Bit where
	show (Zero) = "0"
	show (One) = "1"
	
getBit :: Bool -> Bit
getBit False = Zero
getBit True = One
	
getBits :: String -> IO [Bit]
getBits fp = do
	contents <- fileToWordList fp
	return $ concat $ map word8ToByteArray contents
	
fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ unpack contents
	
word8ToByteArray x = word8ToByteArray' 7 x
	
--Word8ToByteArray' :: Int -> Word8 -> [Bool]
word8ToByteArray' (-1) x = []
word8ToByteArray' i y = (getBit $ testBit y i) : (word8ToByteArray' (i-1) y)

---
zeroes = Zero : zeroes


bitArrayToByteString = BS.pack . bitArrayToWord8Array

bitArrayToWord8Array x = map byteToWord8 $ chop 8 x

chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

byteToWord8 x = (byteToInt x) :: Word8

bitArrayToByte x = take 8 (x++zeroes)

byteToInt = reversedByteToInt . reverse . bitArrayToByte

reversedByteToInt [] = 0
reversedByteToInt (One:xs) = 1 + 2 * (reversedByteToInt xs)
reversedByteToInt (Zero:xs) = 2 * (reversedByteToInt xs)
