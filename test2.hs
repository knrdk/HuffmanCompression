import Data.ByteString.Lazy as BS (readFile, writeFile, unpack, pack)
import Data.Word
import Data.Bits

main = do	
	b <- fileToBitArray "foo.txt"
	BS.writeFile "bar.txt" $ bitArrayToByteString $ map neg b

---
	
data Bit = Zero | One

{-
instance Show Bit where
	show (Zero) = "0"
	show (One) = "1"
-}
getBit :: Bool -> Bit
getBit False = Zero
getBit True = One

zeroes = Zero : zeroes
	
-----

neg :: Bit -> Bit
neg One = Zero
neg Zero = One	
	
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