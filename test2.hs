import Data.ByteString.Lazy as BS (readFile, unpack)
import Data.Word
import Data.Bits
import Data.Binary

main = do	
	b <- getBits "foo.txt"
	print $ take 20 b

data Bit = Zero | One

instance Show Bit where
	show (Zero) = "0"
	show (One) = "1"
	
getBit :: Bool -> Bit
getBit False = Zero
getBit True = One
	
{-
fileToByteString :: String -> IO [ByteString]
fileToByteString fp = do
    return BS.readFile    
	-}
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

test 3 x = 0
test n x = 2*n