module Bit (
	Bit(Zero,One)
	, getBit
	, zeroes
	, stringToWord8
	, stringToBitArray
	, word8ToBitArray
	, bitArrayToByteString
	) where

import Data.Word
import Data.ByteString.Lazy as BS (pack)
import Data.Bits (testBit, setBit, clearBit, bit)

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

---START: stringToBitArray
stringToBitArray :: String -> [Bit]
stringToBitArray [] = []
stringToBitArray ('1':xs) = One : (stringToBitArray xs)
stringToBitArray ('0':xs) = Zero : (stringToBitArray xs)
---END: stringToBitArray

---START: stringToWord8
stringToWord8 :: String -> Word8
stringToWord8 x = intToWord8 (read x :: Int)

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
---END: stringToWord8

---START: word8ToBitArray
word8ToBitArray :: Word8 -> [Bit]	
word8ToBitArray = word8ToBitArray' 7
	
word8ToBitArray' :: Int -> Word8 -> [Bit]
word8ToBitArray' (-1) x = []
word8ToBitArray' i y = (getBit $ testBit y i) : (word8ToBitArray' (i-1) y)
---END: word8ToBitArray

---START: bitArrayToByteString
bitArrayToByteString x = BS.pack $ bitArrayToWord8Array x

bitArrayToWord8Array :: [Bit] -> [Word8]
bitArrayToWord8Array x = map byteToWord8 $ chop 8 x

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)
---END: bitArrayToByteString

---START: byteToWord8
byteToWord8 :: [Bit] -> Word8
byteToWord8 = (byteToWord8' 7) . bitArrayToByte
	where bitArrayToByte x = take 8 (x++zeroes)

byteToWord8' :: Int -> [Bit] -> Word8
byteToWord8' (-1) _ = clearBit (bit 0) 0
byteToWord8' n (One:xs) = setBit (byteToWord8' (n-1) xs) n
byteToWord8' n (Zero:xs) = byteToWord8' (n-1) xs
---END: byteToWord8