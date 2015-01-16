import Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.List as L

data Bit = One | Zero


main = do
			contents <- fileToWordList "foo2.txt"   
			print $ L.sort $ L.map snd $ getFrequencies contents


getFrequencies s = L.map (\x->([L.head x], L.length x)) . L.group . L.sort $ s	--do poprawienia
	

fileToWordList :: String -> IO [Word8]
fileToWordList fp = do
    contents <- BS.readFile fp
    return $ unpack contents