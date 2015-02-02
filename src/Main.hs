import Data.Word (Word8)
import Data.Foldable (foldl')
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Turn strings like "10010100" into Word8
readByte :: String -> Word8
readByte = foldl' step 0
  where step byte char = consumeBit char (shiftL byte 1)
        consumeBit '0' = id
        consumeBit '1' = (.|. 1)
        consumeBit _   = error "invalid input"

readBytes :: [String] -> ByteString
readBytes = BS.pack . fmap readByte

main :: IO ()
main = do
    let naïveté = readBytes ["01101110", "01100001", "01101001", "11001100", "10001000", "01110110", "01100101", "01110100", "11000011", "10101001"]
    print naïveté
