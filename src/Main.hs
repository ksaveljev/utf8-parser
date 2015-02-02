import Data.Word (Word8, Word32)
import Data.Attoparsec.ByteString (Parser, parseOnly, endOfInput, manyTill', anyWord8)
import Data.Foldable (foldl')
import Data.Bits ((.|.), shiftL, setBit, testBit)
import Data.ByteString (ByteString)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (unless, foldM)
import qualified Data.ByteString as BS

type SubByte = (Word8, Int)

subZero :: SubByte
subZero = (0, 0)

pushBit :: Bool -> SubByte -> SubByte
pushBit True (b, n) = (setBit (shiftL b 1) 0, n + 1)
pushBit False (b, n) = (shiftL b 1, n + 1)

-- | Turn strings like "10010100" into Word8
readByte :: String -> Word8
readByte = foldl' step 0
  where step byte char = consumeBit char (shiftL byte 1)
        consumeBit '0' = id
        consumeBit '1' = (.|. 1)
        consumeBit _   = error "invalid input"

readBytes :: [String] -> ByteString
readBytes = BS.pack . fmap readByte

parseUtf8 :: ByteString -> Either String [Word32]
parseUtf8 = parseOnly utf8Parser

utf8Parser :: Parser [Word32]
utf8Parser = manyTill' codePointParser endOfInput

codePointParser :: Parser Word32
codePointParser = byteSequence ["0xxxxxxx"] <|>
                  overlong 0x7F (multibyte "110xxxxx" 1) <|>
                  overlong 0x7FF (multibyte "1110xxxx" 2) <|>
                  overlong 0xFFFF (multibyte "11110xxx" 3)
  where multibyte leader count = byteSequence (leader : replicate count "10xxxxxx")

overlong :: Word32 -> Parser Word32 -> Parser Word32
overlong m parser = checkedParser parser (> m) "overlong codepoint!"

checkedParser :: Parser a -> (a -> Bool) -> String -> Parser a
checkedParser parser p msg = do
    byte <- parser
    unless (p byte) (fail msg)
    return byte

matchByte :: String -> Word8 -> Maybe SubByte
matchByte pattern byte = foldM check subZero (zip pattern bits)
  where bits = testBit byte <$> [7, 6 .. 0]
        check subByte ('1', True) = Just subByte
        check subByte ('0', False) = Just subByte
        check subByte ('x', bit) = Just (pushBit bit subByte)
        check _ _ = Nothing

satisfyMaybe :: (Word8 -> Maybe a) -> Parser a
satisfyMaybe f = do
    byte <- anyWord8
    maybe (fail "maybe not satisfied") return (f byte)

bitPattern :: String -> Parser SubByte
bitPattern pattern = satisfyMaybe (matchByte pattern)

byteSequence :: [String] -> Parser Word32
byteSequence patterns = do
    subBytes <- mapM bitPattern patterns
    return (foldl' mergeSubByte 0 subBytes)

mergeSubByte :: Word32 -> SubByte -> Word32
mergeSubByte whole (byte, bitCount) = shiftL whole bitCount .|. fromIntegral byte

test :: [String] -> IO ()
test = print . parseUtf8 . readBytes

main :: IO ()
main = do
    test [ "01101110", "01100001"             -- na
         , "01101001", "11001100", "10001000" -- ï
         , "01110110", "01100101", "01110100" -- vet
         , "11000011", "10101001"             -- é
         ]
    test ["11000000", "10000001"] -- overlong
    test ["11000000"] -- not enough continuation bits
    test ["10010010"] -- leading continuation bit
    test ["11010111", "10000000", "10001010"] -- too many continuation bits
