import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits(shift)

data ETerm = SmallIntegerExt Int
             | IntegerExt Int
             deriving (Show)

load_bytes = BS.readFile

sample_small_int =
    BS.pack x
  where x = [131 :: Word8, 97 :: Word8, 4 :: Word8]

sample_int =
    BS.pack x
  where x = [131 :: Word8, 98 :: Word8, 0 :: Word8, 18 :: Word8, 216 :: Word8, 217 :: Word8]

term_decode0 :: [Char] -> (Char, [Char])
term_decode0 (x:xs) = (x, xs)

term_decode :: BS.ByteString -> Maybe ETerm
term_decode term =
    case fromIntegral tag :: Int of
      97 -> Just $ parse_small_int rest
      98 -> Just $ parse_int rest
      _ -> Nothing
  where term' = BS.unpack term
        (131:tag:rest) = term'
  -- where 131 = BS.head term
  --       tag = BS.head $ BS.tail term
  --       rest = BS.unpack $ BS.tail $ BS.tail term
--  where (131:tag:rest) = term

-- DONE: assert "" == BS.tail(xs)
-- TODO: how to handle non exhaustive case?
-- parse_small_int :: [Word8] -> Maybe ETerm
parse_small_int :: [Word8] -> ETerm
parse_small_int (x:[]) =
    SmallIntegerExt $ fromIntegral x
-- parse_small_int _ = Nothing

parse_int :: [Word8] -> ETerm
parse_int xs =
  IntegerExt $ bytes_to_int xs

bytes_to_int :: [Word8] -> Int
bytes_to_int [] = 0
bytes_to_int (x:xs) =
    x' `shift` width + bytes_to_int(xs)
  where width = length(xs) * 8
        x' = fromIntegral x
