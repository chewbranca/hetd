import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy as BS
import Data.Word

data ETerm = SmallIntegerExt Word8
             deriving (Show)

load_bytes = BS.readFile

sample_data =
    BS.pack x
  where x = [131 :: Word8, 97 :: Word8, 4 :: Word8]

term_decode0 :: [Char] -> (Char, [Char])
term_decode0 (x:xs) = (x, xs)

--term_decode :: ByteString -> (Int, 
term_decode :: BS.ByteString -> Maybe ETerm
term_decode term =
    case fromIntegral tag :: Int of
      97 -> Just $ parse_int rest
      _ -> Nothing
  where 131 = BS.head term
        tag = BS.head $ BS.tail term
        rest = BS.tail $ BS.tail term
--  where (131:tag:rest) = term

-- how to assert BS.tail(xs) is empty?
parse_int :: BS.ByteString -> ETerm
parse_int xs =
    SmallIntegerExt x
  where x = BS.head(xs)
