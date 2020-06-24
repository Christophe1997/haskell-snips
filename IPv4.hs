-- Problem: Int32 to IPv4
-- Rank: 5
-- Src: https://www.codewars.com/kata/52e88b39ffb6ac53a400022e

module IPv4 where
import Data.Word  (Word8, Word32)
import Data.Bits  (shiftR)
import Data.List  (intercalate)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP word32 = intercalate "." . map show $ bytes
                    where bytes = fromIntegral <$> [word32 `shiftR` 24,
                                                    word32 `shiftR` 16,
                                                    word32 `shiftR` 8,
                                                    word32] :: [Word8]

