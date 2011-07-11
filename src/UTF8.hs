module UTF8 where

import Data.Word
import Data.Bits
import Data.Char

{-
-- Could also be imported:

import Codec.Binary.UTF8.Light as UTF8
 
encode :: Char -> [Word8]
encode c = head (UTF8.encodeUTF8' [UTF8.c2w c])

-}

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: Char -> [Word8]
encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
