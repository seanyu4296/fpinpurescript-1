module BigIntUtil
       ( bigIntBitsToInt
       ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

bigIntBitsToInt :: BigInt -> Int
bigIntBitsToInt b =
  let b' = BigInt.and b intMask
  in unsafeBigIntToInt $ if b' <= topInt
     then b'
     else msbValue + (BigInt.and b' msbMask)

intMask :: BigInt
intMask = unsafeFromHex "FFFFFFFF"

msbMask :: BigInt
msbMask = unsafeFromHex "7FFFFFFF"

topInt :: BigInt
topInt = BigInt.fromInt top

msbValue :: BigInt
msbValue = - unsafeFromHex "80000000"

unsafeFromHex :: String -> BigInt
unsafeFromHex s = unsafePartial $ fromJust $ BigInt.fromBase 16 s

unsafeBigIntToInt :: BigInt -> Int
unsafeBigIntToInt b = unsafePartial $ fromJust $ Int.fromNumber $ BigInt.toNumber b