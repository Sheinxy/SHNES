module Emulator.Components.Ram where

import qualified Data.Vector.Unboxed.Mutable as MVector
import           Data.Word

type RAM = MVector.IOVector Word8

-- Let's avoid magic values, because we're clean kitties
ramSize :: Word16
ramSize = 0x0800

newRam :: IO RAM
newRam = do
    MVector.replicate (fromIntegral ramSize) 0

readByte :: RAM -> Int -> IO Word8
readByte = MVector.read

writeByte :: RAM -> Int -> Word8 -> IO ()
writeByte = MVector.write
