module Emulator.Components.Ram where

import qualified Data.Vector.Unboxed.Mutable as MVector
import           Data.Word

type RAM = MVector.IOVector Word8

-- Let's avoid magic values, because we're clean kitties
cpuRamSize :: Word16
cpuRamSize = 0x0800

newCpuRam :: IO RAM
newCpuRam = do
    MVector.replicate (fromIntegral cpuRamSize) 0

readByte :: RAM -> Int -> IO Word8
readByte = MVector.read

writeByte :: RAM -> Int -> Word8 -> IO ()
writeByte = MVector.write
