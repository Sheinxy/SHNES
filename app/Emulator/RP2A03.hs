module Emulator.RP2A03 where

import Emulator.RP2A03.Cpu (Cpu(..), newCpu)

data RP2A03 = RP2A03
    { rp2a03Cpu :: Cpu
    -- TODO: RAM, Mapper
    } deriving (Show)

newRp2a03 :: IO RP2A03
newRp2a03 = do
   cpu <- newCpu
   return $ RP2A03 cpu
