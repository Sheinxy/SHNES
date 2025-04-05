module Emulator.Nes where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy       as BL
import           Emulator.Cartridge
import           Emulator.Components.Cpu
import           Emulator.Components.Mapper (Mapper, mapper, readWord)
import           Emulator.Components.Ram    (RAM, newRam)

data Nes = Nes
    { nesCpu       :: Cpu
    , nesCartridge :: Cartridge
    , nesRam       :: RAM
    , nesMapper    :: Mapper
    }

instance Show Nes where
    show nes = show (nesCpu nes)

newNes :: String -> IO Nes
newNes romFile = do
    romData <- BL.readFile romFile
    ram <- newRam
    cpu <- newCpu
    let cartridge = runGet parseCartridge romData
    let mapper' = mapper (mapperNumber $ header cartridge) ram cartridge
    {-
    pc <- readWord mapper' 0xFFFC
    setCpuPC cpu pc
    -}
    return $ Nes cpu cartridge ram mapper'

runNes :: Nes -> IO ()
runNes nes = do
    a <- loadNextByte cpu mapper'
    print (showHex a "")
    print nes
    a <- loadNextByte cpu mapper'
    print (showHex a "")
    print nes
    where cpu = nesCpu nes
          mapper' = nesMapper nes
