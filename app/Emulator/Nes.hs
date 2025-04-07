module Emulator.Nes where

import           Control.Monad              (when)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy       as BL
import           Emulator.Cartridge
import           Emulator.Components.Cpu
import           Emulator.Components.Mapper (Mapper, mapper)
import           Emulator.Components.Ram    (RAM, newCpuRam)
import           Emulator.Cpu.Opcode        (cpuStep)
import           Utils.Debug

data Nes = Nes
    { nesCpu       :: Cpu
    , nesCartridge :: Cartridge
    , nesCpuRam    :: RAM
    , nesMapper    :: Mapper
    }

instance Show Nes where
    show nes = show (nesCpu nes)

newNes :: String -> IO Nes
newNes romFile = do
    romData <- BL.readFile romFile
    cpuRam <- newCpuRam
    cpu <- newCpu
    let cartridge = runGet parseCartridge romData
    let mapper' = mapper (mapperNumber $ header cartridge) cpuRam cartridge
    {-
    pc <- cpuReadWord mapper' 0xFFFC
    setCpuPC cpu pc
    -}
    return $ Nes cpu cartridge cpuRam mapper'

runNes :: Nes -> IO ()
runNes nes = do
    pc <- getCpuPC cpu
    trace (getHexRep (fromIntegral pc) ++ " ")
    trace (show cpu ++ "    ")
    cpuStep cpu mapper'
    trace "\n"
    when (pc /= 0xC66E) $
        runNes nes
    where cpu = nesCpu nes
          mapper' = nesMapper nes
