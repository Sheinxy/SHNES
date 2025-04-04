module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Emulator.Cartridge
import           Emulator.RP2A03
import           System.Environment
import           System.Exit          (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [romFile] -> do
            romData <- BL.readFile romFile
            let cartridge = runGet parseCartridge romData
            chip <- newRp2a03
            print chip
        _ -> die "Usage: SHNES <rom file>"
