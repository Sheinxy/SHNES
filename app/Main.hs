module Main where

import           Emulator.Cartridge
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           System.Environment
import           System.Exit          (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [romFile] -> do
            romData <- BL.readFile romFile
            let cartridge = runGet parseCartridge romData
            print cartridge
        _ -> die "Usage: SHNES <rom file>"
