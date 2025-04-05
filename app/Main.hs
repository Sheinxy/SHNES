module Main where

import           Emulator.Nes
import           System.Environment
import           System.Exit        (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [romFile] -> do
            nes <- newNes romFile
            runNes nes
        _ -> die "Usage: SHNES <rom file>"
