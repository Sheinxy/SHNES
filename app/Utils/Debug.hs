module Utils.Debug where

import           Text.Printf (printf)

getHexRep :: Int -> String
getHexRep x | x >= 0 && x <= 0xFF = printf "%02X" x
            | otherwise = printf "%04X" x

debugOn :: Bool
debugOn = True

trace :: String -> IO ()
trace | debugOn = putStr
      | otherwise = const $ pure ()
