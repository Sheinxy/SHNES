module Emulator.Cpu.Addressing (AddressingMode (..), Indexing (..), getTraceString, getOperandAddress) where

import           Data.Int                   (Int8)
import           Data.Word
import           Emulator.Components.Cpu    (Cpu (registerX, registerY),
                                             getCpuPC, getCpuRegister,
                                             loadNextByte, loadNextWord)
import           Emulator.Components.Mapper (Mapper (cpuReadByte), cpuReadWord)
import           Utils.Debug

data Indexing = X | Y | None deriving Show

data AddressingMode =
    Implied | Accumulator | Immediate | Relative |
    ZeroPage Indexing | Absolute Indexing | Indirect Indexing deriving Show

getIndexOffset :: Indexing -> Cpu -> IO Word8
getIndexOffset None = return . const 0
getIndexOffset X    = getCpuRegister registerX
getIndexOffset Y    = getCpuRegister registerY

getTraceString :: AddressingMode -> Int -> Int -> String
getTraceString Implied _ _ = ""
getTraceString Accumulator _ _ = ""
getTraceString Immediate _ value = "#$" ++ getHexRep value
getTraceString Relative addr value = "$" ++ getHexRep addr ++ " = " ++ getHexRep value
getTraceString (ZeroPage None) addr value = "($" ++ getHexRep addr ++ ") = " ++ getHexRep value
getTraceString (ZeroPage indexing) addr value = "($" ++ getHexRep addr ++ " + " ++ show indexing ++ ") = " ++ getHexRep value
getTraceString (Absolute None) addr value = "$" ++ getHexRep addr ++ " = " ++ getHexRep value
getTraceString (Absolute indexing) addr value = "$" ++ getHexRep addr ++ " + " ++ show indexing ++ " = " ++ getHexRep value
getTraceString (Indirect None) addr value = "($" ++ getHexRep addr ++ ") = " ++ getHexRep value
getTraceString (Indirect X) addr value = "($" ++ getHexRep addr ++ " + X) = " ++ getHexRep value
getTraceString (Indirect Y) addr value = "($" ++ getHexRep addr ++ ") + Y = " ++ getHexRep value

-- https://www.nesdev.org/wiki/CPU_addressing_modes
getOperandAddress :: Cpu -> Mapper -> AddressingMode -> IO Word16
getOperandAddress cpu mapper mode = case mode of
    Implied -> return 0
    Accumulator -> return 0
    Immediate -> do
        pc <- getCpuPC cpu
        _ <- loadNextByte cpu mapper -- Skipping this byte
        return pc
    Relative -> do
        off <- loadNextByte cpu mapper
        pc <- getCpuPC cpu
        let signedOff = fromIntegral off :: Int8
        return (fromIntegral pc + fromIntegral signedOff)
    ZeroPage indexing -> do
        idx <- getIndexOffset indexing cpu
        addr <- loadNextByte cpu mapper
        return $ (fromIntegral addr + fromIntegral idx) `mod` 256
    Absolute indexing -> do
        idx <- getIndexOffset indexing cpu
        addr <- loadNextWord cpu mapper
        return (fromIntegral addr + fromIntegral idx)
    Indirect None -> do
        addr <- loadNextWord cpu mapper
        cpuReadWord mapper addr
    Indirect X -> do
        x <- getIndexOffset X cpu
        arg <- loadNextByte cpu mapper
        let loAddr = (fromIntegral arg + fromIntegral x) `mod` 256
        let hiAddr = (fromIntegral arg + fromIntegral x + 1) `mod` 256

        lo <- cpuReadByte mapper loAddr
        hi <- cpuReadByte mapper hiAddr
        return $ fromIntegral lo + fromIntegral hi * 256
    Indirect Y -> do
        y <- getIndexOffset Y cpu
        arg <- loadNextByte cpu mapper

        lo <- cpuReadByte mapper $ fromIntegral arg
        hi <- cpuReadByte mapper $ (fromIntegral arg + 1) `mod` 256

        return $ fromIntegral y + fromIntegral lo + fromIntegral hi * 256
