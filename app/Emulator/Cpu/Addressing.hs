module Emulator.Cpu.Addressing (AddressingMode (..), Indexing (..), getTraceString, getOperandAddress, getStoreAddress, getJmpAddress, pageCrossed) where

import           Control.Monad              (when)
import           Data.Bits                  (shiftL, (.&.), (.|.))
import           Data.Int                   (Int8)
import           Data.Word
import           Emulator.Components.Cpu    (Cpu (registerX, registerY),
                                             getCpuPC, getCpuRegister,
                                             incrementCycle, loadNextByte,
                                             loadNextWord)
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

pageCrossed :: Word16 -> Word16 -> Bool
pageCrossed addr1 addr2 = (addr1 .&. 0xFF00) /= (addr2 .&. 0xFF00)

getOperandAddress :: Cpu -> Mapper -> AddressingMode -> IO Word16
getOperandAddress = getOperandAddress' True

-- Indirect jump has a bug where reading a word from 0xYYFF will read
-- The first byte from 0xYYFF and the second from 0xYY00 instead of 0xYZ00
getJmpAddress :: Cpu -> Mapper -> AddressingMode -> IO Word16
getJmpAddress cpu mapper (Absolute None) = getOperandAddress cpu mapper (Absolute None)
getJmpAddress cpu mapper _ = do
    addr <- loadNextWord cpu mapper
    unsplit <- cpuReadWord mapper addr

    lo <- cpuReadByte mapper addr
    hi <- cpuReadByte mapper (addr .&. 0xFF00)
    return $
        if (addr .&. 0xFF) == 0xFF
        then (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        else unsplit

getStoreAddress :: Cpu -> Mapper -> AddressingMode -> IO Word16
getStoreAddress = getOperandAddress' False

-- https://www.nesdev.org/wiki/CPU_addressing_modes
getOperandAddress' :: Bool -> Cpu -> Mapper -> AddressingMode -> IO Word16
getOperandAddress' addCycleOnPageCross cpu mapper mode = case mode of
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
        let result = fromIntegral addr + fromIntegral idx
        when addCycleOnPageCross $
            when (pageCrossed addr result) $
                incrementCycle cpu 1
        return result
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

        let addr = fromIntegral lo + fromIntegral hi * 256
        let result = fromIntegral y + addr
        when addCycleOnPageCross $
            when (pageCrossed addr result) $
                incrementCycle cpu 1
        return result

