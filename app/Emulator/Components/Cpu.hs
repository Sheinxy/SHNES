{-# LANGUAGE BinaryLiterals #-}
module Emulator.Components.Cpu where

import           Data.IORef
import           Data.Word
import           Emulator.Components.Mapper (Mapper (..), readWord)
import           Numeric                    (showHex)

-- Making a deal with the devil (this is for debugging purposes)
import           System.IO.Unsafe           (unsafePerformIO)

data Cpu = Cpu
    { registerA  :: IORef Word8 -- A is byte-wide and along with the arithmetic logic unit (ALU), supports using the status register for carrying, overflow detection, and so on.
    -- X and Y are byte-wide and used for several addressing modes. They can be used as loop counters easily, using INC/DEC and branch instructions. Not being the accumulator, they have limited addressing modes themselves when loading and saving.
    , registerX  :: IORef Word8
    , registerY  :: IORef Word8
    -- The 2-byte program counter PC supports 65536 direct (unbanked) memory locations, however not all values are sent to the cartridge. It can be accessed either by allowing CPU's internal fetch logic increment the address bus, an interrupt (NMI, Reset, IRQ/BRQ), and using the RTS/JMP/JSR/Branch instructions.
    , registerPC :: IORef Word16
    -- S is byte-wide and can be accessed using interrupts, pulls, pushes, and transfers. It indexes into a 256-byte stack at $0100-$01FF.
    , registerS  :: IORef Word8
    -- P has 6 bits used by the ALU but is byte-wide. PHP, PLP, arithmetic, testing, and branch instructions can access this register. See status flags.
    , registerP  :: IORef Word8
    }

instance Show Cpu where
    show cpu = "A: " ++ showHex regA "" ++
               ", X: " ++ showHex regX "" ++
               ", Y: " ++ showHex regY "" ++
               ", PC: " ++ showHex regPC "" ++
               ", S: " ++ showHex regS "" ++
               ", P: " ++ showHex regP ""
            where regA = unsafePerformIO $ readIORef $ registerA cpu
                  regX = unsafePerformIO $ readIORef $ registerX cpu
                  regY = unsafePerformIO $ readIORef $ registerY cpu
                  regPC = unsafePerformIO $ readIORef $ registerPC cpu
                  regS = unsafePerformIO $ readIORef $ registerS cpu
                  regP = unsafePerformIO $ readIORef $ registerP cpu

newCpu :: IO Cpu
newCpu = do
    regA <- newIORef 0
    regX <- newIORef 0
    regY <- newIORef 0
    regPC <- newIORef 0xC000
    regS <- newIORef 0xFD
    -- NV1BDIZC
    -- 00100100 = 0x24
    regP  <- newIORef 0b00100100

    return $ Cpu regA regX regY regPC regS regP

setCpuRegister :: Cpu -> (Cpu -> IORef Word8) -> Word8 -> IO ()
setCpuRegister cpu reg = writeIORef (reg cpu)

setCpuPC :: Cpu -> Word16 -> IO ()
setCpuPC cpu = writeIORef (registerPC cpu)

loadNextByte :: Cpu -> Mapper -> IO Word8
loadNextByte cpu mapper = do
    pc <- readIORef (registerPC cpu)
    val <- readByte mapper pc
    modifyIORef (registerPC cpu) (+ 1)
    return val

loadNextWord :: Cpu -> Mapper -> IO Word16
loadNextWord cpu mapper = do
    pc <- readIORef (registerPC cpu)
    val <- readWord mapper pc
    modifyIORef (registerPC cpu) (+ 2)
    return val
