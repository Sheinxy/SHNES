{-# LANGUAGE BinaryLiterals #-}
module Emulator.Components.Cpu where

import           Data.Bits
import           Data.IORef
import           Data.Word
import           Emulator.Components.Mapper (Mapper (..), cpuReadWord)
import           Utils.Debug

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
    , cpuCycle   :: IORef Int
    }

instance Show Cpu where
    show cpu = "A: " ++ getHexRep (fromIntegral regA) ++
               ", X: " ++ getHexRep (fromIntegral regX) ++
               ", Y: " ++ getHexRep (fromIntegral regY) ++
               ", PC: " ++ getHexRep (fromIntegral regPC) ++
               ", S: " ++ getHexRep (fromIntegral regS) ++
               ", P: " ++ getHexRep (fromIntegral regP) ++
               ", CYC: " ++ show cyc
            where regA = unsafePerformIO $ readIORef $ registerA cpu
                  regX = unsafePerformIO $ readIORef $ registerX cpu
                  regY = unsafePerformIO $ readIORef $ registerY cpu
                  regPC = unsafePerformIO $ readIORef $ registerPC cpu
                  regS = unsafePerformIO $ readIORef $ registerS cpu
                  regP = unsafePerformIO $ readIORef $ registerP cpu
                  cyc = unsafePerformIO $ readIORef $ cpuCycle cpu

statusC :: Word8
statusC = 1

statusZ :: Word8
statusZ = 2

statusI :: Word8
statusI = 4

statusD :: Word8
statusD = 8

statusB :: Word8
statusB = 16

status1 :: Word8
status1 = 32

statusV :: Word8
statusV = 64

statusN :: Word8
statusN = 128

getCpuStatusFlag :: Word8 -> Cpu -> IO Bool
getCpuStatusFlag flag cpu = do
    status <- getCpuRegister registerP cpu
    let res = (fromIntegral status .&. fromIntegral flag) :: Word8
    return $ res /= 0

setCpuStatusFlag :: Word8 -> Bool -> Cpu -> IO ()
setCpuStatusFlag flag value cpu = do
    status <- getCpuRegister registerP cpu
    let status' = if value then
                    status .|. fromIntegral flag
                  else
                    status .&. fromIntegral (complement flag)
    setCpuRegister registerP cpu status'

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

    cyc <- newIORef 7
    return $ Cpu regA regX regY regPC regS regP cyc

setCpuRegister :: (Cpu -> IORef Word8) -> Cpu -> Word8 -> IO ()
setCpuRegister reg = writeIORef . reg

getCpuRegister :: (Cpu -> IORef Word8) -> Cpu -> IO Word8
getCpuRegister reg = readIORef . reg

modifyCpuRegister :: (Cpu -> IORef Word8) -> Cpu -> (Word8 -> Word8) -> IO ()
modifyCpuRegister reg = modifyIORef . reg

setCpuPC :: Cpu -> Word16 -> IO ()
setCpuPC cpu = writeIORef (registerPC cpu)

getCpuPC :: Cpu -> IO Word16
getCpuPC = readIORef . registerPC

loadNextByte :: Cpu -> Mapper -> IO Word8
loadNextByte cpu mapper = do
    pc <- readIORef (registerPC cpu)
    val <- cpuReadByte mapper pc
    modifyIORef (registerPC cpu) (+ 1)
    return val

loadNextWord :: Cpu -> Mapper -> IO Word16
loadNextWord cpu mapper = do
    pc <- readIORef (registerPC cpu)
    val <- cpuReadWord mapper pc
    modifyIORef (registerPC cpu) (+ 2)
    return val

pushByte :: Cpu -> Mapper -> Word8 -> IO ()
pushByte cpu mapper value = do
    sp <- getCpuRegister registerS cpu
    cpuWriteByte mapper (0x0100 + fromIntegral sp) value
    modifyCpuRegister registerS cpu (subtract 1)

pushWord :: Cpu -> Mapper -> Word16 -> IO ()
pushWord cpu mapper value = do
    let lo = fromIntegral value
    let hi = fromIntegral value `shiftR` 8 :: Word16

    pushByte cpu mapper (fromIntegral hi)
    pushByte cpu mapper lo

pullByte :: Cpu -> Mapper -> IO Word8
pullByte cpu mapper = do
    modifyCpuRegister registerS cpu (+ 1)
    sp <- getCpuRegister registerS cpu
    cpuReadByte mapper (0x0100 + fromIntegral sp)

pullWord :: Cpu -> Mapper -> IO Word16
pullWord cpu mapper = do
    lo <- pullByte cpu mapper
    hi <- pullByte cpu mapper
    return $ (fromIntegral hi `shiftL` 8) + fromIntegral lo

incrementCycle :: Cpu -> Int -> IO ()
incrementCycle cpu cyc = modifyIORef (cpuCycle cpu) (+ cyc)
