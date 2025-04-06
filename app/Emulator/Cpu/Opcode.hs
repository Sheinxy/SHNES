module Emulator.Cpu.Opcode (cpuStep) where

import           Control.Monad              (when)
import           Data.Bits                  (xor, (.&.))
import           Data.Word
import           Emulator.Components.Cpu
import           Emulator.Components.Mapper (Mapper (..))
import           Emulator.Cpu.Addressing
import           Utils.Debug

cpuStep :: Cpu -> Mapper -> IO ()
cpuStep cpu mapper = do
    opcode <- loadNextByte cpu mapper
    let operation = case opcode of
            -- ADC
            0x69 -> adc Immediate; 0x65 -> adc (ZeroPage None)
            0x75 -> adc (ZeroPage X); 0x6D -> adc (Absolute None)
            0x7D -> adc (Absolute X); 0x79 -> adc (Absolute Y)
            0x61 -> adc (Indirect X); 0x71 -> adc (Indirect Y)
            -- BCC
            0x90 -> bcc Relative
            -- BCS
            0xB0 -> bcs Relative
            -- BEQ
            0xF0 -> beq Relative
            -- BMI
            0x30 -> bmi Relative
            -- BNE
            0xD0 -> bne Relative
            -- BPL
            0x10 -> bpl Relative
            -- BIT
            0x24 -> bit (ZeroPage None); 0x2C -> bit (Absolute None)
            -- BVC
            0x50 -> bvc Relative
            -- BVS
            0x70 -> bvs Relative
            -- CLC
            0x18 -> clc
            -- JMP
            0x4C -> jmp (Absolute None); 0x6C -> jmp (Indirect None)
            -- JSR
            0x20 -> jsr (Absolute None)
            -- LDA
            0xA9 -> lda Immediate; 0xA5 -> lda (ZeroPage None)
            0xB5 -> lda (ZeroPage X); 0xAD -> lda (Absolute None)
            0xBD -> lda (Absolute X); 0xB9 -> lda (Absolute Y)
            0xA1 -> lda (Indirect X); 0xB1 -> lda (Indirect Y)
            -- LDX
            0xA2 -> ldx Immediate; 0xA6 -> ldx (ZeroPage None)
            0xB6 -> ldx (ZeroPage Y); 0xA4 -> ldx (Absolute None)
            0xBE -> ldx (Absolute Y)
            -- NOP
            0xEA -> nop
            -- RTS
            0x60 -> rts
            -- SEC
            0x38 -> sec
            -- STA
            0x85 -> sta (ZeroPage None); 0x95 -> sta (ZeroPage X)
            0x8D -> sta (Absolute None); 0x9D -> sta (Absolute X)
            0x99 -> sta (Absolute Y); 0x81 -> sta (Indirect X)
            0x91 -> sta (Indirect Y)
            -- STX
            0x86 -> stx (ZeroPage None); 0x96 -> stx (ZeroPage Y)
            0x8E -> stx (Absolute None)
            _ -> nop
    operation cpu mapper

traceInstruction :: Integral a => String -> AddressingMode -> Word16 -> a -> IO ()
traceInstruction mnemonic mode addr value = trace $ mnemonic ++ " " ++ getTraceString mode (fromIntegral addr) (fromIntegral value)

adc :: AddressingMode -> Cpu -> Mapper -> IO ()
adc mode cpu mapper = do
    a <- getCpuRegister registerA cpu
    c <- fromEnum <$> getCpuStatusFlag statusC cpu

    addr <- getOperandAddress cpu mapper mode
    v <- cpuReadByte mapper addr

    traceInstruction "ADC" mode addr v

    let result = fromIntegral a + fromIntegral v + fromIntegral c :: Int
    let overflow = (result `xor` fromIntegral a) .&. (result `xor` fromIntegral v) .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (result > 0xFF) cpu
    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusV (overflow /= 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu $ fromIntegral result

-- Factorisation of all the BXX conditions
bxx :: String -> (Bool -> Bool) -> Word8 -> AddressingMode -> Cpu -> Mapper -> IO ()
bxx mnemonic condition flag mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode

    traceInstruction mnemonic mode addr addr
    status <- getCpuStatusFlag flag cpu
    when (condition status) $
        setCpuPC cpu addr

bcc :: AddressingMode -> Cpu -> Mapper -> IO ()
bcc = bxx "BCC" not statusC

bcs :: AddressingMode -> Cpu -> Mapper -> IO ()
bcs = bxx "BCS" id statusC

beq :: AddressingMode -> Cpu -> Mapper -> IO ()
beq = bxx "BEQ" id statusZ

bmi :: AddressingMode -> Cpu -> Mapper -> IO ()
bmi = bxx "BMI" id statusN

bne :: AddressingMode -> Cpu -> Mapper -> IO ()
bne = bxx "BNE" not statusZ

bpl :: AddressingMode -> Cpu -> Mapper -> IO ()
bpl = bxx "BPL" not statusN

bit :: AddressingMode -> Cpu -> Mapper -> IO ()
bit mode cpu mapper = do
    a <- getCpuRegister registerA cpu
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "BIT" mode addr a

    memory <- cpuReadByte mapper addr

    let result = fromIntegral a .&. fromIntegral memory :: Word8
    let overflow = result .&. 0x40
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusV (overflow /= 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

bvc :: AddressingMode -> Cpu -> Mapper -> IO ()
bvc = bxx "BVC" not statusV

bvs :: AddressingMode -> Cpu -> Mapper -> IO ()
bvs = bxx "BVS" id statusV

clc :: Cpu -> Mapper -> IO ()
clc cpu _ = do
    trace "CLC"

    setCpuStatusFlag statusC False cpu

jmp :: AddressingMode -> Cpu -> Mapper -> IO ()
jmp mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "JMP" mode addr addr

    setCpuPC cpu addr

jsr :: AddressingMode -> Cpu -> Mapper -> IO ()
jsr mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "JSR" mode addr addr

    pc <- getCpuPC cpu
    pushWord cpu mapper pc
    setCpuPC cpu addr

lda :: AddressingMode -> Cpu -> Mapper -> IO ()
lda mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    v <- cpuReadByte mapper addr

    traceInstruction "LDA" mode addr v

    let negative = fromIntegral v .&. 0x80 :: Int
    setCpuStatusFlag statusZ (v == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu v

ldx :: AddressingMode -> Cpu -> Mapper -> IO ()
ldx mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    v <- cpuReadByte mapper addr

    traceInstruction "LDX" mode addr v

    let negative = fromIntegral v .&. 0x80 :: Int
    setCpuStatusFlag statusZ (v == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerX cpu v

rts :: Cpu -> Mapper -> IO ()
rts cpu mapper = do
    pc <- pullWord cpu mapper

    trace "RTS"

    setCpuPC cpu pc

sec :: Cpu -> Mapper -> IO ()
sec cpu _ = do
    trace "SEC"

    setCpuStatusFlag statusC True cpu

sta :: AddressingMode -> Cpu -> Mapper -> IO ()
sta mode cpu mapper = do
    a <- getCpuRegister registerA cpu
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "STA" mode addr a

    cpuWriteByte mapper addr a

stx :: AddressingMode -> Cpu -> Mapper -> IO ()
stx mode cpu mapper = do
    x <- getCpuRegister registerX cpu
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "STX" mode addr x

    cpuWriteByte mapper addr x

nop :: Cpu -> Mapper -> IO ()
nop _ _ = do
    trace "NOP"
