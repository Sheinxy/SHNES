module Emulator.Cpu.Opcode (cpuStep) where

import           Control.Monad              (when)
import           Data.Bits                  (complement, xor, (.&.), (.|.))
import           Data.IORef
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
            -- AND
            0x29 -> and' Immediate; 0x25 -> and' (ZeroPage None)
            0x35 -> and' (ZeroPage X); 0x2D -> and' (Absolute None)
            0x3D -> and' (Absolute X); 0x39 -> and' (Absolute Y)
            0x21 -> and' (Indirect X); 0x31 -> and' (Indirect Y)
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
            -- CLD
            0xD8 -> cld
            -- CLV
            0xB8 -> clv
            -- CMP
            0xC9 -> cmp Immediate; 0xC5 -> cmp (ZeroPage None)
            0xD5 -> cmp (ZeroPage X); 0xCD -> cmp (Absolute None)
            0xDD -> cmp (Absolute X); 0xD9 -> cmp (Absolute Y)
            0xC1 -> cmp (Indirect X); 0xD1 -> cmp (Indirect Y)
            -- CPX
            0xE0 -> cpx Immediate; 0xE4 -> cpx (ZeroPage None)
            0xEC -> cpx (Absolute None)
            -- CPY
            0xC0 -> cpy Immediate; 0xC4 -> cpy (ZeroPage None)
            0xCC -> cpy (Absolute None)
            -- DEC
            0xC6 -> dec (ZeroPage None); 0xD6 -> dec (ZeroPage X)
            0xCE -> dec (Absolute None); 0xDE -> dec (Absolute X)
            -- DEX
            0XCA -> dex
            -- DEY
            0x88 -> dey
            -- EOR
            0x49 -> eor Immediate; 0x45 -> eor (ZeroPage None)
            0x55 -> eor (ZeroPage X); 0x4D -> eor (Absolute None)
            0x5D -> eor (Absolute X); 0x59 -> eor (Absolute Y)
            0x41 -> eor (Indirect X); 0x51 -> eor (Indirect Y)
            -- INC
            0xE6 -> inc (ZeroPage None); 0xF6 -> inc (ZeroPage X)
            0xEE -> inc (Absolute None); 0xFE -> inc (Absolute X)
            -- INX
            0xE8 -> inx
            -- INY
            0xC8 -> iny
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
            0xB6 -> ldx (ZeroPage Y); 0xAE -> ldx (Absolute None)
            0xBE -> ldx (Absolute Y)
            -- LDY
            0xA0 -> ldy Immediate; 0xA4 -> ldy (ZeroPage None)
            0xB4 -> ldy (ZeroPage X); 0xAC -> ldy (Absolute None)
            0xBC -> ldy (Absolute X)
            -- NOP
            0xEA -> nop
            -- ORA
            0x09 -> ora Immediate; 0x05 -> ora (ZeroPage None)
            0x15 -> ora (ZeroPage X); 0x0D -> ora (Absolute None)
            0x1D -> ora (Absolute X); 0x19 -> ora (Absolute Y)
            0x01 -> ora (Indirect X); 0x11 -> ora (Indirect Y)
            -- PHA
            0x48 -> pha
            -- PHP
            0x08 -> php
            -- PLA
            0x68 -> pla
            -- PLP
            0x28 -> plp
            -- RTI
            0x40 -> rti
            -- RTS
            0x60 -> rts
            -- SBC
            0xE9 -> sbc Immediate; 0xE5 -> sbc (ZeroPage None)
            0xF5 -> sbc (ZeroPage X); 0xED -> sbc (Absolute None)
            0xFD -> sbc (Absolute X); 0xF9 -> sbc (Absolute Y)
            0xE1 -> sbc (Indirect X); 0xF1 -> sbc (Indirect Y)
            -- SEC
            0x38 -> sec
            -- SED
            0xF8 -> sed
            -- SEI
            0x78 -> sei
            -- STA
            0x85 -> sta (ZeroPage None); 0x95 -> sta (ZeroPage X)
            0x8D -> sta (Absolute None); 0x9D -> sta (Absolute X)
            0x99 -> sta (Absolute Y); 0x81 -> sta (Indirect X)
            0x91 -> sta (Indirect Y)
            -- STX
            0x86 -> stx (ZeroPage None); 0x96 -> stx (ZeroPage Y)
            0x8E -> stx (Absolute None)
            -- TAX
            0xAA -> tax
            -- TAY
            0xA8 -> tay
            -- TSX
            0xBA -> tsx
            -- TXA
            0x8A -> txa
            -- TXS
            0x9A -> txs
            -- TYA
            0x98 -> tya
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

-- Because and is a Prelude function :(
and' :: AddressingMode -> Cpu -> Mapper -> IO ()
and' mode cpu mapper = do
    a <- getCpuRegister registerA cpu

    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "AND" mode addr memory

    let result = a .&. memory
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

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

    memory <- cpuReadByte mapper addr

    traceInstruction "BIT" mode addr memory

    let result = fromIntegral a .&. fromIntegral memory :: Word8
    let overflow = memory .&. 0x40
    let negative = memory .&. 0x80

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

cld :: Cpu -> Mapper -> IO ()
cld cpu _ = do
    trace "CLD"

    setCpuStatusFlag statusD False cpu

clv :: Cpu -> Mapper -> IO ()
clv cpu _ = do
    trace "CLV"

    setCpuStatusFlag statusV False cpu

compareReg :: String -> (Cpu -> IORef Word8) -> AddressingMode -> Cpu -> Mapper -> IO ()
compareReg mnemonic reg mode cpu mapper = do
    register <- getCpuRegister reg cpu

    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction mnemonic mode addr memory

    let result = register - memory
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (register >= memory) cpu
    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

cmp :: AddressingMode -> Cpu -> Mapper -> IO ()
cmp = compareReg "CMP" registerA

cpx :: AddressingMode -> Cpu -> Mapper -> IO ()
cpx = compareReg "CPX" registerX

cpy :: AddressingMode -> Cpu -> Mapper -> IO ()
cpy = compareReg "CPY" registerY

dec :: AddressingMode -> Cpu -> Mapper -> IO ()
dec mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "DEC" mode addr memory

    let result = memory - 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    cpuWriteByte mapper addr result

dex :: Cpu -> Mapper -> IO ()
dex cpu _ = do
    trace "DEX"

    x <- getCpuRegister registerX cpu

    let result = x - 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerX cpu result

dey :: Cpu -> Mapper -> IO ()
dey cpu _ = do
    trace "DEY"

    y <- getCpuRegister registerY cpu

    let result = y - 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerY cpu result

eor :: AddressingMode -> Cpu -> Mapper -> IO ()
eor mode cpu mapper = do
    a <- getCpuRegister registerA cpu

    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "EOR" mode addr memory

    let result = a `xor` memory
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

inc :: AddressingMode -> Cpu -> Mapper -> IO ()
inc mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "INC" mode addr memory

    let result = memory + 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    cpuWriteByte mapper addr result

inx :: Cpu -> Mapper -> IO ()
inx cpu _ = do
    trace "INX"

    x <- getCpuRegister registerX cpu

    let result = x + 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerX cpu result

iny :: Cpu -> Mapper -> IO ()
iny cpu _ = do
    trace "INY"

    y <- getCpuRegister registerY cpu

    let result = y + 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerY cpu result

jmp :: AddressingMode -> Cpu -> Mapper -> IO ()
jmp mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "JMP" mode addr addr

    setCpuPC cpu addr

jsr :: AddressingMode -> Cpu -> Mapper -> IO ()
jsr mode cpu mapper = do
    pc <- getCpuPC cpu
    addr <- getOperandAddress cpu mapper mode

    traceInstruction "JSR" mode addr addr

    pushWord cpu mapper (pc + 1)
    setCpuPC cpu addr

loadRegister :: String -> (Cpu -> IORef Word8) -> AddressingMode -> Cpu -> Mapper -> IO ()
loadRegister mnemonic reg mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    v <- cpuReadByte mapper addr

    traceInstruction mnemonic mode addr v

    let negative = fromIntegral v .&. 0x80 :: Int
    setCpuStatusFlag statusZ (v == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister reg cpu v

lda :: AddressingMode -> Cpu -> Mapper -> IO ()
lda = loadRegister "LDA" registerA

ldx :: AddressingMode -> Cpu -> Mapper -> IO ()
ldx = loadRegister "LDX" registerX

ldy :: AddressingMode -> Cpu -> Mapper -> IO ()
ldy = loadRegister "LDY" registerY

nop :: Cpu -> Mapper -> IO ()
nop _ _ = do
    trace "NOP"

ora :: AddressingMode -> Cpu -> Mapper -> IO ()
ora mode cpu mapper = do
    a <- getCpuRegister registerA cpu

    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "ORA" mode addr memory

    let result = a .|. memory
    let negative = result .&. 0x80

    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

pha :: Cpu -> Mapper -> IO ()
pha cpu mapper = do
    trace "PHA"

    a <- getCpuRegister registerA cpu
    pushByte cpu mapper a

php :: Cpu -> Mapper -> IO ()
php cpu mapper = do
    trace "PHP"

    setCpuStatusFlag statusB True cpu
    flags <- getCpuRegister registerP cpu
    setCpuStatusFlag statusB False cpu

    pushByte cpu mapper flags

pla :: Cpu -> Mapper -> IO ()
pla cpu mapper = do
    trace "PLA"

    value <- pullByte cpu mapper
    let negative = value .&. 0x80

    setCpuStatusFlag statusZ (value == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu value

plp :: Cpu -> Mapper -> IO ()
plp cpu mapper = do
    trace "PLP"

    flags <- pullByte cpu mapper
    -- TODO: Handle delay on I flag
    setCpuRegister registerP cpu flags
    setCpuStatusFlag statusB False cpu
    setCpuStatusFlag status1 True cpu

rti :: Cpu -> Mapper -> IO ()
rti cpu mapper = do
    -- TODO: I still have no idea what interrupts are :D
    trace "RTI"

    flags <- pullByte cpu mapper
    pc <- pullWord cpu mapper

    setCpuPC cpu pc

    setCpuRegister registerP cpu flags
    setCpuStatusFlag statusB False cpu
    setCpuStatusFlag status1 True cpu

rts :: Cpu -> Mapper -> IO ()
rts cpu mapper = do
    pc <- pullWord cpu mapper

    trace "RTS"

    setCpuPC cpu (pc + 1)

sbc :: AddressingMode -> Cpu -> Mapper -> IO ()
sbc mode cpu mapper = do
    a <- getCpuRegister registerA cpu
    c <- fromEnum . not <$> getCpuStatusFlag statusC cpu

    addr <- getOperandAddress cpu mapper mode
    memory <- cpuReadByte mapper addr

    traceInstruction "SBC" mode addr memory

    let result = fromIntegral a - fromIntegral memory - fromIntegral c :: Int
    let overflow = (result `xor` fromIntegral a) .&. (result `xor` fromIntegral (complement memory)) .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (result >= 0x00) cpu
    setCpuStatusFlag statusZ (result == 0x00) cpu
    setCpuStatusFlag statusV (overflow /= 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu $ fromIntegral result

sec :: Cpu -> Mapper -> IO ()
sec cpu _ = do
    trace "SEC"

    setCpuStatusFlag statusC True cpu

sed :: Cpu -> Mapper -> IO ()
sed cpu _ = do
    trace "SED"

    setCpuStatusFlag statusD True cpu

sei :: Cpu -> Mapper -> IO ()
sei cpu _ = do
    trace "SEI"

    -- TODO: This should be delayed one instruction :)
    setCpuStatusFlag statusI True cpu

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

tax :: Cpu -> Mapper -> IO ()
tax cpu _ = do
    a <- getCpuRegister registerA cpu

    trace "TAX"
    let negative = a .&. 0x80

    setCpuStatusFlag statusZ (a == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerX cpu a

tay :: Cpu -> Mapper -> IO ()
tay cpu _ = do
    a <- getCpuRegister registerA cpu

    trace "TAY"
    let negative = a .&. 0x80

    setCpuStatusFlag statusZ (a == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerY cpu a

tsx :: Cpu -> Mapper -> IO ()
tsx cpu _ = do
    s <- getCpuRegister registerS cpu

    trace "TSX"
    let negative = s .&. 0x80

    setCpuStatusFlag statusZ (s == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerX cpu s

txa :: Cpu -> Mapper -> IO ()
txa cpu _ = do
    x <- getCpuRegister registerX cpu

    trace "TXA"
    let negative = x .&. 0x80

    setCpuStatusFlag statusZ (x == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu x

txs :: Cpu -> Mapper -> IO ()
txs cpu _ = do
    x <- getCpuRegister registerX cpu

    trace "TXS"

    setCpuRegister registerS cpu x

tya :: Cpu -> Mapper -> IO ()
tya cpu _ = do
    y <- getCpuRegister registerY cpu

    trace "TYA"
    let negative = y .&. 0x80

    setCpuStatusFlag statusZ (y == 0x00) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu y
