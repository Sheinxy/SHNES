module Emulator.Cpu.Opcode (cpuStep) where

import           Control.Monad              (when)
import           Data.Bits                  (complement, shiftL, shiftR, xor,
                                             (.&.), (.|.))
import           Data.IORef
import           Data.Word
import           Emulator.Components.Cpu
import           Emulator.Components.Mapper (Mapper (..))
import           Emulator.Cpu.Addressing
import           Utils.Debug

getOperation :: Word8 -> (Cpu -> Mapper -> IO (), Int)
getOperation opcode = case opcode of
    -- ADC
    0x69 -> (adc Immediate, 2); 0x65 -> (adc (ZeroPage None), 3)
    0x75 -> (adc (ZeroPage X), 4); 0x6D -> (adc (Absolute None), 4)
    0x7D -> (adc (Absolute X), 4); 0x79 -> (adc (Absolute Y), 4)
    0x61 -> (adc (Indirect X), 6); 0x71 -> (adc (Indirect Y), 5)
    -- AND
    0x29 -> (and' Immediate, 2); 0x25 -> (and' (ZeroPage None), 3)
    0x35 -> (and' (ZeroPage X), 4); 0x2D -> (and' (Absolute None), 4)
    0x3D -> (and' (Absolute X), 4); 0x39 -> (and' (Absolute Y), 4)
    0x21 -> (and' (Indirect X), 6); 0x31 -> (and' (Indirect Y), 5)
    -- ASL
    0x0A -> (asl Accumulator, 2); 0x06 -> (asl (ZeroPage None), 5)
    0x16 -> (asl (ZeroPage X), 6); 0x0E -> (asl (Absolute None), 6)
    0x1E -> (asl (Absolute X), 7)
    -- BCC
    0x90 -> (bcc Relative, 2)
    -- BCS
    0xB0 -> (bcs Relative, 2)
    -- BEQ
    0xF0 -> (beq Relative, 2)
    -- BMI
    0x30 -> (bmi Relative, 2)
    -- BNE
    0xD0 -> (bne Relative, 2)
    -- BPL
    0x10 -> (bpl Relative, 2)
    -- BIT
    0x24 -> (bit (ZeroPage None), 3); 0x2C -> (bit (Absolute None), 4)
    -- BVC
    0x50 -> (bvc Relative, 2)
    -- BVS
    0x70 -> (bvs Relative, 2)
    -- CLC
    0x18 -> (clc, 2)
    -- CLD
    0xD8 -> (cld, 2)
    -- CLV
    0xB8 -> (clv, 2)
    -- CMP
    0xC9 -> (cmp Immediate, 2); 0xC5 -> (cmp (ZeroPage None), 3)
    0xD5 -> (cmp (ZeroPage X), 4); 0xCD -> (cmp (Absolute None), 4)
    0xDD -> (cmp (Absolute X), 4); 0xD9 -> (cmp (Absolute Y), 4)
    0xC1 -> (cmp (Indirect X), 6); 0xD1 -> (cmp (Indirect Y), 5)
    -- CPX
    0xE0 -> (cpx Immediate, 2); 0xE4 -> (cpx (ZeroPage None), 3)
    0xEC -> (cpx (Absolute None), 4)
    -- CPY
    0xC0 -> (cpy Immediate, 2); 0xC4 -> (cpy (ZeroPage None), 3)
    0xCC -> (cpy (Absolute None), 4)
    -- DEC
    0xC6 -> (dec (ZeroPage None), 5); 0xD6 -> (dec (ZeroPage X), 6)
    0xCE -> (dec (Absolute None), 6); 0xDE -> (dec (Absolute X), 7)
    -- DEX
    0xCA -> (dex, 2)
    -- DEY
    0x88 -> (dey, 2)
    -- EOR
    0x49 -> (eor Immediate, 2); 0x45 -> (eor (ZeroPage None), 3)
    0x55 -> (eor (ZeroPage X), 4); 0x4D -> (eor (Absolute None), 4)
    0x5D -> (eor (Absolute X), 4); 0x59 -> (eor (Absolute Y), 4)
    0x41 -> (eor (Indirect X), 6); 0x51 -> (eor (Indirect Y), 5)
    -- INC
    0xE6 -> (inc (ZeroPage None), 5); 0xF6 -> (inc (ZeroPage X), 6)
    0xEE -> (inc (Absolute None), 6); 0xFE -> (inc (Absolute X), 7)
    -- INX
    0xE8 -> (inx, 2)
    -- INY
    0xC8 -> (iny, 2)
    -- JMP
    0x4C -> (jmp (Absolute None), 3); 0x6C -> (jmp (Indirect None), 5)
    -- JSR
    0x20 -> (jsr (Absolute None), 6)
    -- LDA
    0xA9 -> (lda Immediate, 2); 0xA5 -> (lda (ZeroPage None), 3)
    0xB5 -> (lda (ZeroPage X), 4); 0xAD -> (lda (Absolute None), 4)
    0xBD -> (lda (Absolute X), 4); 0xB9 -> (lda (Absolute Y), 4)
    0xA1 -> (lda (Indirect X), 6); 0xB1 -> (lda (Indirect Y), 5)
    -- LDX
    0xA2 -> (ldx Immediate, 2); 0xA6 -> (ldx (ZeroPage None), 3)
    0xB6 -> (ldx (ZeroPage Y), 4); 0xAE -> (ldx (Absolute None), 4)
    0xBE -> (ldx (Absolute Y), 4)
    -- LDY
    0xA0 -> (ldy Immediate, 2); 0xA4 -> (ldy (ZeroPage None), 3)
    0xB4 -> (ldy (ZeroPage X), 4); 0xAC -> (ldy (Absolute None), 4)
    0xBC -> (ldy (Absolute X), 4)
    -- LSR
    0x4A -> (lsr Accumulator, 2); 0x46 -> (lsr (ZeroPage None), 5)
    0x56 -> (lsr (ZeroPage X), 6); 0x4E -> (lsr (Absolute None), 6)
    0x5E -> (lsr (Absolute X), 7)
    -- NOP
    0xEA -> (nop, 2)
    -- ORA
    0x09 -> (ora Immediate, 2); 0x05 -> (ora (ZeroPage None), 3)
    0x15 -> (ora (ZeroPage X), 4); 0x0D -> (ora (Absolute None), 4)
    0x1D -> (ora (Absolute X), 4); 0x19 -> (ora (Absolute Y), 4)
    0x01 -> (ora (Indirect X), 6); 0x11 -> (ora (Indirect Y), 5)
    -- PHA
    0x48 -> (pha, 3)
    -- PHP
    0x08 -> (php, 3)
    -- PLA
    0x68 -> (pla, 4)
    -- PLP
    0x28 -> (plp, 4)
    -- ROL
    0x2A -> (rol Accumulator, 2); 0x26 -> (rol (ZeroPage None), 5)
    0x36 -> (rol (ZeroPage X), 6); 0x2E -> (rol (Absolute None), 6)
    0x3E -> (rol (Absolute X), 7)
    -- ROR
    0x6A -> (ror Accumulator, 2); 0x66 -> (ror (ZeroPage None), 5)
    0x76 -> (ror (ZeroPage X), 6); 0x6E -> (ror (Absolute None), 6)
    0x7E -> (ror (Absolute X), 7)
    -- RTI
    0x40 -> (rti, 6)
    -- RTS
    0x60 -> (rts, 6)
    -- SBC
    0xE9 -> (sbc Immediate, 2); 0xE5 -> (sbc (ZeroPage None), 3)
    0xF5 -> (sbc (ZeroPage X), 4); 0xED -> (sbc (Absolute None), 4)
    0xFD -> (sbc (Absolute X), 4); 0xF9 -> (sbc (Absolute Y), 4)
    0xE1 -> (sbc (Indirect X), 6); 0xF1 -> (sbc (Indirect Y), 5)
    -- SEC
    0x38 -> (sec, 2)
    -- SED
    0xF8 -> (sed, 2)
    -- SEI
    0x78 -> (sei, 2)
    -- STA
    0x85 -> (sta (ZeroPage None), 3); 0x95 -> (sta (ZeroPage X), 4)
    0x8D -> (sta (Absolute None), 4); 0x9D -> (sta (Absolute X), 5)
    0x99 -> (sta (Absolute Y), 5); 0x81 -> (sta (Indirect X), 6)
    0x91 -> (sta (Indirect Y), 6)
    -- STX
    0x86 -> (stx (ZeroPage None), 3); 0x96 -> (stx (ZeroPage Y), 4)
    0x8E -> (stx (Absolute None), 4)
    -- STY
    0x84 -> (sty (ZeroPage None), 3); 0x94 -> (sty (ZeroPage X), 4)
    0x8C -> (sty (Absolute None), 4)
    -- TAX
    0xAA -> (tax, 2)
    -- TAY
    0xA8 -> (tay, 2)
    -- TSX
    0xBA -> (tsx, 2)
    -- TXA
    0x8A -> (txa, 2)
    -- TXS
    0x9A -> (txs, 2)
    -- TYA
    0x98 -> (tya, 2)
    _ -> (nop, 2)  -- default to NOP (safe fallback)


cpuStep :: Cpu -> Mapper -> IO ()
cpuStep cpu mapper = do
    opcode <- loadNextByte cpu mapper
    let (operation, cycles) = getOperation opcode
    operation cpu mapper
    incrementCycle cpu cycles

traceInstruction :: Integral a => String -> AddressingMode -> Word16 -> a -> IO ()
traceInstruction mnemonic mode addr value = trace $ mnemonic ++ " " ++ getTraceString mode (fromIntegral addr) (fromIntegral value)

adc :: AddressingMode -> Cpu -> Mapper -> IO ()
adc mode cpu mapper = do
    a <- getCpuRegister registerA cpu
    c <- fromEnum <$> getCpuStatusFlag statusC cpu

    addr <- getOperandAddress cpu mapper mode
    v <- cpuReadByte mapper addr

    traceInstruction "ADC" mode addr v

    let result = a + v + fromIntegral c
    let carry = fromIntegral a + fromIntegral v + c :: Int -- Ugly hack to get the carry bit
    let overflow = (result `xor` a) .&. (result `xor` v) .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry > 0xFF) cpu
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

asl :: AddressingMode -> Cpu -> Mapper -> IO ()
asl Accumulator cpu _ = do
    value <- getCpuRegister registerA cpu
    trace "ASL A"

    let result = value `shiftL` 1
    let carry = value .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

asl mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    value <- cpuReadByte mapper addr

    traceInstruction "ASL" mode addr value

    let result = value `shiftL` 1
    let carry = value .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    cpuWriteByte mapper addr result

-- Factorisation of all the BXX conditions
bxx :: String -> (Bool -> Bool) -> Word8 -> AddressingMode -> Cpu -> Mapper -> IO ()
bxx mnemonic condition flag mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode

    traceInstruction mnemonic mode addr addr
    status <- getCpuStatusFlag flag cpu
    when (condition status) $ do
        pc <- getCpuPC cpu
        when (pageCrossed pc addr) $
            incrementCycle cpu 1
        setCpuPC cpu addr
        incrementCycle cpu 1

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
    addr <- getJmpAddress cpu mapper mode

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

lsr :: AddressingMode -> Cpu -> Mapper -> IO ()
lsr Accumulator cpu _ = do
    value <- getCpuRegister registerA cpu
    trace "LSR A"

    let result = value `shiftR` 1
    let carry = value .&. 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

lsr mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    value <- cpuReadByte mapper addr

    traceInstruction "LSR" mode addr value

    let result = value `shiftR` 1
    let carry = value .&. 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    cpuWriteByte mapper addr result


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

rol :: AddressingMode -> Cpu -> Mapper -> IO ()
rol Accumulator cpu _ = do
    value <- getCpuRegister registerA cpu
    c <- fromIntegral . fromEnum <$> getCpuStatusFlag statusC cpu
    trace "ROL A"

    let result = (value `shiftL` 1) .|. c
    let carry = value .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

rol mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    value <- cpuReadByte mapper addr
    c <- fromIntegral . fromEnum <$> getCpuStatusFlag statusC cpu

    traceInstruction "ROR" mode addr value

    let result = (value `shiftL` 1) .|. c
    let carry = value .&. 0x80
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu


    cpuWriteByte mapper addr result

ror :: AddressingMode -> Cpu -> Mapper -> IO ()
ror Accumulator cpu _ = do
    value <- getCpuRegister registerA cpu
    c <- fromIntegral . fromEnum <$> getCpuStatusFlag statusC cpu
    trace "ROR A"

    let result = (value `shiftR` 1) .|. (c `shiftL` 7)
    let carry = value .&. 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    setCpuRegister registerA cpu result

ror mode cpu mapper = do
    addr <- getOperandAddress cpu mapper mode
    value <- cpuReadByte mapper addr
    c <- fromIntegral . fromEnum <$> getCpuStatusFlag statusC cpu

    traceInstruction "ROR" mode addr value

    let result = (value `shiftR` 1) .|. (c `shiftL` 7)
    let carry = value .&. 1
    let negative = result .&. 0x80

    setCpuStatusFlag statusC (carry /= 0) cpu
    setCpuStatusFlag statusZ (result == 0) cpu
    setCpuStatusFlag statusN (negative /= 0) cpu

    cpuWriteByte mapper addr result

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
    addr <- getStoreAddress cpu mapper mode

    traceInstruction "STA" mode addr a

    cpuWriteByte mapper addr a

stx :: AddressingMode -> Cpu -> Mapper -> IO ()
stx mode cpu mapper = do
    x <- getCpuRegister registerX cpu
    addr <- getStoreAddress cpu mapper mode

    traceInstruction "STX" mode addr x

    cpuWriteByte mapper addr x

sty :: AddressingMode -> Cpu -> Mapper -> IO ()
sty mode cpu mapper = do
    y <- getCpuRegister registerY cpu
    addr <- getStoreAddress cpu mapper mode

    traceInstruction "STY" mode addr y

    cpuWriteByte mapper addr y

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
