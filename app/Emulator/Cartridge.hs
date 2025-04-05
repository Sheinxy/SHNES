{-# LANGUAGE BinaryLiterals #-}
module Emulator.Cartridge (Cartridge(..), Header(..), parseCartridge) where

import           Control.Monad   (when)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Word

data Cartridge = Cartridge
    { header     :: Header
    , prgRomData :: BS.ByteString
    , chrRomData :: BS.ByteString
    } deriving (Show)

parseCartridge :: Get Cartridge
parseCartridge = do
    header' <- parseHeader

    when (hasTrainer header') $
        skip 512

    let prgRomLength = 16384 * fromIntegral (prgRomSize header')
    prgRomData' <- getByteString prgRomLength

    let chrRomLength = 8192 * fromIntegral (chrRomSize header')
    chrRomData' <- getByteString chrRomLength

    return $ Cartridge header' prgRomData' chrRomData'

data Header = Header
    { magicValue              :: Word32 -- 4 bytes, Constant $4E $45 $53 $1A (ASCII "NES" followed by MS-DOS end-of-file)
    , prgRomSize              :: Word8 -- 1 byte, Size of PRG ROM in 16 KB units
    , chrRomSize              :: Word8 -- 1 byte, Size of CHR ROM in 8 KB units (value 0 means the board uses CHR RAM)
    -- Flags 6 – Mapper, mirroring, battery, trainer
    , isHorizontalArrangement :: Bool -- 1 bit
    , isBatteryBacked         :: Bool -- 1 bit
    , hasTrainer              :: Bool -- 1 bit
    , useAltNametableLayout   :: Bool -- 1 bit
    , mapperNumber            :: Word8 -- 1 byte, split between flags 6 and 7
    -- Flags 7 – Mapper, VS/Playchoice, NES 2.0
    , isVsUnisystem           :: Bool -- 1 bit
    , nesFormatFlag           :: Word8 -- 2 bit, If equal to 2, flags 8-15 are in NES 2.0 format
    -- Flags 8 – PRG-RAM size (rarely used extension)
    , prgRamSize              :: Word8 -- 1 byte
    -- Flags 9 – TV system (rarely used extension)
    , isPal                   :: Bool -- 1 bit
    } deriving (Show)

parseHeader :: Get Header
parseHeader = do
    magicValue' <- getWord32be

    when (magicValue' /= 0x4E45531A) $
        fail "Invalid magic value"

    prgRomSize' <- getWord8
    chrRomSize' <- getWord8

    flags6 <- getWord8
    let isHorizontalArrangement' = testBit flags6 0
        isBatteryBacked'         = testBit flags6 1
        hasTrainer'              = testBit flags6 2
        useAltNametableLayout'   = testBit flags6 3
        lowerNybbleMapper       = shiftR  flags6 4

    flags7 <- getWord8
    let isVsUnisystem'     = testBit flags7 0
        nesFormatFlag'     = (flags7 `shiftR` 2) .&. 0b11
        upperNybbleMapper = shiftR flags7 4

        mapperNumber' = (upperNybbleMapper `shiftL` 4) .|.
                       (lowerNybbleMapper .&. 0x0F)

    prgRamSize' <- getWord8
    flags9 <- getWord8
    let isPal' = testBit flags9 0
    _ <- getWord8  -- Read and ignore flags10
    _ <- getWord8  -- Read and ignore byte 11
    _ <- getWord32le -- Read and ignore bytes 12-15

    return $ Header
             magicValue'
             prgRomSize'
             chrRomSize'
             isHorizontalArrangement'
             isBatteryBacked'
             hasTrainer'
             useAltNametableLayout'
             mapperNumber'
             isVsUnisystem'
             nesFormatFlag'
             prgRamSize'
             isPal'

