module Emulator.Components.Mapper (Mapper(..), cpuReadWord, cpuWriteWord, mapper) where

import           Data.Bits
import qualified Data.ByteString         as BS
import           Data.Word
import           Emulator.Cartridge      (Cartridge (..))
import           Emulator.Components.Ram (RAM, cpuRamSize)
import qualified Emulator.Components.Ram as Ram (readByte, writeByte)

data Mapper = Mapper
    { cpuReadByte  :: Word16 -> IO Word8
    , cpuWriteByte :: Word16 -> Word8 -> IO ()
    -- TODO: ppuRead and ppuWrite
    }

cpuReadWord :: Mapper -> Word16 -> IO Word16
cpuReadWord (Mapper readByte _) addr = do
    lo <- readByte addr
    hi <- readByte (addr + 1)
    return $ (fromIntegral hi `shiftL` 8) + fromIntegral lo

cpuWriteWord :: Mapper -> Word16 -> Word16 -> IO ()
cpuWriteWord (Mapper _ writeByte) addr word = do
    let lo = fromIntegral word
    let hi = fromIntegral word `shiftR` 8 :: Word16
    writeByte addr lo
    writeByte (addr + 1) (fromIntegral hi)

cpuRamMask :: Int
cpuRamMask = fromIntegral cpuRamSize - 1

defaultReadByte :: RAM -> (Word16 -> IO Word8) -> Word16 -> IO Word8
defaultReadByte ram mapping addr
    | addr <= 0x2000 = Ram.readByte ram (fromIntegral addr .&. cpuRamMask)
    | addr <= 0x3FFF = fail "PPU Not implemented"
    | addr <= 0x4017 = fail "APU Not implemented"
    | otherwise = mapping addr

defaultWriteByte :: RAM -> (Word16 -> Word8 -> IO ()) -> Word16 -> Word8 -> IO ()
defaultWriteByte ram mapping addr byte
    | addr <= 0x2000 = Ram.writeByte ram (fromIntegral addr .&. cpuRamMask) byte
    | addr <= 0x3FFF = fail "PPU Not implemented"
    | addr <= 0x4017 = fail "APU Not implemented"
    | otherwise = mapping addr byte

mapper :: Word8 -> RAM -> Cartridge -> Mapper
mapper 0 ram cart = Mapper (defaultReadByte ram readByte) (defaultWriteByte ram writeByte)
    where prgRom = prgRomData cart
          nrom256 = BS.length prgRom == 0x8000
          readByte addr
            | addr <= 0x7FFF = fail "PRG Ram not implemented"
            | addr <= 0xBFFF || nrom256 = pure $ BS.index prgRom (fromIntegral addr - 0x8000)
            | otherwise = pure $ BS.index prgRom (fromIntegral addr - 0xC000)
          writeByte addr _
            | 0x6000 <= addr && addr <= 0x7FFF = fail "PRGRAM Not implemented"
            | otherwise = pure ()

mapper n _ _ = error ("Unknown mapper: " ++ show n)
