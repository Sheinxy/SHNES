module Emulator.Components.Mapper (Mapper(..), readWord, writeWord, mapper) where

import           Data.Bits
import qualified Data.ByteString         as BS
import           Data.Word
import           Emulator.Cartridge      (Cartridge (..))
import           Emulator.Components.Ram
import qualified Emulator.Components.Ram as Ram (readByte, writeByte)

data Mapper = Mapper
    { readByte  :: Word16 -> IO Word8
    , writeByte :: Word16 -> Word8 -> IO ()
    }

readWord :: Mapper -> Word16 -> IO Word16
readWord (Mapper readByte' _) addr = do
    lo <- readByte' addr
    hi <- readByte' (addr + 1)
    return $ (fromIntegral hi `shiftL` 8) + fromIntegral lo

writeWord :: Mapper -> Word16 -> Word16 -> IO ()
writeWord (Mapper _ writeByte') addr word = do
    let (lo, hi) = (fromIntegral word .&. 0xFF, fromIntegral word `shiftR` 8)
    writeByte' addr lo
    writeByte' (addr + 1) hi

defaultReadByte :: RAM -> (Word16 -> IO Word8) -> Word16 -> IO Word8
defaultReadByte ram mapping addr
    | addr <= 0x2000 = Ram.readByte ram (fromIntegral addr .&. 0x7FF)
    | addr <= 0x3FFF = fail "PPU Not implemented"
    | addr <= 0x4017 = fail "APU Not implemented"
    | otherwise = mapping addr

defaultWriteByte :: RAM -> (Word16 -> Word8 -> IO ()) -> Word16 -> Word8 -> IO ()
defaultWriteByte ram mapping addr byte
    | addr <= 0x2000 = Ram.writeByte ram (fromIntegral addr .&. 0x7FF) byte
    | addr <= 0x3FFF = fail "PPU Not implemented"
    | addr <= 0x4017 = fail "APU Not implemented"
    | otherwise = mapping addr byte

mapper :: Word8 -> RAM -> Cartridge -> Mapper
mapper 0 ram cart = Mapper (defaultReadByte ram readByte') (defaultWriteByte ram writeByte')
    where prgRom = prgRomData cart
          nrom256 = BS.length prgRom == 0x8000
          readByte' addr
            | addr <= 0x7FFF = fail "PRG Ram not implemented"
            | addr <= 0xBFFF || nrom256 = pure $ BS.index prgRom (fromIntegral addr - 0x8000)
            | otherwise = pure $ BS.index prgRom (fromIntegral addr - 0xC000)
          writeByte' addr _
            | 0x6000 <= addr && addr <= 0x7FFF = fail "PRGRAM Not implemented"
            | otherwise = pure ()

mapper n _ _ = error ("Unknown mapper: " ++ show n)
