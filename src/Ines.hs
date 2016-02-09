module Ines
    ( loadINESFile
    ) where

import           Control.Applicative
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import           Data.Bits
import qualified Data.ByteString            as B
import           Data.Serialize             (decode, encode)
import           Data.Word                  (Word8)

import           Cartridge                  (Cartridge, initSRAM)


iNESFileMagic :: Integer
iNESFileMagic = 0x1a53454e


data INESFileHeader = INESFileHeader
    { magic    :: !Integer      -- ^ iNES magic number
    , numPRG   :: !Word8        -- ^ number of PRG-ROM banks (16KB each)
    , numCHR   :: !Word8        -- ^ number of CHR-ROM banks (8KB each)
    , control1 :: !Word8        -- ^ control bits
    , control2 :: !Word8        -- ^ control bits
    , numRAM   :: !Word8        -- ^ PRG-RAM size (x 8KB)
    , padding  :: !B.ByteString -- ^ unused padding
    } deriving (Show)


-- | parseINESFileHeader parses a .nes header
parseINESFileHeader :: Parser INESFileHeader
parseINESFileHeader = INESFileHeader
    <$> fmap fromIntegral anyWord32le
    <*> anyWord8
    <*> anyWord8
    <*> anyWord8
    <*> anyWord8
    <*> anyWord8
    <*> A.take 7
    -- <*  takeByteString


-- | LoadNESFile reads an iNES file (.nes) and returns a Cartridge on success.
-- http://wiki.nesdev.com/w/index.php/INES
-- http://nesdev.com/NESDoc.pdf (page 28)
loadINESFile :: String -> IO ()
loadINESFile path = do
    fileStr <- B.readFile path
    case parseOnly parseINESFileHeader fileStr of
      Left e -> print e -- parsing error
      Right is -> print is
