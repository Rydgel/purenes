{-# LANGUAGE OverloadedStrings #-}

module Rom
    ( Region
    , Mirroring
    , ROMFile
    , loadROM
    ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
import           Prelude                    hiding (take)


-- | The game region. Useful to adjust framerate
-- among other things.
data Region
   = NTSC
   | PAL
    deriving (Show)

data Mirroring
   = Horizontal
   | Vertical
   | FourScreen
    deriving (Show)

-- | This is the ROM file datatype.
data ROMFile = ROMFile
    { prgBanks    :: !Word8
    , chrBanks    :: !Word8
    , mirroring   :: !Mirroring
    , battery     :: !Bool
    , trainer     :: !Bool
    , fourScreen  :: !Bool
    , vsCart      :: !Bool
    , mapper      :: !Word8 -- ^ todo Mapper ADTs etc.
    , ramBanks    :: !Word8
    , regionFlag  :: !Region
    , trainerData :: !B.ByteString
    , wramBanks   :: !(V.Vector B.ByteString)
    , romBanks    :: !(V.Vector B.ByteString)
    , vromBanks   :: !(V.Vector B.ByteString)
    } deriving (Show)

-- | A parser combinators function.
parseROMFile :: Parser ROMFile
parseROMFile = do
    _ <- string "NES\x1a"
    prg <- anyWord8
    chr <- anyWord8
    c1 <- anyWord8
    c2 <- anyWord8
    ramb <- anyWord8
    region <- anyWord8
    _ <- take 6 -- ^ the padding
    t <- parserTrainer $ trainerStatus c1
    rom <- count (fromIntegral prg) (take 16384)
    vrom <- count (fromIntegral chr) (take 8192)
    wram <- count (fromIntegral ramb) (take 8192)

    return ROMFile
      { prgBanks = prg
      , chrBanks = chr
      , mirroring = mirroringStatus c1
      , battery = batteryStatus c1
      , trainer = trainerStatus c1
      , fourScreen = fourScreenStatus c1
      , vsCart = vsCartStatus c1
      , mapper = mapperStatus c1 c2
      , ramBanks = parseRamBanks ramb
      , regionFlag = parseRegion region
      , trainerData = t
      , wramBanks = V.fromList wram
      , romBanks = V.fromList rom
      , vromBanks = V.fromList vrom
      }

parserTrainer :: Bool -> Parser B.ByteString
parserTrainer True  = take 512
parserTrainer False = pure ""

mirroringStatus :: Word8 -> Mirroring
mirroringStatus c1
  | c1 .&. 0x01 /= 0x00 = Vertical
  | c1 .&. 0x08 /= 0x00 = FourScreen
  | otherwise           = Horizontal

-- | Get battery status from the two control bits.
batteryStatus :: Word8  -> Bool
batteryStatus c1 = (c1 `shiftR` 0x01) .&. 0x01 /= 0x00

trainerStatus :: Word8 -> Bool
trainerStatus c1 = c1 .&. 0x04 == 0x04

fourScreenStatus :: Word8 -> Bool
fourScreenStatus c1 = c1 .&. 0x08 == 0x08

vsCartStatus :: Word8 -> Bool
vsCartStatus c2 = c2 `shiftR` 0x01 /= 0x00

mapperStatus :: Word8 -> Word8 -> Word8
mapperStatus c1 c2 = (c1 `shiftR` 0x04) .|. c2

parseRamBanks :: Word8 -> Word8
parseRamBanks 0x00 = 0x01
parseRamBanks w    = w

parseRegion :: Word8 -> Region
parseRegion w
  | w .&. 0x01 /= 0x00 = PAL
  | otherwise          = NTSC

-- | This function loads a .nes file into memory.
-- loadROM :: String -> IO ROMFile
loadROM :: String -> IO ()
loadROM path = do
    fileStr <- B.readFile path
    case parseOnly parseROMFile fileStr of
      Left e -> print e -- parsing error
      Right is -> print is
