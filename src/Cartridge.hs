module Cartridge
    ( Cartridge
    , initSRAM
    ) where

import qualified Data.ByteString as B
import           Data.Serialize  (encode)
import           Data.Word       (Word8)


data Cartridge = Cartridge
    { prg     :: !B.ByteString
    , chr     :: !B.ByteString
    , sram    :: !B.ByteString
    , mapper  :: !Word8
    , mirror  :: !Word8
    , battery :: !Word8
    } deriving (Show)


initSRAM :: B.ByteString
initSRAM = encode (0x2000 :: Integer)
