{-# LANGUAGE TemplateHaskell #-}

module CPU
    (
    ) where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.Word                  (Word8)

-- | todo memory interface
data CpuEnv = CpuEnv
    { _cycles    :: Int    -- ^ number of cycle
    , _pc        :: Int    -- ^ program counter
    , _sp        :: Word8  -- ^ stack pointer
    , _a         :: Word8  -- ^ accumulator
    , _x         :: Word8  -- ^ x register
    , _y         :: Word8  -- ^ y register
    , _c         :: Word8  -- ^ carry flag
    , _z         :: Word8  -- ^ zero flag
    , _i         :: Word8  -- ^ interrupt disable flag
    , _d         :: Word8  -- ^ decimal mode flag
    , _b         :: Word8  -- ^ break command flag
    , _u         :: Word8  -- ^ unused flag
    , _v         :: Word8  -- ^ overflow flag
    , _n         :: Word8  -- ^ negative flag
    , _interrupt :: Word8  -- ^ interrupt type to perform
    , _stall     :: Int    -- ^ number of cycle to stall
    }

makeLenses ''CpuEnv

type CPU = State CpuEnv ()


-- | Set the CPU at the state when the console is
-- powered-up.
resetCpu :: CPU
resetCpu = do
    pc .= 0xFFFC
    sp .= 0xFD
    setFlags 0x24

-- | Set the CPU flags to the powered-up state.
setFlags :: Word8 -> CPU
setFlags flags = do
    c .= (flags `shiftR` 0) .&. 1
    z .= (flags `shiftR` 1) .&. 1
    i .= (flags `shiftR` 2) .&. 1
    d .= (flags `shiftR` 3) .&. 1
    b .= (flags `shiftR` 4) .&. 1
    u .= (flags `shiftR` 5) .&. 1
    v .= (flags `shiftR` 6) .&. 1
    n .= (flags `shiftR` 7) .&. 1
