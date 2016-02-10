module Main where

import Rom (loadROM)

main :: IO ()
main = loadROM "test/roms/croom.nes"
