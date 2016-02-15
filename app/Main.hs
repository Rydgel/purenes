module Main where

import           Rom (loadROM)
import           Cpu (resetCpu, initCpu)
import           Control.Monad.State.Strict

main :: IO ()
main = print $ execState resetCpu initCpu
