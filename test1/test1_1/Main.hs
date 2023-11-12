-- app/Main.hs

module Main where

import System.Environment
import RandMap
import System.Random
import game/Main (playGame)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nStr, sStr] -> do
            let n = read nStr
                s = read sStr
            let initialPlayerPos = (1, 1)
                randomMap = generateRandomMap n s
            playGame randomMap initialPlayerPos
        _ -> putStrLn "Uso: ./max <n> <s>"
