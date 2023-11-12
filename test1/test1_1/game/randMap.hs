-- RandMap.hs

module RandMap (generateRandomMap) where

import System.Random

-- Tamaño del mapa
mapSize :: Int
mapSize = 10

-- Genera un mapa aleatorio dado un tamaño (n) y una semilla (s)
generateRandomMap :: Int -> Int -> [[Char]]
generateRandomMap n seed = take n $ randomRs ('L', ' ') (mkStdGen seed)