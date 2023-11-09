module MaxGame (
    Cell(..),
    World(..),
    generateWorld,
    moveCharacter,
    isGameOver
) where

import System.Random
import System.IO

data Cell = Walkable | Obstacle | Lava | Character | Treasure deriving (Eq, Show)

data World = World { gridSize :: Int, grid :: [[Cell]], characterPos :: (Int, Int), treasurePos :: (Int, Int) }

-- Función para generar un mundo aleatorio
generateWorld :: Int -> Int -> IO World
generateWorld n seed = do
    gen <- newStdGen
    let randomGrid = take n $ map (take n) $ randomRs (0, 2) gen :: [[Int]]
    let cellGrid = map (map convertToCell) randomGrid
    let (characterRow, characterCol) = findWalkableCell randomGrid
    let (treasureRow, treasureCol) = findWalkableCell randomGrid
    return $ World n cellGrid (characterRow, characterCol) (treasureRow, treasureCol)

-- Función para convertir números aleatorios en tipos de celda
convertToCell :: Int -> Cell
convertToCell 0 = Walkable
convertToCell 1 = Obstacle
convertToCell 2 = Lava
convertToCell _ = error "Invalid cell type"

-- Función para encontrar una celda caminable en la grilla
findWalkableCell :: [[Int]] -> (Int, Int)
findWalkableCell grid =
    let n = length grid
        gen = mkStdGen $ head $ randoms (mkStdGen 42) :: StdGen
        randomRow = head $ randomRs (0, n-1) gen
        randomCol = head $ randomRs (0, n-1) (snd $ split gen)
    in if (grid !! randomRow) !! randomCol == 0
        then (randomRow, randomCol)
        else findWalkableCell grid

-- Función para mover al personaje
moveCharacter :: World -> Char -> World
moveCharacter world direction =
    case direction of
        'W' -> tryMove (-1, 0) world
        'A' -> tryMove (0, -1) world
        'S' -> tryMove (1, 0) world
        'D' -> tryMove (0, 1) world
        _   -> world

-- Función auxiliar para intentar mover al personaje en la dirección especificada
tryMove :: (Int, Int) -> World -> World
tryMove (dr, dc) world@(World n grid (row, col) treasurePos) =
    let newRow = row + dr
        newCol = col + dc
    in if isValidMove newRow newCol n grid
        then World n grid (newRow, newCol) treasurePos
        else world

-- Función auxiliar para verificar si el movimiento es válido
isValidMove :: Int -> Int -> Int -> [[Cell]] -> Bool
isValidMove row col n grid =
    row >= 0 && col >= 0 && row < n && col < n && (grid !! row) !! col == Walkable

-- Función para verificar si el juego ha terminado
isGameOver :: World -> Bool
isGameOver (World _ _ characterPos treasurePos) = characterPos == treasurePos
