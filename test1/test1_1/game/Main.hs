
-- game/Main.hs

module Main where

import RandMap
import System.Random

-- Función para imprimir el mapa
printMap :: [[Char]] -> IO ()
printMap = mapM_ putStrLn

-- Tipo para representar la posición del jugador
type Position = (Int, Int)

-- Definición de tipos
data Direction = Up | Down | Left | Right | Quit deriving (Eq)

-- Función para mover al jugador en una dirección
movePlayer :: Position -> Direction -> Position
movePlayer (x, y) dir = case dir of
    Up    -> (x, max 0 (y - 1))
    Down  -> (x, min (mapSize - 1) (y + 1))
    Left  -> (max 0 (x - 1), y)
    Right -> (min (mapSize - 1) (x + 1), y)
    Quit  -> (x, y)

-- Función para actualizar el mapa con la nueva posición del jugador
updateMap :: [[Char]] -> Position -> [[Char]]
updateMap gameMap (x, y) =
    let (row1, playerRow:row2) = splitAt y gameMap
        (left, _:right) = splitAt x playerRow
    in row1 ++ [left ++ ['@'] ++ right] ++ row2

-- Función para verificar si el movimiento es válido
isValidMove :: [[Char]] -> Position -> Bool
isValidMove gameMap (x, y) =
    x >= 0 && x < mapSize && y >= 0 && y < mapSize && gameMap !! y !! x /= 'L' && gameMap !! y !! x /= '$'

-- Función principal para jugar
playGame :: [[Char]] -> Position -> IO ()
playGame gameMap playerPos = do
    printMap gameMap
    putStrLn "Mueve al jugador con las teclas W (arriba), A (izquierda), S (abajo), D (derecha) o Q (salir): "
    input <- getChar
    let dir = case input of
            'W' -> Up
            'A' -> Left
            'S' -> Down
            'D' -> Right
            'Q' -> Quit
            _   -> error "Error de caracter"
    -- Consume el carácter de salto de línea
    _ <- getChar
    if dir == Quit
        then putStrLn "¡Hasta la vista baby!"
        else do
            let newPlayerPos = movePlayer playerPos dir
            if isValidMove gameMap newPlayerPos
                then do
                    let newGameMap = updateMap gameMap newPlayerPos
                    if gameMap !! snd newPlayerPos !! fst newPlayerPos == 'X'
                        then do
                            printMap newGameMap
                            putStrLn "¡Hay premio!"
                        else playGame newGameMap newPlayerPos
                else do
                    putStrLn "¡Oh no! Movimiento no válido"
                    putStrLn "Juego terminado."

main :: IO ()
main = do
    putStrLn "Bienvenido al mejor juego que creé en mi vida de Haskell"
    let initialPlayerPos = (1, 1)  -- Punto de partida
    playGame defaultMap initialPlayerPos
