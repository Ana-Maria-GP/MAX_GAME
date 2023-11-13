import System.Environment
import System.Random

--seccion de mapa
randomChar :: Int -> Int -> Int -> IO Char
randomChar row col numRows = do
  let isEdge = row == 0 || row == numRows - 1 || col == 0 || col == numRows - 1
  rand <- randomRIO (0, 1 :: Int)
  return $ case (rand, isEdge) of
    (0, _) -> 'L'
    (1, _) -> ' '
    
generateRandomMap :: Int -> IO [[Char]]
generateRandomMap numRows = do
  let rowIndices = [0 .. numRows - 1]
      colIndices = [0 .. numRows - 1]  -- Mismo número de columnas que de filas
  rows <- sequence
    [ do
        row <- sequence
          [ do
              char <- randomChar row col numRows
              return char
          | col <- colIndices
          ]
        return row
    | row <- rowIndices
    ]
  return rows

-- Tamaño del mapa test
mapSize :: Int
mapSize = 10

-- Mapa predeterminado test
defaultMap :: [[Char]]
defaultMap =
    [ "LLLLLLLLLL"  
    , "L@       L"
    , "L        L"
    , "L LL $LLLL"
    , "L        L"
    , "L   LLLL L"
    , "LL L     L"
    , "L   L L$LL"
    , "L       XL"
    , "LLLLLLLLLL"
    ]

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
    Main.Left  -> (max 0 (x - 1), y)
    Main.Right -> (min (mapSize - 1) (x + 1), y)
    Quit  -> (x, y)

-- Función para actualizar el mapa con la nueva posición del jugador
updateMap :: [[Char]] -> Position -> [[Char]]
updateMap gameMap (x, y) =
    let (row1, playerRow:row2) = splitAt y gameMap
        (left, _:right) = splitAt x playerRow
    in row1 ++ [left ++ ['@'] ++ right] ++ row2

--Función principal para jugar    

playGame :: [[Char]] -> Position -> IO ()
playGame gameMap playerPos = do
    printMap gameMap
    putStrLn "Mueve al jugador con las teclas W (arriba), A (izquierda), S (abajo), D (derecha) o Q (salir): "
    input <- getChar
    let dir = case input of
            'W' -> Up
            'A' -> Main.Left
            'S' -> Down
            'D' -> Main.Right
            'Q' -> Quit
            _   -> error "error de caracter"
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
                            putStrLn "¡Hay premio ¡"
                        else playGame newGameMap newPlayerPos
                else do
                    putStrLn "¡. Se Equivoco"
                    putStrLn "Juego terminado."


-- Función para verificar si el movimiento es válido
isValidMove :: [[Char]] -> Position -> Bool
isValidMove gameMap (x, y) =
    x >= 0 && x < mapSize && y >= 0 && y < mapSize && gameMap !! y !! x /= 'L' && gameMap !! y !! x /= '$' 


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Bienvenido "
    let numRows = if length args > 0 then read (head args) else 20
    randomMap <- generateRandomMap numRows
    let initialPlayerPos = (1, 1)  -- Punto Partida    
    playGame randomMap initialPlayerPos
------



{-

  
  mapM_ putStrLn randomMap
-}