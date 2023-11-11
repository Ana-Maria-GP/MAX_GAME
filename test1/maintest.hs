
-- Tamaño del mapa
mapSize :: Int
mapSize = 10

-- Mapa predeterminado
defaultMap :: [[Char]]
defaultMap =
    [ "##########"
    , "#@       #"
    , "#        #"
    , "# ##  ####"
    , "#        #"
    , "#   #### #"
    , "####     #"
    , "#   #####X"
    , "#        #"
    , "##########"
    ]

-- Función para imprimir el mapa
printMap :: [[Char]] -> IO ()
printMap = mapM_ putStrLn

-- Tipo para representar la posición del jugador
type Position = (Int, Int)

-- Definición de tipos
data Direction = Up | Down | Left | Right | Quit deriving (Eq)

{-Funcion que Actualiza TEST001 -}
-- Función para actualizar el mapa con la nueva posición del jugador
updateMap :: [[Char]] -> Position -> [[Char]]
updateMap gameMap (newX, newY) = do
    let newRow = replaceAtIndex newX '@' (gameMap !! newY)
    let updatedMap = replaceAtIndex newY newRow gameMap
    -- Restaurar la posición anterior con un espacio en blanco
    let (x, y) = playerPos
    let prevRow = replaceAtIndex x ' ' (updatedMap !! y)
    replaceAtIndex y prevRow updatedMap


-- Función para mover al jugador en una dirección
movePlayer :: Position -> Direction -> Position
movePlayer (x, y) dir = case dir of
    Up    -> (x, max 0 (y - 1))
    Down  -> (x, min (mapSize - 1) (y + 1))
    Main.Left  -> (max 0 (x - 1), y)
    Main.Right -> (min (mapSize - 1) (x + 1), y)
    Quit  -> (x, y)

{-Funcion que Actualiza y a la vez deja el camino
-- Función para actualizar el mapa con la nueva posición del jugador
updateMap :: [[Char]] -> Position -> [[Char]]
updateMap gameMap (x, y) =
    let (row1, playerRow:row2) = splitAt y gameMap
        (left, _:right) = splitAt x playerRow
    in row1 ++ [left ++ ['@'] ++ right] ++ row2
    -- Función principal para jugar
    
-}
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
            _   -> error "Entrada no válida"
    -- Consume el carácter de salto de línea
    _ <- getChar
    if dir == Quit
        then putStrLn "¡Hasta la próxima!"
        else do
            let newPlayerPos = movePlayer playerPos dir
            if isValidMove gameMap newPlayerPos
                then do
                    let newGameMap = updateMap gameMap newPlayerPos
                    if gameMap !! snd newPlayerPos !! fst newPlayerPos == 'X'
                        then do
                            printMap newGameMap
                            putStrLn "¡Encontraste el tesoro! ¡Ganaste!"
                        else playGame newGameMap newPlayerPos
                else do
                    putStrLn "¡Te has chocado con una pared! ¡Perdiste!"
                    putStrLn "Juego terminado."


-- Función para verificar si el movimiento es válido
isValidMove :: [[Char]] -> Position -> Bool
isValidMove gameMap (x, y) =
    x >= 0 && x < mapSize && y >= 0 && y < mapSize && gameMap !! y !! x /= '#'



main :: IO ()
main = do
    putStrLn "Bienvenido al juego 2D"
    let initialPlayerPos = (1, 1)  -- Posición inicial del jugador
    playGame defaultMap initialPlayerPos
