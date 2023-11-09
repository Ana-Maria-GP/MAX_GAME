import System.Environment
import System.IO
import MaxGame

main :: IO ()
main = do
    args <- getArgs
    case args of
        [size, seed] -> do
            let n = read size :: Int
                s = read seed :: Int
            world <- generateWorld n s
            playGame world
        _ -> putStrLn "Uso: ./max <n> <s>"

-- Función principal para jugar
playGame :: World -> IO ()
playGame world = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    displayWorld world
    putStrLn "Ingrese una acción de movimiento -> W, A, S, D, o R"
    action <- getChar
    if action == 'R'
        then do
            newWorld <- generateWorld (gridSize world) 42 -- Usar una nueva semilla o la misma según tus necesidades
            playGame newWorld
        else do
            let newWorld = moveCharacter world action
            if isGameOver newWorld
                then putStrLn "¡Felicidades! Has encontrado el tesoro."
                else playGame newWorld

-- Función para mostrar el mundo en el terminal
displayWorld :: World -> IO ()
displayWorld (World n grid characterPos _) = do
    putStrLn $ replicate (n * 3 + 2) '-'
    mapM_ (\row -> do
        putStr "| "
        mapM_ (\cell -> putStr $ cellToChar cell ++ " ") row
        putStrLn "|"
    ) grid
    putStrLn $ replicate (n * 3 + 2) '-'

-- Función auxiliar para convertir tipos de celda a caracteres
cellToChar :: Cell -> String
cellToChar Walkable  = " "
cellToChar Obstacle  = "L"
cellToChar Lava      = "$"
cellToChar Character = "@"
cellToChar Treasure  = "X"
