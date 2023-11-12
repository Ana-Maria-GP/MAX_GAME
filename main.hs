import System.Environment (getArgs)
import Control.Monad (replicateM_)
import System.Process
import System.Random


--Funcion para crear una matriz de n x n
createMatrix :: Int -> [[Char]]
createMatrix n = replicate n (replicate n '0')

-- Funcion que imprime la matriz
printMatrix :: [[Char]] -> IO ()
printMatrix matrix = mapM_ printRow matrix
  where
    printRow row = putStrLn $ unwords (map (\c -> [c]) row)

  
-- Function to generate a random number between 1 and 5 with a seed
randomNumberWithSeed :: Int -> IO ()
randomNumberWithSeed seed = do
    let generator = mkStdGen seed
        randomNumber :: Int
        randomNumber = fst $ randomR (1, 5) generator
    putStrLn $ "Random number between 1 and 5 with seed " ++ show seed ++ ": " ++ show randomNumber


-- Funcion para cambiar un elemento de la matriz a otro Char
updateMatrixElem :: Int -> Int -> Char -> [[Char]] -> [[Char]]
updateMatrixElem x y newChar matrix =
    take x matrix ++
    [take y (matrix !! x) ++ [newChar] ++ drop (y + 1) (matrix !! x)] ++
    drop (x + 1) matrix


--Funcion que crea las paredes en la matriz
applyRandomWalls :: Int -> [[Char]] -> IO [[Char]]
applyRandomWalls 0 matrix = return matrix
applyRandomWalls n matrix = do
    x <- randomRIO (0, length matrix - 1) -- Seleccion aleatoria de coordenadas dentro de la matriz
    y <- randomRIO (0, length (head matrix) - 1)
    updatedMatrix <- createWall x y 10.0 matrix  -- al aumentar el float aumenta el largo de las paredes 
    applyRandomWalls (n - 1) updatedMatrix

-- Funcion que crea las paredes
createWall :: Int -> Int -> Float -> [[Char]] -> IO [[Char]]
createWall x y threshold matrix
  | x <= 0 || y <= 0 || x >= length matrix || y >= length matrix = return matrix  -- Casos de borde para evitar out of bounds
  | otherwise = do
    let directions = ['A', 'B', 'C', 'D']   --Lista con direccionas a las que se puede ir A de Arriba, b de Abajo, c de izCkierda y D de Derecha
    let directionsLength = length directions
    directionIndex <- randomRIO (0, directionsLength - 1)   -- Seleccion aleatoria de un indice del arreglo
    putStrLn $ "Direction index: " ++ show directionIndex  -- Debug print
    let direction = directions !! directionIndex    -- Se pasa del numero random a la Letra correspondiente en el arreglo
    putStrLn $ "Chosen direction: " ++ [direction]  -- Debug print
    roll <- randomIO :: IO Float    --Se rollea un numero para ver si se crean mas paredes o no
    putStrLn $ "Roll value: " ++ show roll  -- Debug print

    if roll < threshold then -- Si el roll es menor al threshold se creara una pared en la direccion "direction"
        case direction of -- Mientras mas alto sea el Threshold mas extensas seran las paredes
            'A' -> do -- Aca empieza el bloque recursivo que crea las paredes en las direcciones random
                putStrLn "Creating wall upwards"  -- Debug print
                createWall (x - 1) y ((threshold / 3) * 2) (updateMatrixElem x y 'L' matrix) -- Threshold disminuye en un 33% por cada iteracion
            'B' -> do
                putStrLn "Creating wall downwards"  -- Debug print
                createWall (x + 1) y (threshold / 2) (updateMatrixElem x y 'L' matrix)
            'C' -> do
                putStrLn "Creating wall to the left"  -- Debug print
                createWall x (y - 1) (threshold / 2) (updateMatrixElem x y 'L' matrix)
            'D' -> do
                putStrLn "Creating wall to the right"  -- Debug print
                createWall x (y + 1) (threshold / 2) (updateMatrixElem x y 'L' matrix)
    else do -- En caso de que el roll sea < Threshold se termina la creacion de paredes
        putStrLn "No wall created"  -- Debug print
        return matrix

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sizeStr,seedStr] -> do -- Primer argumento de ejecucion corresponde al tamano de la matriz y el segundo corresponde a la seed que se quiere utilizar
            let size = read sizeStr :: Int
                seed = read seedStr :: Int
                matrix = replicate size (replicate size '0')
            updatedMatrix <- applyRandomWalls (read sizeStr) matrix  -- Aca se crean las paredes, la cantidad de paredes corresponde al tamano de la matriz
            putStrLn $ "Matriz de " ++ show size ++ "x" ++ show size 
            printMatrix updatedMatrix -- Print de la matriz
            putStrLn "Bom dia bld:"
            answer <- getLine -- Aca empiezan los inputs del usuario
            if answer /= "terminar"
                then do
                    _ <- system "clear"  -- Use "cls" on Windows
                    main
                else randomNumberWithSeed seed
        _ -> putStrLn "Para ejecutar: ./Jogo <tamano>"
