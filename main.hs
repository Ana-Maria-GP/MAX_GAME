import System.Environment (getArgs)
import Control.Monad (replicateM_)
import System.Process
import System.Random


-- Function to create a matrix of size n
createMatrix :: Int -> [[Char]]
createMatrix n = replicate n (replicate n '0')

-- Function to print a matrix
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

updateMatrixElem :: Int -> Int -> Char -> [[Char]] -> [[Char]]
updateMatrixElem x y newChar matrix =
    take x matrix ++
    [take y (matrix !! x) ++ [newChar] ++ drop (y + 1) (matrix !! x)] ++
    drop (x + 1) matrix

applyRandomWalls :: Int -> [[Char]] -> IO [[Char]]
applyRandomWalls 0 matrix = return matrix
applyRandomWalls n matrix = do
    x <- randomRIO (0, length matrix - 1)
    y <- randomRIO (0, length (head matrix) - 1)
    updatedMatrix <- createWall x y 2.0 matrix  -- Adjust the threshold as needed
    applyRandomWalls (n - 1) updatedMatrix

-- Function to create a wall
createWall :: Int -> Int -> Float -> [[Char]] -> IO [[Char]]
createWall x y threshold matrix
  | x <= 0 || y <= 0 || x >= length matrix || y >= length matrix = return matrix  -- Stop recursion if x or y is non-positive
  | otherwise = do
    let directions = ['A', 'B', 'C', 'D']
    let directionsLength = length directions
    directionIndex <- randomRIO (0, directionsLength - 1)
    putStrLn $ "Direction index: " ++ show directionIndex  -- Debug print
    let direction = directions !! directionIndex
    putStrLn $ "Chosen direction: " ++ [direction]  -- Debug print
    roll <- randomIO :: IO Float
    putStrLn $ "Roll value: " ++ show roll  -- Debug print

    if roll < threshold then
        case direction of
            'A' -> do
                putStrLn "Creating wall upwards"  -- Debug print
                createWall (x - 1) y ((threshold / 3) * 2) (updateMatrixElem x y 'L' matrix)
            'B' -> do
                putStrLn "Creating wall downwards"  -- Debug print
                createWall (x + 1) y (threshold / 2) (updateMatrixElem x y 'L' matrix)
            'C' -> do
                putStrLn "Creating wall to the left"  -- Debug print
                createWall x (y - 1) (threshold / 2) (updateMatrixElem x y 'L' matrix)
            'D' -> do
                putStrLn "Creating wall to the right"  -- Debug print
                createWall x (y + 1) (threshold / 2) (updateMatrixElem x y 'L' matrix)
    else do
        putStrLn "No wall created"  -- Debug print
        return matrix

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sizeStr,seedStr] -> do
            let size = read sizeStr :: Int
                seed = read seedStr :: Int
                matrix = replicate size (replicate size '0')
            updatedMatrix <- applyRandomWalls (read sizeStr) matrix  -- Replace 5 with the number of walls you want to create
            putStrLn $ "Matriz de " ++ show size ++ "x" ++ show size 
            printMatrix updatedMatrix
            putStrLn "Bom dia bld:"
            answer <- getLine
            if answer /= "terminar"
                then do
                    _ <- system "clear"  -- Use "cls" on Windows
                    main
                else randomNumberWithSeed seed
        _ -> putStrLn "Para ejecutar: ./Jogo <tamano>"
