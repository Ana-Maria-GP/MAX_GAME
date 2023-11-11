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

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sizeStr,seedStr] -> do
            let size = read sizeStr :: Int
                seed = read seedStr :: Int
                matrix = replicate size (replicate size '0')
                updatedMatrixElem = updateMatrixElem 2 3 'A' matrix
            putStrLn $ "Matriz de " ++ show size ++ "x" ++ show size 
            printMatrix updatedMatrixElem
            putStrLn "Bom dia bld:"
            answer <- getLine
            if answer /= "terminar"
                then do
                    _ <- system "clear"  -- Use "cls" on Windows
                    main
                else randomNumberWithSeed seed
        _ -> putStrLn "Para ejecutar: ./Jogo <tamano>"
