import System.Environment (getArgs)
import Control.Monad (replicateM_)
import System.Process
import System.Random


-- Function to print a single row
printRow :: Int -> IO ()
printRow m = putStrLn $ unwords (replicate m "■")

-- Function to print the matrix
printMatrix :: Int -> IO ()
printMatrix m = replicateM_ m (printRow m)

-- Function to generate a random number between 1 and 5 with a seed
randomNumberWithSeed :: Int -> IO ()
randomNumberWithSeed seed = do
    let generator = mkStdGen seed
        randomNumber :: Int
        randomNumber = fst $ randomR (1, 5) generator
    putStrLn $ "Random number between 1 and 5 with seed " ++ show seed ++ ": " ++ show randomNumber


main :: IO ()
main = do
    args <- getArgs
    case args of
        [sizeStr] -> do
            let size = read sizeStr :: Int
            putStrLn $ "Matriz de " ++ show size ++ "x" ++ show size 
            printMatrix size
            putStrLn "Bom dia bld:"
            answer <- getLine
            if answer /= "terminar"
                then do
                    _ <- system "clear"  -- Use "cls" on Windows
                    main
                else randomNumberWithSeed 420
        _ -> putStrLn "Para ejecutar: ./Jogo <tamano>"
