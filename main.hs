import System.Environment (getArgs)
import Control.Monad (replicateM_)
import System.Process

-- Function to print a single row
printRow :: Int -> IO ()
printRow m = putStrLn $ unwords (replicate m "■")

-- Function to print the matrix
printMatrix :: Int -> IO ()
printMatrix m = replicateM_ m (printRow m)

getUserMessage :: IO ()
getUserMessage = do
    putStrLn "Enter a message:"
    userMessage <- getLine
    putStrLn $ "User message: " ++ userMessage

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
                else putStrLn "Boa noite bld"
        _ -> putStrLn "Para ejecutar: ./Jogo <tamano>"
