import System.Random

-- Función para generar una lista aleatoria de n elementos entre a y b
generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList n a b = do
    gen <- getStdGen
    let randomValues = take n (randomRs (a, b) gen)
    return randomValues

main :: IO ()
main = do
    putStrLn "Generando dos listas aleatorias de 5 elementos cada una:"
    list1 <- generateRandomList 5 1 10  -- Lista aleatoria de 5 elementos entre 1 y 10
    list2 <- generateRandomList 5 (-5) 5  -- Lista aleatoria de 5 elementos entre -5 y 5
    putStrLn "Lista 1: "
    print list1
    putStrLn "Lista 2: "
    print list2