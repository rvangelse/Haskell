valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x 
                | otherwise =  -x

bisiesto :: Int -> Bool
bisiesto año | mod año 4 /= 0 = False
             | mod año 100 /= 0 = True
             | mod año 400 == 0 = True
             | otherwise = False

factorial :: Int -> Int
factorial n | n == 1 || n == 0 = 1
            | otherwise = n * factorial (n-1)

divisoresDe :: Int -> [Int]
divisoresDe n = [x | x <- [1..n], mod n x == 0]

esPrimo :: Int -> Bool
esPrimo n = length (divisoresDe n) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [x | x <- divisoresDe n, esPrimo x]

inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing 
          | otherwise = Just (1 / x) 

aEntero :: Either Int Bool -> Int
aEntero (Left a)  = a
aEntero (Right b) = if b then 1 else 0 

limpiar :: [Char] -> [Char] -> [Char]
limpiar (x:xs) [] = []
limpiar (x:xs) (y:ys) | elem y (x:xs) = limpiar (x:xs) ys
                      | otherwise = y : limpiar (x:xs) ys    

promedio :: [Float] -> Float
promedio (x:xs) = sum (x:xs) / fromIntegral (length (x:xs))

difPromedio :: [Float]-> [Float]
difPromedio xs =  map (\x -> x - promedio xs) xs

todosIguales :: [Int] -> Bool 
todosIguales xs = all (\x -> x == head xs) xs

data AB a = Nil | Bin (AB a) a (AB a) deriving Show
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq b der) = Bin (negacionAB izq) (not b) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1 
productoAB (Bin izq x der) = x * productoAB izq * productoAB der

printAB :: (Show a) => AB a -> IO ()
printAB Nil = putStr "Nil"
printAB (Bin izq x der) = putStr $ "(" ++ show (Bin izq x der) ++ ")"

main :: IO ()
main = do
  let arbol = Bin (Bin Nil True Nil) False (Bin Nil True Nil)
  let arbolNegado = negacionAB arbol
  putStrLn "Árbol original:"
  printAB arbol
  putStrLn "" 
  putStrLn "Árbol negado:" 
  printAB arbolNegado
  putStrLn ""

 









