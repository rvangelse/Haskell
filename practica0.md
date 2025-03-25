# Práctica 0 

## 1. 

```haskell
-- Lista vacia?
null :: [a] -> Bool 

-- Devuelve el primer elemento de la lista
head :: [a] -> a 

-- Devuelve toda la lista, excepto su primer elemento
tail :: [a] -> [a] 

-- Devuelve toda la lista, excepto su último elemento
init :: [a] -> [a] 

-- Devuelve el último elemento de la lista
last :: [a] -> a 

-- Devuelve una lista con los primeros n-elementos de la lista original
take :: Int -> [a] -> [a] 

-- Devuelve una lista sin los primeros n-elementos de la lista original
drop :: Int -> [a] -> [a]

-- Devuelve la concatenación de las dos listas
(++) :: [a] -> [a] -> [a]

-- Toma una lista de listas y concatena sus elementos en una sola lista
concat :: [[a]] -> [a]

-- Invierte el orden de los elementos de una lista
reverse :: [a] -> [a]

-- Es el pertenece
elem :: Eq a ==> a -> [a] -> Bool
-- OJO: Es importante que a pertenezca a la clase Eq, es lo que permite comparar por igualdad 
```
## 2. 

### a.

```haskell
valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0 = x 
                | otherwise =  -x
```
### b. 

```haskell
bisiesto :: Int -> Bool
bisiesto año | mod año 4 /= 0 = False
             | mod año 100 /= 0 = True
             | mod año 400 == 0 = True
             | otherwise = False
```
### c.

```haskell
factorial :: Int -> Int
factorial n | n == 1 || n == 0 = 1
            | otherwise = n * factorial (n-1)
```
### d. 
```haskell
divisoresDe :: Int -> [Int]
divisoresDe n = [x | x <- [1..n], mod n x == 0] -- Listas por comprensión 

esPrimo :: Int -> Bool
esPrimo n = length (divisoresDe n) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [x | x <- divisoresDe n, esPrimo x] 
```
## 3.
```haskell
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b 

inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing 
          | otherwise = Just (1 / x) 

-- Si es Left algo, devuelve ese algo y si es right algo, lo evalua como Bool
aEntero :: Either Int Bool -> Int  
aEntero (Left a)  = a 
aEntero (Right b) = if b then 1 else 0 
```
## 4. 

```haskell
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
-- all devuelve true, si todos los elementos de la lista cumplen el predicado
```
OJO: `String` es un renombre de `[Char]`

## 5. 
```haskell
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil 
negacionAB (Bin I v D) = Bin (negacionAB I) (not v) (negacionAB D)

productoAB :: AB Int -> Int 
productoAB Nil = 1
productoAB (Bin I v D) = v * (productoAB I) * (productoAB D)
```
