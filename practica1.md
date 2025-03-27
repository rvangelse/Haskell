# Practica 1: Programación funcional

## 1. 

```haskell
max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y 
--Version Curryficada
max2 :: Float -> (Float -> Float)
max2 x y | x >= y = x
         | otherwise = y 
```
```haskell
normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)

--Version Curryficada
normaVectorial :: Float -> (Float -> Float)
normaVectorial x y = sqrt(x^2 + y^2)
```
```haskell
-- OJO: flip :: (a -> b -> c) -> b -> a -> c Pide una función
--      (-) :: Float -> Float -> Float Le di la función (-)
subtract :: Float -> Float -> Float
subtract = flip(-)
```
```haskell
--OJO: subtract :: Float -> Float -> Float Pide 2 Floats
--     1 :: Float Le pasé un Float
predecesor :: Float -> Float
predecesor = subtract 1
```
```haskell
--OJO: Como le paso una funcion y le pase un Float "0", me devuelve "b"
evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0 
```
```haskell
--OJO: Le paso una función y un parámetro del tipo "a"
dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f
```
```haskell
-- OJO: map :: (a -> b) -> [a] -> [b]
--      flip :: (a -> b -> c) -> (b -> a -> c) 
flipAll :: [(a -> b -> c)] -> [b -> a -> c]
flipAll = map flip
```
```haskell
--OJO:  flip :: (a -> b -> c) -> (b -> a -> c)
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
```
## 2. 
```haskell
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b) --curry f = \a b -> f(a, b) (Versión usando notación "Lambda")

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b 
```
OJO: `curryN`  no se puede definir, puedes definir curry2, curry3, curry4, etc. Pero, no curryN, ya que Haskell necesita saber `exactamente` el número de argumentos al tipar, debe ser estático, `no variable`.

## 3. 
### I.

```haskell
-- La idea es que se vayan sumando uno a los elementos desde la cabeza de la lista 
-- Y que cuando ya no queden elementos se sume 0 para cortar la recursión
sum :: Num a => [a] -> a
sum = foldr (+) 0
```
```haskell
-- La idea es ir comparando uno a uno los elementos de (y:ys) con x e ir construyendo una cadena de ORs
-- Si en algun momento se genera un True, el resultado final será True 
elem :: Eq a => a -> [a] -> Bool
elem x = foldr(\y rec -> (y == x) || rec) False
```
```haskell
-- La idea es ir agregando los elementos de xs uno a uno 
-- Y cuando ya no queden elementos de xs, agregamos todo a ys. 
(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (\x rec -> x: rec) ys xs
```
```haskell
-- La idea es ir aplicandole "f" a cada uno de los elementos de xs
-- Cuando no queden elementos, le agregamos todos los "x modificados" a la []
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x rec -> f x : rec) []
```
```haskell
-- La idea es ir validando cada uno de los elementos de xs usando el predicado "p"
-- Si es True lo agrego, si no sigo con el siguiente elemento
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr(\x rec -> if p x then x:rec else rec) []
```
### II.

OJO: `foldr1` es un tipo especial de foldr, usa el  `último elemento` de la lista como `caso base`. No se puede aplicar sobre estructuras vacías.

```haskell
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x rec -> if p x rec then x else rec)

--Aplicación: 
minimo :: [a] -> a
minimo = mejorSegun (<)
```
### III.




