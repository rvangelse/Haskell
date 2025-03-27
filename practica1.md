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
