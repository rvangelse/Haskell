import Text.XHtml (base)
max2 :: Float -> Float -> Float
max2 x y | x >= y = x
         | otherwise = y

normaVectorial :: Float -> Float -> Float
normaVectorial x y = sqrt (x^2 + y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)

predecesor :: Float -> Float 
predecesor  = Main.subtract 1 

evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]
flipAll = map flip 

-- La función flipRaro no tiene un tipo válido porque 
-- estás intentando aplicar una función que invierte argumentos a otra función 
-- que también invierte argumentos, creando un ciclo de tipos que no encajan entre sí.

curry ::((a,b) -> c) -> (a-> (b -> c))
curry f a b = f (a, b)

uncurry :: (a -> (b -> c)) -> ((a, b) -> c)
uncurry f (a, b) = f a b


