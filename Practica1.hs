
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

flipRaro :: b -> (a-> b -> c) -> a -> c
flipRaro = flip flip

curry ::((a,b) -> c) -> (a-> (b -> c))
curry f a b = f (a, b)

uncurry :: (a -> (b -> c)) -> ((a, b) -> c)
uncurry f (a, b) = f a b

-- CurryN no se puede definir, puedes definir Curry2, Curry3, Curry4.
-- Pero, no CurryN, ya que Haskell necesita saber exactamente el numero de argumentos
-- al tipar, debe ser ser estatico, no variable.

sum :: Num a => [a] -> a
sum = foldr (+) 0

elem :: Eq a => a -> [a] -> Bool
elem x = foldr (\y rec -> (y==x) || rec) False

masmas:: [a] -> [a] -> [a]
masmas xs ys = foldr (\x rec-> x:rec) ys xs

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f  = foldr (\x rec-> f x:rec) [] 

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr p = foldr (\x rec -> if p x then x:rec else rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\x rec -> if p x rec then x else rec)

sumasParciales :: Num a => [a] -> [a]
sumasParciales  = foldl (\rec x -> (if null rec then x :rec else rec ++ [last rec + x])) [] 

sumaAlt :: Num a => [a] -> a 
sumaAlt =  foldr (\x rec -> x - rec) 0

sumaAltInversa :: Num a => [a] -> a
sumaAltInversa = foldl (\acc x -> x - acc) 0

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y = recr (\x xs rec -> if x == y then xs else y: rec) []
--No podemos usar foldr porque, necesitamos una recursion primitiva que nos permita acceder a mas informacion de las subestructuras recursivas.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado y = recr (\x xs rec  ->if y <= x then y:x:xs else x:rec) [y]