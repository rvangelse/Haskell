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

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (\x rec-> x:rec) ys xs

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f  = foldr (\x rec-> f x:rec) [] 

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr p = foldr (\x rec -> if p x then x:rec else rec) []

