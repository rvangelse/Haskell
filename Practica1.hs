import GHC.Num.BigNat (bigNatAdd)

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
sacarUna y = recr (\x xs rec -> if x == y then xs else x: rec) []

--No podemos usar foldr porque, necesitamos una recursion primitiva que nos permita acceder a mas informacion de las subestructuras recursivas.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado y = recr (\x xs rec  ->if y <= x then y:x:xs else x:rec) [y]

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (Main.uncurry f)

armarPares :: [a] -> [b] -> [(a, b)] -- Zip
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c] -- Zipwith
mapDoble f = foldr (\x rec ys -> if null ys then [] else f x (head ys) : rec (tail ys)) (const [])

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia x = foldNat (\y rec -> rec * x) 1

data AB a = Nil | Bin (AB a) a (AB a) deriving(Show)

foldAB ::b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f x = case x of
            Nil -> z
            (Bin l v r) -> f (rec l) v (rec r)
    where rec = foldAB z f

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB z f x = case x of
            Nil -> z
            (Bin l v r) -> f l v r (rec l) (rec r)
    where rec = recAB z f

esNil :: AB a -> Bool
esNil x = case x of
    Nil -> True
    _ -> False

altura :: AB a -> Integer
altura = foldAB 0 (\rl v rr -> 1 + max rl rr)

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\rl v rr -> 1 + rl + rr)

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> (rl `g` v) `g` rr) (Bin l v r)
    where g x y = if f x y then x else y

esABB :: Ord a => AB a -> Bool
esABB = recAB True f
    where f l v r rl rr | esNil l && esNil r = True
                        | esNil r = rl && raíz l <= v
                        | esNil l = rr && v < raíz r
                        | otherwise = rl && rr && raíz l <= v && v < raíz r

raíz :: AB a -> a
raíz (Bin l v r) = v

data RoseTree a = Rose a [RoseTree a]

foldRose :: ( a -> [b] -> b ) -> RoseTree a -> b
foldRose cRose (Rose n hijos) = cRose n (map rec hijos)
    where rec = foldRose cRose

hojas :: RoseTree a -> [a]
hojas = foldRose (\n hijos -> if null hijos then [n] else concat hijos)

distancias :: RoseTree a -> [Int]
distancias = foldRose (\_ hijos -> if null hijos then [0] else map (+1) (concat hijos))

alturaRT :: RoseTree a -> Int
alturaRT = foldRose (\_ hijos -> 1 + maximum (0 : hijos))


