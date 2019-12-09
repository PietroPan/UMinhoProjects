import Data.List

--4
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((v,g):xs) 
    | n == g = 1 + conta n xs
    | otherwise = conta n xs

--b)
grau :: Polinomio -> Int
grau [x] = snd x
grau (x1:x2:xs)
    | snd x1 > snd x2 = grau (x1:xs)
    | otherwise = grau (x2:xs)

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n (x:xs)
    | n == snd x = x:selgrau n xs
    | otherwise = selgrau n xs

--d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv (x:xs)
    | snd x == 0 = []
    | otherwise = (fst x *fromIntegral (snd x),(snd x)-1):deriv xs

--e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n (x:xs) = (n^snd x)*fst x + calcula n xs

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp (x:xs)
    | fst x == 0 = simp xs
    | otherwise = x:simp xs

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult y (x:xs) = (fst x*fst y,snd x+snd y):mult y xs

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (x:xs) = junta x (normaliza xs)
    where
    junta :: Monomio -> Polinomio -> Polinomio
    junta y [] = [y]
    junta y (x:xs)
        | snd y==snd x = junta (fst y+fst x,snd y) xs
        | otherwise = x:junta y xs

--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma [] x = x
soma x [] = x
soma (x:xs) y = x:soma xs y