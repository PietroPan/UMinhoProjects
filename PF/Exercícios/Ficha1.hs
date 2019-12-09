import Data.List

--1
--a)
perimetro :: Float -> Float
perimetro r = 2*pi*r

--b)
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (((x2-x1)^2)+((y2-y1)^2))

--c)
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

--d)
multiplo :: Int -> Int -> Bool
multiplo _ 0 = False
multiplo a b
    | mod a b == 0 = True
    | otherwise = False

--e)
truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar (x:xs)
    | mod (length (x:xs)) 2 == 0 = (x:xs)
    | otherwise = xs

--f)
max2 :: Int -> Int -> Int
max2 a b
    | a > b = a
    | otherwise = b

--g)
max3 :: Int -> Int -> Int -> Int
max3 a b c
    | (max2 a b) > c = (max2 a b)
    | otherwise = c

--2
--a)
nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c
    | ((b^2)-4*a*c) == 0 = 1
    | ((b^2)-4*a*c) < 0 = 0
    | otherwise = 2

--b)
raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | (nRaizes a b c) == 0 = []
    | (nRaizes a b c) == 1 = [(-b/2*a)]
    | otherwise = [((-b+sqrt(b^2-4*a*c))/(2*a)),((-b-sqrt(b^2-4*a*c))/(2*a))]

type Hora = (Int,Int)

--3
--a)
validh :: (Int,Int) -> Bool
validh (h,m) = h>=0 && h<24 && m>=0 && m<60

--b)
later :: Hora -> Hora -> Bool
later (h1,m1) (h2,m2)
    | h1>h2 = True
    | h1==h2 && m1>m2 = True
    | otherwise = False

--c)
tomin :: Hora -> Int
tomin (0,m) = m
tomin (h,m) = 60+tomin(h-1,m)

--d)
tohour :: Int -> Hora
tohour min = (div min 60,mod min 60)

--e)
difh :: Hora -> Hora -> Int
difh a b = abs (tomin a - tomin b)

--f)
addmin :: Int -> Hora -> Hora
addmin m h = tohour (m + tomin h)

--5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop Verde = False
stop Amarelo = False