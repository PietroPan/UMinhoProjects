import Data.List
import Data.Either
--01
myEnumFromTo ::  Int -> Int -> [Int]
myEnumFromTo a b
    | a == b = [a]
    | a < b = a : myEnumFromTo (a+1) b
    | otherwise = []

--02
myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo a b c
    | a <= c = a : myEnumFromThenTo (a+b-1) b c
    | a > c = []

--03
conect :: [a] -> [a] -> [a]
conect [] c = c
conect (a:b) c = a : conect b c 

--04
indice :: [a] -> Int -> a
indice (h:t) n
    | n==0 = h
    | n/=0 = indice t (n-1)

--05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse t ++ [h]

--06
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake a (h:t) = h:myTake (a-1) t

--07
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 l = l
myDrop a (h:t) = myDrop (a-1) t

--08
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

--09
myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (h:t)
    | a==h = True
    | otherwise = myElem a t

--10
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = a:myReplicate (n-1) a

--11
myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [a] = [a]
myIntersperse a (h:t) = h:a:myIntersperse a t

--12
myGroup :: Eq a => [a] -> [[a]]
myGroup []    = [[]]
myGroup (h:t) = aux1 [h] t
        where
        aux1 :: Eq a => [a] -> [a] -> [[a]]
        aux1 a [] = [a]
        aux1 a (h:t) | elem h a = aux1 (h:a) t
                     | otherwise = a :aux1 [h] t
--13
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h++myConcat t

--14
myInits :: [a] -> [[a]]
myInits l = aux2 0 l
        where
        aux2 :: Int -> [a] -> [[a]]
        aux2 n l
            | n<length l =take n l:aux2 (n+1) l
            | otherwise = [l]

--15
myTails :: [a] -> [[a]]
myTails l = aux3 0 l
        where
        aux3 :: Int -> [a] -> [[a]]
        aux3 n l
            | n<length l = drop n l:aux3 (n+1) l
            | otherwise = [[]]

--16
myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf [] _ = True
myIsPrefixOf _ [] = False
myIsPrefixOf (x:xs) (y:ys)
        | x==y = True && myIsPrefixOf xs ys
        | otherwise = False

--17
myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf [] _ = True
myIsSuffixOf _ [] = False
myIsSuffixOf l1 l2
        | l1 == l2 = True
        | otherwise = myIsSuffixOf l1 (drop 1 l2)

--18
myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf _ [] = False
myIsSubsequenceOf (x:xs) (y:ys)
        | x==y = myIsSubsequenceOf xs ys
        | otherwise = myIsSubsequenceOf (x:xs) ys

--19
myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices a l = aux4 0 a l
        where
        aux4 :: Eq a => Int -> a -> [a] -> [Int]
        aux4 _ _ [] = []
        aux4 n a (h:t)
            | a==h = n:aux4 (n+1) a t
            | otherwise = aux4 (n+1) a t

--20
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub (h:t) = h:myNub (aux5 h t)
        where
        aux5 :: Eq a => a -> [a] -> [a]
        aux5 _ [] = []
        aux5 a (h:t)
            | a==h = aux5 a t
            | otherwise = h:(aux5 a t)

--21
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (h:t)
        | a==h = t
        | otherwise = h:myDelete a t

--22
deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll [] l = l
deleteAll (h:t) l = deleteAll t (myDelete h l)

--23
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] l = []
myUnion l [] = l
myUnion l (h:t)
        | myElem h l = myUnion l t
        | otherwise = h:myUnion l t

--24
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect l [] = []
myIntersect [] l = []
myIntersect (h:t) l
        | myElem h l = h:myIntersect t l
        | otherwise = myIntersect t l

--25
myInsert :: Ord a => a -> [a] -> [a]
myInsert a [] = [a]
myInsert a (h:t)
        | a < h = a:h:t
        | otherwise =h:myInsert a t

--26
myUnwords :: [String] -> String
myUnwords [] = []
myUnwords [l] = l
myUnwords (x:xs) = x++" "++myUnwords (xs)

--27
myUnlines :: [String] -> String
myUnlines [] = []
myUnlines (x:xs) = x++"\n"++myUnlines (xs)

--28
pMaior :: Ord a => [a] -> Int
pMaior l = aux6 (0,1) l
        where
        aux6 :: Ord a => (Int,Int) -> [a] -> Int
        aux6 (r,i) [x] = r
        aux6 (r,i) (x1:x2:xs)
                | x2 > x1 = aux6 (i,i+1) (x2:xs)
                | otherwise = aux6 (r,i+1) (x1:xs)

--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:xs) = repetido x xs || temRepetidos xs
        where
        repetido :: Eq a => a -> [a] -> Bool
        repetido a [] = False
        repetido a (x:xs)
                | a==x = True
                | otherwise = repetido a xs

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (x:xs)
        | (x>='0') && (x<='9') = x:algarismos xs
        | otherwise = algarismos xs

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x1:x2:xs) = x2:posImpares xs

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x1:x2:xs) = x1:posPares xs

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x1:x2:xs)
        | x1<=x2 = True && isSorted (x2:xs)
        | otherwise = False

--34
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

--35
menor :: String -> String -> Bool
menor l []  = False
menor [] l = True
menor (x:xs) (y:ys)
        | x<y = True
        | x>y = False
        | x==y = menor xs ys

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((b,n):xs)
        | a==b = True
        | otherwise = elemMSet a xs

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((_,n):xs) = n+lengthMSet xs

--38
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((x,n):xs) = (aux7 n x)++converteMSet xs
        where
        aux7 :: Int -> a -> [a]
        aux7 0 a = []
        aux7 n a = a:aux7 (n-1) a

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((x,n):xs) 
        | a==x = (x,(n+1)):xs
        | otherwise = (x,n):insereMSet a xs

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a ((x,n):xs)
        | a==x && n==1 = xs
        | a==x = ((x,(n-1)):xs)
        | otherwise = (x,n):removeMSet a xs

--41
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = aux8 1 x xs
        where
        aux8 :: Ord a => Int -> a -> [a] -> [(a,Int)]
        aux8 n y [] = [(y,n)]
        aux8 n y (x:xs)
                | y==x = aux8 (n+1) y xs
                | otherwise = (y,n):aux8 1 x xs

--42
myPartitionEithers :: [Either a b] -> ([a],[b])
myPartitionEithers [] = ([],[])
myPartitionEithers (Left x :t) = (x:l,r)
        where (l,r) = myPartitionEithers t
myPartitionEithers (Right x :t) = (l,x:r)
        where (l,r) = myPartitionEithers t

--43
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes [] = []
myCatMaybes (Nothing:xs) = myCatMaybes xs
myCatMaybes (Just x:xs) = x:myCatMaybes xs

data Movimento = Norte | Sul | Este | Oeste
                deriving Show

--44
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:l) = posicao (x,y+1) l
posicao (x,y) (Sul:l) = posicao (x,y-1) l
posicao (x,y) (Este:l) = posicao (x+1,y) l
posicao (x,y) (Oeste:l) = posicao (x-1,y) l 

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (x,y)
        | yi-y < 0 = Norte:caminho (xi,yi+1) (x,y)
        | yi-y > 0 = Sul:caminho (xi,yi-1) (x,y)
        | xi-x < 0 = Este:caminho (xi+1,yi) (x,y)
        | xi-x > 0 = Oeste:caminho (xi-1,yi) (x,y)
        | otherwise = []

--46
vertical :: [Movimento] -> Bool
vertical [] = False
vertical [Norte] = True
vertical [Sul] = True
vertical (Norte:l) = True && vertical l
vertical (Sul:l) = True && vertical l
vertical (Oeste:l) = False
vertical (Este:l) = False

data Posicao = Pos Int Int
                deriving Show

--47
maisCentral :: [Posicao] -> Posicao
maisCentral (x:xs) = aux9 x xs
        where
        aux9 :: Posicao -> [Posicao] -> Posicao
        aux9 x [] = x
        aux9 (Pos x1 y1) ((Pos x2 y2):l)
                | (abs x1+abs y1) < (abs x2 + abs y2) = aux9 (Pos x1 y1) l
                | otherwise = aux9 (Pos x2 y2) l 

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x1 y1) ((Pos x2 y2):l)
        | (((x1 == x2+1) || (x1 == x2-1)) && y1==y2) = (Pos x2 y2):vizinhos (Pos x1 y1) l
        | (((y1 == y2+1) || (y1 == y2-1)) && x1==x2) = (Pos x2 y2):vizinhos (Pos x1 y1) l
        | otherwise = vizinhos (Pos x1 y1) l

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):l)
        | y1 == y2 = True && mesmaOrdenada ((Pos x1 y1):l)
        | otherwise = False

--50
