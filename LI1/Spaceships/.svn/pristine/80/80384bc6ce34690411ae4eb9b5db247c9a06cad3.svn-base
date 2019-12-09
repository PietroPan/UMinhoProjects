{-| O objetivo desta tarefa é comprimir um estado de jogo o melhor possivel

-Desafio : Comprimir um mapa usando o sistema binário.

 Um mapa pode ser composto por 3 pessas diferentes (Bloco Destrutivel,Bloco Indestrutivel e Vazia) podemos associar a cada uma destas peças valores binários (funcão tornaBinário).
 Tendo em conta que usando Haskell temos à disposição cerca de 1100000 de carateres podemos usar no máximo 20 bits para representar um carater, porque 2^21 (número de combinações binárias possiveis com 21 bits) já excede o número de carateres disponíveis.
 Como cada peça do mapa precisa de 2 bits para ser representada conseguimos ter uma média de compressão de 9.5 peças por caracter
 Um dos problemas que tivemos foi ter de usar um bit para marcar o fim de um caracter para conseguirmos fazer a descompressão corretamente por isso temos uma média de compressão de pessa de 9.5 por carater e não de 10.
 
-Desafio : Comprimir a lista de jogadores e a lista de disparos

 Para além de querermos ter uma boa compressão também queriamos que a nossa função fosse capaz de comprimir um grande número de jogadores com varias vidas e disparos.
 Para conseguirmos fazer isto criamos uma função que calcula o número de bits necessários para comprimir algo (se receber um lista de jogadores com jogadores que tem como o número máximo de vida 3 dá-nos que é necessário 2 bits para comprimir as vidas mas se fossem 4 já ia dar que eram necessários 3 bits)
 Deste modo conseguimos comprimir mapas de todos os tipos

Em conclusão esta tarefa foi feita de modo a desefiarmo-nos e deu-nos bastante informação sobre o sistema binário.
-}


module Tarefa3_2018li1g077 where
import Tarefa1_2018li1g077
import Tarefa2_2018li1g077
import Data.List
import LI11819
import Data.Char
import Tarefa0_2018li1g077
-- * Testes

-- | Testes unitários da Tarefa 3.-
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [(Estado (mapaInicial (6,6)) [(Jogador (1,1) C 1 0 4), (Jogador (3,3) D 1 2 2)] [(DisparoChoque 0 3)]), (Estado (mapaInicial (10,10)) [(Jogador (1,1) D 2 3 5)] [(DisparoCanhao 0 (3,3) C), (DisparoLaser 0 (3,1) D),(DisparoChoque 0 3)]), (Estado (mapaInicial (6,6)) [] [] )]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime (Estado (m:ms) j d) = [chr (length m)] ++ -- tamanho duma linha
      (tira15 (addPosMap (posParaIndice (map posicaoJogador j) (length m)) (mapaBin (init (map aux ms))))) ++   -- mapa
       ['\1100000'] ++ 
       [chr (bTSS2 j) ] ++ (tira15(concat (indToBin (bTSS j) (map tttr(ordena (listPosTripl (map posicaoJogador j) 0) ))))) ++ -- indices [3,1,0,2]
       ['\1050000'] ++ -- direcoes
       (tira15 (concat (map tornaBinarioD (map tiraDJ j)))) ++
       ['\1049999'] ++ -- vidas, lasers, choques
       [(chr (maxAttribute j))] ++
       (vlcParaBin (maxAttribute j) j) ++
       ['\1049998'] ++ (if ((length (disparosC d)) <= 0) then ['\1049997'] else ((vChoques)  ++ (disparoParaChar (disparosC d)))) ++   --disparoChoques
       ['\1049996']  ++ (if (length (disparosL d)) <= 0 then ['\1049995'] else [chr wow] ++ (tira15 (concat (indToBin wow ( concat(tiraNumDisparos d))))))
            where bTSS j  | length j > 0 = (checkBits (maximum (map tttr (listPosTripl (map posicaoJogador j) 0))) 1 1)
                          | otherwise = 1
                  
                  bTSS2 j | length j > 0 = (checkBits (maximum (map tttr (listPosTripl (map posicaoJogador j) 0))) 1 1)
                          | otherwise = 1100002
                  vChoques = [chr (checkBits (maximum (concat (map disPAux d))) 1 1)]
                  wow = (checkBits (maximum (concat(tiraNumDisparos d))) 1 1)

tiraNumDisparos :: [Disparo] -> [[Int]]
tiraNumDisparos [] = []
tiraNumDisparos ((DisparoCanhao a (b,c) d):xs) = ([0,a,b,c,(dPStringN2 d)]:(tiraNumDisparos xs))
tiraNumDisparos ((DisparoLaser a (b,c) d):xs) = ([1,a,b,c,(dPStringN2 d)]:(tiraNumDisparos xs))
tiraNumDisparos (_:xs) = tiraNumDisparos xs



-- wow = checkBits (maximum (concat (map tiraNumDisparos d))) 1 1
-- tira15 (concat (indToBin wow (concat (tiraNumDisparos d))))

dPStringN2 :: Direcao -> Int
dPStringN2 C = 4
dPStringN2 D = 3
dPStringN2 B = 2
dPStringN2 E = 1
                  

projectilParaString :: [Disparo] -> String
projectilParaString [] = []
projectilParaString ((DisparoLaser a (x,y) c):xs) = ("L" ++ [(chr a)] ++ (parParaString (x,y)) ++  (dPString c) ++ " " ++ (projectilParaString xs))
projectilParaString ((DisparoCanhao a (x,y) c):xs) = ("C" ++ [(chr a)] ++ (parParaString (x,y))  ++ (dPString c) ++ " " ++ (projectilParaString xs))
projectilParaString (_:xs) = projectilParaString xs

parParaString :: (Int,Int) -> String
parParaString (x,y) = [(chr x)] ++ "," ++ [(chr y)]

dPString :: Direcao -> String
dPString C = "4"
dPString D = "3"
dPString B = "2"
dPString E = "1"

tiraPBala :: Disparo ->  Posicao
tiraPBala (DisparoLaser a b c)  = b

tiraInBala :: Disparo -> Int
tiraInBala (DisparoLaser a b c) = a

tiraDirBala :: [Disparo] -> [Direcao]
tiraDirBala ((DisparoLaser a b c):xs) = (c : (tiraDirBala xs))
 

disparoParaChar :: [Disparo] -> String
disparoParaChar [] = "w"
disparoParaChar (d:ds) = (tira15 (concat (indToBin x (concat (map disPAux (d:ds))))))
  where x = (checkBits (maximum (concat (map disPAux (d:ds)))) 1 1)

disPAux :: Disparo -> [Int]
disPAux (DisparoChoque j t) = [j,t]
disPAux _ = []

disparosC :: [Disparo] -> [Disparo]
disparosC [] = []
disparosC ((DisparoChoque a b):xs) = (DisparoChoque a b) : (disparosC xs)
disparosC (_:xs) = disparosC xs

disparosL :: [Disparo] -> [Disparo]
disparosL [] = []
disparosL ((DisparoLaser a b c):xs) = (DisparoLaser a b c) : (disparosL xs)
disparosL ((DisparoCanhao a b c):xs) = (DisparoCanhao a b c) : (disparosL xs)
disparosL (_:xs) = disparosL xs


tiraVLC (Jogador a b c d e) = [c,d,e]

-- numero minimo de bits para ler os dados do jogador individual ( vida, laser e choque)
maxAttribute :: [Jogador] -> Int
maxAttribute [] = 0
maxAttribute j = (checkBits (maximum(concat (map tiraVLC j))) 1 1)

--transforma lista de choques, vidas e lasers de todos os jogadores em 1's e 0's, sabendo o numero de bits

vlcParaBin :: Int -> [Jogador] -> String
vlcParaBin n j =  tira15 (concat(indToBin n (concat (map tiraVLC j))))

tiraDJ :: Jogador -> Direcao
tiraDJ (Jogador a b c d e) = b
 
                        
aux :: [a] -> [a]
aux (x:xs) = init xs



--verifica a menor numero de bits possiveis para representar um numero
-- checkBits int 1 2
checkBits :: Int -> Int -> Int -> Int
checkBits int bts total = if (int <= total) then bts else (checkBits int (bts + 1) (total + 2^(bts)))



-- transforma uma lista de posicoes para um triplo (posicao, identificador do jogador)
listPosTripl :: [Posicao] -> Int -> [(Int, Int, Int)]
listPosTripl [] _ = []
listPosTripl ((x,y):xs) n = ((x,y,n) : (listPosTripl xs (n+1))) 

-- ordena uma lista de posicoes pela ordem que as posicoes aparecem
ordena :: [(Int,Int,Int)] -> [(Int,Int,Int)]
ordena [] = []
ordena (x:xs) = ordenaAux x (ordena xs)

ordenaAux :: (Int,Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)]
ordenaAux (x,y,z) [] = [(x,y,z)]
ordenaAux (x,y,z) ((a,b,c):xs) | x < a = ((x,y,z):(a,b,c):xs)
                               | x == a = if y < b then ((x,y,z):(a,b,c):xs) else ((a,b,c):(ordenaAux (x,y,z) xs))
                               | otherwise = ((a,b,c):(ordenaAux (x,y,z) xs))
-- transforma uma lista de inteiros numa string de bins de n-bytes
indToBin :: Int -> [Int] -> [String]
indToBin n l = (map (posicoesJogador n) l) 



tttr :: (Int,Int,Int) -> Int
tttr (x,y,z) = z

-- indice jogador para binario
posicoesJogador :: Int -> Int -> String
posicoesJogador bts iJ = (replicate (bts - (length (pegaLinha [] iJ))) '0')  ++ (pegaLinha [] iJ)


--


-- pega numa lista de indices e adiciona "11" nos sitios certos para marcar a posicao dos jogadroes

addPosMap :: [Int] -> String -> String
addPosMap [] m = m 
addPosMap _ [] = []
addPosMap liJ (b:bs) = if (elem (0) liJ) then ("11" ++ [b] ++ (addPosMap (map ((-1)+) liJ) (bs))) 
                                       else (b : (addPosMap (map ((-1)+) liJ) bs)) 

-- recebe uma posicao e o comprimento duma linha e devolve um indice a ser usado para por "11" no mapa de 1's e 0's 
posParaIndice :: [Posicao] -> Int -> [Int]
posParaIndice [] _ = []
posParaIndice ((x , y): xs) n = (((x-1) * (n-2) + y)*2) : (posParaIndice xs n) 



tira15 :: String -> String
tira15 [] = []
tira15 m = [(chr(contaPecas (take 19 m)))] ++ (tira15 (drop 19 m)) 




mapaBin :: [[Peca]] -> String
mapaBin m = concat (map (tornaBinario) (concat m))

tornaBinario :: Peca -> String
tornaBinario Vazia = "10"
tornaBinario (Bloco Destrutivel) = "01"
tornaBinario (Bloco Indestrutivel) = "00"

tornaBinarioD :: Direcao -> String
tornaBinarioD C = "10"
tornaBinarioD D = "01"
tornaBinarioD B = "00"
tornaBinarioD E = "11"

-- contaPecas : transforma uma lista de 0's e 1's num inteiro, adiciona 1 no inicio para não bugar

contaPecas :: String -> Int
contaPecas l = contaPecasAux ((length l)) ("1" ++ l)

contaPecasAux :: Int -> String -> Int
contaPecasAux 0 (x:xs) = ((binParaInt (x:xs)) * (2 ^ 0))
contaPecasAux n (x:xs) = ((binParaInt (x:xs)) * (2 ^ n)) + (contaPecasAux (n-1) xs)

binParaInt :: String -> Int
binParaInt (x:xs) = read [x] :: Int



------------------------------------------------------------------------------------------------------------------------


-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime (x:xs) = (Estado (charParaMap mp) (verificaVazio iJs)    ( (verificaChoques(tail dchQ)) ++ (verificaLC (tail lEc) ) ) )      
                       where mp = takeWhile (/= '\1100000') (x:xs)
                             iJs = (dropWhile ( /= '\1100000') (takeWhile (/= '\1050000') (x:xs)))
                             dJs = (dropWhile (/= '\1050000') (takeWhile (/= '\1049999') (x:xs)))
                             vlc = (dropWhile (/= '\1049999') (takeWhile (/= '\1049998') (x:xs))) 
                             dchQ = (dropWhile (/= '\1049998') (takeWhile (/= '\1049996') (x:xs)))
                             lEc = (dropWhile (/= '\1049996') (x:xs))
                             verificaVazio :: String -> [Jogador]
                             verificaVazio (x:xs) = (if (x == '\1100002') then [] else criaLJogador (map (doisP) (ordenaPosJog (posicaoJogadores (charParaInd (tail iJs)) (binParaPos mp)))) (charParaLD (prepStringD  (tail dJs))) (charParaInd  (tail  vlc)))
                             verificaChoques :: String -> [Disparo]
                             verificaChoques (x:xs) = (if (x == '\1049997') then [] else (charToChq (x:xs)))
                             verificaLC :: String -> [Disparo]
                             verificaLC (x:xs) = (if (x == '\1049995') then [] else (lNumToD (a)))
                               where a = (bit8Num (ord x) bs)  
                                     bs = (prepStringD (xs))

a (x:xs) = (bit8Num (ord b) bs)  
  where b:bs = (prepStringD (x:xs))
                                   
-- *** funções auxiliares da função "descomprime"

-- | Converte um número na sua posição correspondente
dPString2n 4 = C
dPString2n 3 = D
dPString2n 2 = B
dPString2n 1 = E
      



-- | Transforma uma lista de numeros numa lista de disparos
lNumToD :: [Int] -> [Disparo]
lNumToD [] = []
lNumToD (a) = ((lNumToDa (take 5 a)) : (lNumToD (drop 5 a)))
 
-- | função auxiliar da função "lNumToD". A partir de uma lista de 5 números, cria um único disparo.
--
-- > lNumToDa [0,1,2,3,4] = DisparoCanhao (1,2) 3 C
-- > lNumToDa [1,3,9,1,1] = DisparoLaser (3,9) 1 E

lNumToDa :: [Int] -> Disparo
lNumToDa [a,b,c,d,e] = if a == 0 then (DisparoCanhao b (c,d) (dPString2n e)) else (DisparoLaser b (c,d) (dPString2n e))

-- | Converte um char na sua direçao correspondente

dPString2 '4' = C
dPString2 '3' = D
dPString2 '2' = B
dPString2 '1' = E

-- | Partindo duma string, devolve uma lista de disparos do tipo DisparoChoque
charToChq :: String -> [Disparo]
charToChq [] = []
charToChq (x:xs) = charToChqA ((bit8Num (ord x) (concat (map tail (map (pegaLinha []) (map ord xs))))))

-- | Função auxiliar da função charToChq. Percorre a lista de numeros gerada na função charToChq, recolhendo 2 valores (Int, Tick) correspondentes a cada DisparoChoque 
charToChqA :: [Int] -> [Disparo]
charToChqA [] = []
charToChqA j = let [a,b] = (take 2 j) 
             in ((DisparoChoque a b):(charToChqA (drop 2 j)))


-- | Cria a lista de jogadores 
criaLJogador :: [Posicao] -- ^ Lista das posicoes organizadas por ordem crescente dos indices dos jogadores
              ->[Direcao] -- ^ Lista das direções de cada jogador, organizadas por ordem crescente dos indices 
                 -> [Int] -- ^ Lista de numeros que correspondem ás vidas, lasers e choques dos jogadores.
             -> [Jogador] 

criaLJogador (x:xs) (y:ys) l = let [a,b,c] = (take 3 l) in ((Jogador x y a b c) : (criaLJogador xs ys (drop 3 l)))
criaLJogador _ _ _ = [] 
 
 


-- | Prepara uma string de chars numa de 0's e 1's , pronta a ser lida
prepStringD :: String -> String
prepStringD [] = []
prepStringD l = concat (map tail ((map (pegaLinha []) (map ord l))))



-- | Associa lista de 1's e 0's a direcoes
charParaLD :: String -> [Direcao]
charParaLD l | take 2 l == "10" = ((C) : (charParaLD (drop 2 l)))
             | take 2 l == "01" = ((D) : (charParaLD (drop 2 l)))
             | take 2 l == "00" = ((B) : (charParaLD (drop 2 l)))
             | take 2 l == "11" = ((E) : (charParaLD (drop 2 l)))

-- | Funcao que associa uma lista de indices a uma lista de posicoes
-- 
-- > ordenaPosJog [2,0,1] [(1,3),(1,4),(2,2)] = [(1,3,2),(1,4,0),(2,2,1)]

posicaoJogadores :: [Int] -> [Posicao] -> [(Int,Int,Int)]
posicaoJogadores (x:xs) ((a,b):ys) = (a,b,x) : (posicaoJogadores xs ys)
posicaoJogadores _ _ = []

-- | Funcao que ordena uma lista de triplos pelo indice do jogador
--
-- > ordenaPosJog [(1,3,2),(1,4,0),(2,2,1)] = [(1,4,0),(2,2,1),(1,3,2)] 
ordenaPosJog :: [(Int,Int,Int)] -> [(Int,Int,Int)]
ordenaPosJog [] = []
ordenaPosJog (x:xs) = ordPJA x (ordenaPosJog xs)

-- | Função auxiliar da função ordenaPosJog, insere um tuple no sitio adequado da lista.

ordPJA :: (Int,Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)]
ordPJA x [] = [x]
ordPJA (x,y,z) ((a,b,c):xs) = if z < c then ((x,y,z):(a,b,c):xs) else ((a,b,c):(ordPJA (x,y,z) xs))

-- | Funcao que tira o ultimo elemento duma tripla.

doisP :: (Int,Int,Int) -> (Int,Int)
doisP (a,b,c) = (a,b)
 




{--  Transforma uma string num mapa
 
(criaListas (ord x) (Bloco Indestrutivel)) : (  adiciona a primeira linha de blocos indestrutiveis
map aux2 (                                      adiciona um bloco indestrutivel no inicio e no fim de cada linha
criaMapa x (                                    transforma a lista de pecas num mapa de acordo com o comprimento de uma linha
agrupaListas (                                  transforma a lista de 1's e 0's numa lista de pecas
removeJogadores (                               remove os 11's que marcam as posicoes dos jogadores
concat(                                         junta todas as strings (String)
map tail (                                      remove todos os primeiros elementos (por causa do 1 adicionado pela função tira15)
map (pegaLinha [])  (                           transforma os numeros em strings de 1's e 0's ([String])
map ord xs)))))))                               transforma os caracters em numeros ([Int])
 ++ [criaListas (ord x) (Bloco Indestrutivel)]  cria a ultima lista de blocos indestrutiveis --}


charParaMap :: String -> Mapa
charParaMap (x:xs) = (criaListas (ord x) (Bloco Indestrutivel)) : (map aux2 (criaMapa x (agrupaListas (removeJogadores( concat(map tail (map (pegaLinha [])  (map ord xs)))))))) ++ [criaListas (ord x) (Bloco Indestrutivel)]



-- | Sabendo o número de bits usados para representar os números (ord x), transforma uma string numa lista de números

charParaInd :: String -> [Int]
charParaInd (x:xs) = (bit8Num (ord x) (concat (map tail (map (pegaLinha []) (map ord xs)))))

--
binParaPos :: String -> [Posicao]
binParaPos (x:xs) = map (indParaPos ((ord x)-2) (1,1)) (map ( `div` 2) (criaPos 0 (concat (map tail (map (pegaLinha []) (map ord xs)))) []))

--binParaPos :: String -> [Posicao]
--binParaPos (x:xs) = map (indParaPos ((ord x)-2) (1,1)) (map ( `div` 2) (criaPos 0 (concat (map tail (map (pegaLinha []) (map ord xs)))) []))

-- transforma uma lista de listas de 1's e 0's  numa lista de numero correspondentes (sabendo o n de bits)
bit8Num :: Int -> String -> [Int]
bit8Num n [] = []
bit8Num n l = (contaPecasAux (n-1) (take n l)) : (bit8Num n (drop n l))

criaMapa :: Char -> [Peca] -> Mapa
criaMapa n [] = []
criaMapa n l = ((take ((ord n)-2) l) : (criaMapa n (drop ((ord n)-2) l)))




-- recebe um indice e o comprimento da linha do mapa (Char), e entrega uma posicao


indParaPos :: Int -> Posicao -> Int -> Posicao 
indParaPos ch (x,y) n = if n > (ch) then (indParaPos  (ch) (x+1, y) (n - ch)) else (x, n) 
 

-- calcula os indices do elementos 11 na lista de 1's e 0's
criaPos :: Int -> String -> [Int] -> [Int]
criaPos l [] ac = ac
criaPos l n ac = if (take 2 n) == "11" then (criaPos (l + 2) (drop 4 n) (ac ++ [l])) else (criaPos (l+2) (drop 2 n) ac) 

-- | remove os 11's da string de 1's e 0's
removeJogadores :: String -> String 
removeJogadores [] = []
removeJogadores l = if (take 2 l) == "11" then removeJogadores (drop 2 l) 
                                                      else (take 2 l) ++ (removeJogadores (drop 2 l))

aux2 :: [Peca] -> [Peca]
aux2 x = [Bloco Indestrutivel] ++ x ++ [Bloco Indestrutivel]

-- |  Agrupa uma string numa lista com strings de comprimento indicado
agrupaBin :: Int -> String -> [String]
agrupaBin n [] = []
agrupaBin n l = [take n l] ++ (agrupaBin n (drop n l))


agrupaListas :: String -> [Peca] 
agrupaListas [] = []
agrupaListas l = ((agrupaListasAux (take 2 l)) : (agrupaListas (drop 2 l)))

agrupaListasAux :: String -> Peca 
agrupaListasAux l        | l == "00" = (Bloco Indestrutivel)
                         | l == "01" = (Bloco Destrutivel)
                         | l == "10" = Vazia


-- | Transforma um numero numa string de 0's e 1's
pegaLinha :: String -> Int -> String
pegaLinha l 0 = ("0" ++ l)
pegaLinha l 1 = ("1" ++ l)
pegaLinha l n | mod n 2 == 0 = pegaLinha ("0" ++ l) (div n 2) 
              | mod n 2 == 1 = pegaLinha ("1" ++ l) (div n 2)

aux3 :: Int -> String
aux3 1 = show 1
aux3 x = aux3 (div x 2) ++ show (mod x 2)
