-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g077 where

import LI11819
import Tarefa1_2018li1g077
import Tarefa2_2018li1g077
import Data.List
import LI11819
import Data.Char
import Tarefa0_2018li1g077

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 0},Jogador {posicaoJogador = (1,7), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 0}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,5), direcaoDisparo = D},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,5), direcaoDisparo = B},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (2,1), direcaoDisparo = C},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (3,1), direcaoDisparo = B}]}), (Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,1), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 0},Jogador {posicaoJogador = (1,7), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 0}], disparosEstado = [DisparoLaser {jogadorDisparo = 0, posicaoDisparo = (1,2), direcaoDisparo = D},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,5), direcaoDisparo = D},DisparoLaser {jogadorDisparo = 0, posicaoDisparo = (1,4), direcaoDisparo = E}]}), (Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,9), direcaoJogador = E, vidasJogador = 0, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (1,7), direcaoJogador = E, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,8), direcaoDisparo = E},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,6), direcaoDisparo = E},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,5), direcaoDisparo = E},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,4), direcaoDisparo = E},DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (1,3), direcaoDisparo = E},DisparoLaser {jogadorDisparo = 0, posicaoDisparo = (1,2), direcaoDisparo = D}]}), (Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (1,9), direcaoJogador = E, vidasJogador = 0, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (1,7), direcaoJogador = E, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (1,5), direcaoJogador = E, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (1,3), direcaoJogador = E, vidasJogador = 0, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 0, tempoDisparo = 5},DisparoChoque {jogadorDisparo = 1, tempoDisparo = 0},DisparoChoque {jogadorDisparo = 2, tempoDisparo = 5},DisparoChoque {jogadorDisparo = 3, tempoDisparo = 0},DisparoLaser {jogadorDisparo = 0, posicaoDisparo = (1,2), direcaoDisparo = D}]}),(Estado mapa6x6 [(Jogador (1,1) D 1 1 1)] [(DisparoCanhao 0 (1,2) D ),(DisparoCanhao 0 (1,2) E)] ),(Estado mapa6x6 [(Jogador (1,1) D 1 1 1)] [(DisparoCanhao 0 (1,2) D ),(DisparoCanhao 0 (1,3) E)] ),(Estado mapa6x6 [(Jogador (1,1) D 1 1 1)] [(DisparoCanhao 0 (1,1) D)] ), (Estado mapa6x6 [(Jogador (1,1) D 1 1 1)] [(DisparoLaser 0 (1,2) D ),(DisparoCanhao 0 (1,2) E)] ),(Estado mapa6x6 [(Jogador (1,1) D 1 1 1)] [(DisparoLaser 0 (1,2) D ),(DisparoCanhao 0 (1,3) E)] )]
 where mapa6x6 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] 


-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- |  

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado mp [] []) = (Estado mp [] [])
tickLasers (Estado mp jGs []) = (Estado mp jGs [])
tickLasers (Estado mp [] dsps) = Estado mP jS (dS ++ disChq) 
 where (Estado mP jS dS) = efeitosExpl (Estado mp [] dsps) (juntaXpl (checkTD disL disC) ([],[]) )   
       disC = disparosCa (dsps)
       disChq = disparosC (dsps)
       disL = disparosLser (dsps)
       checkTD :: [Disparo] -> [Disparo] -> [([PosicaoGrelha], [Posicao])]
       checkTD [] [] = []
       checkTD [] y = []
       checkTD (x:xs) y = (laser x mp [] y ([],[])) : (checkTD xs y)
tickLasers (Estado mp jGs (d:ds)) =  if length disL > 0 then Estado mP jS (disChq ++ (disparosCa dS)) else (Estado mp jGs (disC ++ disChq))   
 where (Estado mP jS dS) = efeitosExpl (Estado mp jGs (d:ds)) (juntaXpl (checkTD disL disC) ([],[]) )   
       disC = disparosCa (d:ds)
       disChq = disparosC (d:ds)
       disL = disparosLser (d:ds)
       checkTD :: [Disparo] -> [Disparo] -> [([PosicaoGrelha], [Posicao])]
       checkTD [] [] = []
       checkTD [] y = []
       checkTD (x:xs) y = (laser x mp jGs y ([],[])) : (checkTD xs y)








-- | Junta uma lista de Posicoes afetadas pela explosao
juntaXpl :: [([PosicaoGrelha], [Posicao])] -> ([PosicaoGrelha],[Posicao]) -> ([PosicaoGrelha], [Posicao])
juntaXpl [] (a,b) = (a,b)
juntaXpl ((a,b):xs) (c,d) = juntaXpl xs ((c ++ a),(d ++ b))

-- | Sabendo a posicaoGrelha que afeta um jogador, devolve a posicaoGrelha ocupada pelo jogador
explJ :: PosicaoGrelha -> Direcao -> [Jogador] -> ([PosicaoGrelha], [Posicao])
explJ p dir [] = ([],[])
explJ p dir (j:js) = if elem (posicaoJogador j) (checkJg p dir) then ([(posicaoJogador j)],[]) else explJ p dir js 


-- | Recebe uma posicãoGrelha e uma direção, devolve as posições do mapa e da grelha que deverão ser afetadas pela explosão.

expl :: Posicao -> Direcao -> ([PosicaoGrelha], [Posicao])
expl (x,y) C = ([(x,y)],[]) 
expl (x,y) D = ([(x,y)],[])
expl (x,y) B = ([(x,y)],[])
expl (x,y) E = ([(x,y)],[])

-- | Sabendo uma posicão da grelha e uma direção, devolve as posicoes associadas aos 2 blocos á frente dessa posição. 

expL :: PosicaoGrelha -> Direcao -> ([PosicaoGrelha], [Posicao])
expL (x,y) C = ([],[(x,y) ,(x, y+ 1)]) 
expL (x,y) D = ([],[(x,y+1) ,(x+1, y+ 1)])
expL (x,y) B = ([],[(x+1,y) ,(x+1, y+1)])
expL (x,y) E = ([],[(x,y) ,(x+1, y)])

-- | Sabendo as posições da grelha e do mapa afetados por uma explosão, muda o estado.

efeitosExpl :: Estado -> ([PosicaoGrelha], [Posicao]) -> Estado
efeitosExpl (Estado mp jg d) (pG,pM) = Estado (boom pM mp) (boomJg pG2 jg) ((disparosLser d) ++ (disparosC d) ++ (fst $ boomDs pG (disparosCa d)))  
 where pG2 = (snd $ boomDs pG (disparosCa d))

-- | Sabendo as posições do mapa a arrebentar, arrebenta com os blocos destrutíveis
boom :: [Posicao] -> Mapa -> Mapa
boom _ [] = []
boom [] mp = mp
boom (p:ps) mp = boom ps (if ((encontraPosicaoMatriz p mp) == (Bloco Destrutivel)) then (atualizaPosicaoMatriz p (Vazia) mp) else mp)

-- | Sabendo as posições da grelha afetadas por uma explosão, aplica os efeitos das explosões nos jogadores (acumulador)
boomJg :: [PosicaoGrelha] ->  [Jogador] -> [Jogador]
boomJg _  [] = [] 
boomJg [] jgs = jgs
boomJg  (pBs) ((Jogador pJ dir vJ lJ cJ):js) =  ((Jogador pJ dir (if elem pJ pBs && vJ > 0 then vJ -1 else vJ) lJ cJ):(boomJg pBs js)) 

-- | Sabendo as posições da grelha afetadas por uma explosão, aplica os efeitos das explosões nos disparosCanhao.

boomDs :: [PosicaoGrelha] -> [Disparo] -> ([Disparo],[PosicaoGrelha])
boomDs pGS [] = ([],pGS)
boomDs pGS (x:xs) = if elem (posicaoDisparo x) pGS then boomDs (delete (posicaoDisparo x) pGS) xs else (x:a,(posicaoDisparo x):b)
 where (a,b) = boomDs pGS (xs)

 --  Posições onde deve haver uma explosão causada por um laser
laser :: Disparo  -- ^ DisparoLaser a ser analisado
         -> Mapa  -- ^ Mapa do jogo.
         -> [Jogador]  -- ^ Lista de jogadores
         -> [Disparo]  -- ^ Lista de disparos
         -> ([PosicaoGrelha],[Posicao])  -- ^ Acumulador
         -> ([PosicaoGrelha],[Posicao])
 
laser (DisparoLaser ind (p1,p2) dir) mp [] lD (a,b) = (a,b)
laser (DisparoLaser ind (p1,p2) dir) mp (j:js) lD (a,b) = if not c then (a1 ++ a,b ++ b1) else (laser (DisparoLaser ind (p1 + e,p2 + f) dir) mp (j:js) lD  (k ++ a ++a1 ++ n,b ++ l ++ b1 ++ m))
 where (c,d) = pdContinuarL (checkB mp (p1,p2) dir) 
       (a1,b1) = if d then expL (p1,p2) dir else ([],[])
       (e,f) = direcaoParaVetor dir
       lDCa = disparosCa lD
       listPosGD = map posicaoDisparo lDCa
       listPosJ = map posicaoJogador (j:js)
       (g,h) = expl (p1,p2) dir
       i = (verificaDisparos (p1,p2) lD) 
       vJk = (verificaJogadores' (p1,p2) dir (filter (\(Jogador aa bb cc dd ee) -> cc /= 0) (j:js)))
       (k,l) = if i  then (g,h) else ([],[]) 
       (n,m) = if vJk then explJ (p1,p2) dir (j:js) else ([],[])
       verificaJogadores' _ _ [] = False
       verificaJogadores' (x,y) dir ((Jogador (a,b) dir2 vj ls ch):js) = elem (a,b) (init $ checkJg (x,y) dir)  || (verificaJogadores' (x,y) dir (js))

       
       

-- | verifica se existe algum disparo no caminho do projéctil, e se existir, remove-o da lista de disparos (laser)

verificaDisparos :: PosicaoGrelha -> [Disparo] -> Bool
verificaDisparos posC1 [] = False
verificaDisparos posC1 ((DisparoCanhao ind posC2 dir):ds) = if posC1 == posC2 then True
                                                            else (verificaDisparos posC1 ds)

-- | Verifica se um jogador é atingido pelo disparo, conhecendo a sua posicão e direcão.
verificaJogadores :: PosicaoGrelha -> Direcao -> [Jogador] -> Bool
verificaJogadores _ _ [] = False
verificaJogadores (x,y) dir ((Jogador (a,b) dir2 vj ls ch):js) = elem (a,b) (checkJg (x,y) dir)  || (verificaJogadores (x,y) dir (js))

-- | Transforma uma posicaoGrelha numa lista de posicões. Usada pela funcao verificaJogadores para verificar se um Disparo atingiu um jogador.
checkJg :: PosicaoGrelha -> Direcao -> [PosicaoGrelha]
checkJg (x,y) C = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y)]
checkJg (x,y) D = [(x-1,y+1),(x,y+1),(x+1,y+1),(x,y)]
checkJg (x,y) B = [(x+1,y+1),(x+1,y),(x+1,y-1),(x,y)]
checkJg (x,y) E = [(x-1,y-1),(x,y-1),(x+1,y-1),(x,y)]

-- | Verifica se existe algum elementa da primeira lista que pertence á segunda.
compLista :: Eq a => [a] -> [a] -> Bool
compLista l [] = False
compLista [] l = False
compLista (x:xs) l = ((elem x l) || (compLista xs l))

 
-- | Verifica se o DisparoLaser pode continuar no seu caminho, dependendo do que há á sua frente, e decidindo se há explosão ou não). O primeiro elemento do par decide se o projéctil poderá continuar, o segundo decide se deverá criar uma explosão.

pdContinuarL :: (Peca,Peca) -> (Bool,Bool)
pdContinuarL  (x,y) | x == Bloco Indestrutivel || y == Bloco Indestrutivel = (False,True)
                    | x  == Bloco Destrutivel || y == Bloco Destrutivel = (True,True)
                    | otherwise = (True,False)


-- | Verifica se o DisparoCanhao pode continuar no seu caminho, dependendo do que há á sua frente, e decidindo se há explosão ou não). O primeiro elemento do par decide se o projéctil poderá continuar, o segundo decide se deverá criar uma explosão.

pdContinuarC :: (Peca,Peca) -> (Bool,Bool)
pdContinuarC  (x,y) | x == Vazia && y == Vazia = (True,False)
                    | otherwise = (False,True)  
   
 

-- | Verifica os blocos que se encontram junto do disparo, dependendo a direção
checkB :: Mapa -> Posicao -> Direcao -> (Peca,Peca)
checkB m (x,y) C = ((encontraPosicaoMatriz (x,y) m) , (encontraPosicaoMatriz (x, y+ 1) m)) 
checkB m (x,y) D = ((encontraPosicaoMatriz (x,y+1) m) , (encontraPosicaoMatriz (x+1, y+ 1) m))
checkB m (x,y) B = ((encontraPosicaoMatriz (x+1,y) m) , (encontraPosicaoMatriz (x+1, y+1) m))
checkB m (x,y) E = ((encontraPosicaoMatriz (x,y) m) , (encontraPosicaoMatriz (x+1, y) m))

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
{-tickCanhoes (Estado mp (j:js) []) = (Estado mp (j:js) []) 
tickCanhoes (Estado mp [] (d:ds)) = if length disC <= 0 then Estado mp [] (d:ds) else Estado mP jS (disChq ++ disL ++ (concat $ map tiraD (checkTD (d:ds) (d:ds))))
 where (Estado mP jS dS) = efeitosExpl (Estado mp [] []) (juntaXpl (map tiraPGP (checkTD (d:ds) (d:ds)))  ([],[]) )          
       disC = disparosCa (d:ds)
       disChq = disparosC (d:ds)
       disL = disparosLser (d:ds)
       checkTD :: [Disparo] -> [Disparo] -> [([PosicaoGrelha], [Posicao],[Disparo])]
       checkTD [] y = []
       checkTD (x:xs) y = (canhao x mp [] y ([],[])) : (checkTD xs y)
       rmDisp a = (rmvD(rmPosD a)) -}
tickCanhoes (Estado mp jgD (dds)) = if length disC <= 0 then Estado mp jgD (dds) else Estado mP jS (disChq ++ disL ++ (concat $ map tiraD (checkTD (disC) (disC))))
 where (Estado mP jS dS) = efeitosExpl (Estado mp jgD dds) (juntaXpl (map tiraPGP (checkTD (disC) (disC)))  ([],[]) )          
       disC = disparosCa (dds)
       disChq = disparosC (dds)
       disL = disparosLser (dds)
       checkTD :: [Disparo] -> [Disparo] -> [([PosicaoGrelha], [Posicao],[Disparo])]
       checkTD [] y = []
       checkTD (x:xs) y = (canhao x mp (filter (\(Jogador p d v l c) -> v > 0) jgD) (delete x y) ([],[])) : (checkTD xs y)
       rmDisp a = (rmvD(rmPosD a))


       

tiraPGP :: ([PosicaoGrelha],[Posicao],[Disparo]) -> ([PosicaoGrelha],[Posicao])
tiraPGP (a,b,c) = (a,b)


tiraD :: ([PosicaoGrelha],[Posicao],[Disparo]) -> [Disparo]
tiraD (a,b,c) = c





-- | Remove os disparos que "colidiram" sem ocupar a mesma posição (com acumulador)
rmvD :: [Disparo] -> [Disparo]
rmvD [] = [] 
rmvD (x:xs) = let (a,b) = direcaoParaVetor (direcaoDisparo x)
                  (pD1,pD2) = posicaoDisparo x
                  (c,d) = ((pD1 + negate a, pD2 + negate b), (dirOposta $ direcaoDisparo x))
                  (e,f) = (posicaoDisparo x, direcaoDisparo x)
                  disp :: Disparo -> (PosicaoGrelha,Direcao)
                  disp (DisparoCanhao ind pos dir) = (pos,dir)
              in if elem (c,d) (map disp (x:xs)) 
                 then rmvD $ filter  ((compPGD (c,d) (e,f)).disp) (x:xs)
                 else x : (rmvD xs)


-- | Recebe um disparo, faz um par que contêm a sua posicão e direção.
disp (DisparoCanhao ind pos dir) = (pos,dir)


-- | Compara pares com uma posicaoGrelha e direçao
compPGD :: (PosicaoGrelha,Direcao) -> (PosicaoGrelha,Direcao) -> (PosicaoGrelha,Direcao) -> Bool
compPGD (a,b) (x,y) (c,d) = not $ ((a == c) && (b == d)) || (x == c && y == d) 

-- | Devolve a direção oposta á direção recebida.
dirOposta :: Direcao -> Direcao
dirOposta C = B
dirOposta B = C
dirOposta D = E
dirOposta E = D

-- | Remove os disparos com a mesma posição
rmPosD :: [Disparo] -> [Disparo]
rmPosD [] = []
rmPosD (x:xs) = if elem (posicaoDisparo x) (map posicaoDisparo xs) then rmPosD $ filter ((/= (posicaoDisparo x)).posicaoDisparo) (x:xs) else x : (rmPosD xs)

-- | Remove todas as instâncias de um certo elemento de uma lista
deLete :: Eq a => a -> [a] -> [a]
deLete _ [] = []
deLete a (x:xs) = if a == x then deLete a xs else x : (deLete a xs)

-- | Posições onde deve haver uma explosão causada por um disparoCanhao
canhao :: Disparo -- ^ DisparoLaser a ser analisado
         -> Mapa -- ^ Mapa do jogo.
         -> [Jogador] -- ^ Lista de jogadores
         -> [Disparo] -- ^ Lista de disparos
         -> ([PosicaoGrelha],[Posicao]) -- ^ Acumulador
         -> ([PosicaoGrelha],[Posicao],[Disparo])
 
--canhao (DisparoCanhao ind (p1,p2) dir) mp jdD lD (a,b) = ([],[],[(DisparoCanhao ind (p1,p2) dir)])
canhao (DisparoCanhao ind (p1,p2) dir) mp jgD lD (a,b) 
 = if not c || vJk then (a ++ a1 ++ k ++ g,b ++ b1 ++ l++h, []) else ([],[], (if (elem (p1,p2) (map posicaoDisparo lD)) || (elem ((p1 + negate e,p2 + negate f),(dirOposta dir)) (map disp lD)) then [] else [(DisparoCanhao ind (p1+e,p2+f) dir)])) 
 where (c,d) = pdContinuarC (checkB mp (p1,p2) dir) 
       (a1,b1) = if d then expL (p1,p2) dir else ([],[])
       (e,f) = direcaoParaVetor dir
       lDCa = delete (DisparoCanhao ind (p1,p2) dir) lD
       listPosGD = map posicaoDisparo lDCa
       listPosJ = map posicaoJogador (filter (\(Jogador pp dd vv ll cc) -> vv > 0) jgD)
       (g,h) = explJ (p1,p2) dir (filter (\(Jogador p d v l c) -> v > 0) jgD)
       vJk = (verificaJogadores (p1,p2) dir (filter (\(Jogador pp dd vv ll cc) -> vv > 0) jgD) )
       (k,l) = if vJk then (g,h) else ([],[])
       disp :: Disparo -> (PosicaoGrelha,Direcao)
       disp (DisparoCanhao ind1 pos dir1) = (pos,dir1)



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado mp js dS) = (Estado mp js ((disparosL dS) ++ (efeitoChoques (disparosC dS)))) 


-- | A remove um tick a cada DisparoChoque, remove o DisparoChoque se tiver 0 Ticks restantes.
efeitoChoques :: [Disparo] -> [Disparo]
efeitoChoques [] = []
efeitoChoques ((DisparoChoque a 0):xs) = efeitoChoques xs
efeitoChoques ((DisparoChoque a b):xs) = ((DisparoChoque a (b-1)):(efeitoChoques xs)) 


-- | Seleciona os "DisparoChoque" duma lista de disparos.
disparosC :: [Disparo] -> [Disparo]
disparosC [] = []
disparosC ((DisparoChoque a b):xs) = (DisparoChoque a b) : (disparosC xs)
disparosC (_:xs) = disparosC xs

-- | Seleciona os "DisparoCanhao" e "DisparoLaser" duma lista de disparos. 
disparosL :: [Disparo] -> [Disparo]
disparosL [] = []
disparosL ((DisparoLaser a b c):xs) = (DisparoLaser a b c) : (disparosL xs)
disparosL ((DisparoCanhao a b c):xs) = (DisparoCanhao a b c) : (disparosL xs)
disparosL (_:xs) = disparosL xs

-- | Seleciona os "DisparoLaser" duma lista de disparos.
disparosLser [] = []
disparosLser ((DisparoLaser a b c):xs) = (DisparoLaser a b c) : (disparosLser xs)
disparosLser (_:xs) = disparosLser xs


-- | Seleciona os "DisparoCanhao" duma lista de disparos.
disparosCa [] = []
disparosCa ((DisparoCanhao a b c):xs) = (DisparoCanhao a b c) : (disparosCa xs)
disparosCa (_:xs) = disparosCa xs



