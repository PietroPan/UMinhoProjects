{-| Este módulo define funções comuns da Tarefa 6 do trabalho prático.

O objetivo desta tarefa é programar um robot que consiga jogar bem o jogo desenvolvido no ãmbito da UC de LI1. Sendo que se deseja que o bot vença os torneios organizados contra outros robots, este, de modo a terminar o jogo com ao maior número de pontos, deve ser capaz de: 
    a) encontrar um caminho para qualquer posição do mapa;

    b) avaliar a sua situação relativamente à posição dos inimigos no mapa e efetuar uma jogada;

    c) desviar-se de disparos 

-Desafio : Encontrar um caminho para qualquer posição válida do mapa.

Para encontrar um caminho até uma posição desejada, é chamada a função pathfinder.

  O primeiro passo desta função é indicar todas as posições da grelha que existem num mapa com exceção das posições que se encontram à beira da borda (função posGM). 

  O segundo passo  é atribuír o valor (Just 0)  à posição destino e o valor Nothing a todas as outras posições (função jogadasNecessarias1). Forma-se então uma lista do tipo [(PosicaoGrelha,Maybe Int)]

  Depois, é aplicada a função assignJna3. Ás 4 posições à volta da posição marcada com (Just 0) é associado o valor (Just 1). As 4 posições à volta das posições marcadas com o valor (Just 1) são marcadas com o valor (Just 2) .

  Este processo repete-se até não haverem mais posicões marcadas com o valor Nothing, ou até não haverem mais posições a que seja possível aplicar um valor. Neste caso,  não é possível aceder a essas posições e permanecem com o valor Nothing. Uma posição não é associada a um valor (Just n) se já estiver associada a um valor (Just m).

  A função pathfinder vê as 4 posições à volta da posição inicial, e escolhe apenas as que tem um valor (Just n) associado menor ao valor associado à posição inicial.  

- Desafio : Avaliar a sua situação relativamente à posição dos outros jogadores de modo a decidir a jogada mais apropriada
  De início, é associado a algumas posições da grelha do mapa uma  “perigo” relativo a cada jogador (função f1). 

 -           Se um jogador precisa apenas de uma ou menos jogadas para ficar “em linha” com uma posição, essa posição fica associada ao par (indíce do jogador, 7).

  -          Se um jogador  conseguir ficar “em linha” com uma posição ao movimentar-se na só horizontal, essa posição fica associada ao par (indíce do jogador, 2).

   -         Se um jogador  conseguir ficar “em linha” com uma posição ao movimentar-se na só vertical, essa posição fica associada ao par (indíce do jogador, 3).

    -        Se um jogador conseguir ficar “em linha” com uma posição ao movimentar-se na vertical ou na horizontal, essa posição fica associada ao par (índice do jogador, 5)

     -      Se um jogador não conseguir ficar “em linha” com uma posição a mover-se exclusivamente na vertical ou na horizontal, a essa posição não é associado qualquer par.

  Na função bot, são analisadas as 6 posições da grelha que um jogador ocupa. O bot decide sempre atacar o inimigo com o indíce associado ao maior “nível de perigo”. Se não houverem quaisquer posições ocupadas pelo jogador com “nível de perigo” 7 ou 5, o bot decide mover-se para o jogador mais próximo.

  A função pontos é usada para verificar se, dada a oportunidade de disparar um disparoCanhao, vale a pena disparar um disparoLaser. A função pontos devolve o número de Blocos Destrutiveis e de disparosCanhao que seriam destruídos caso o jogador destruisse um laser. Se este número for maior que 6, é disparado um laser.

 -  Desafio : Desviar-se de balas.

            O primeiro passo é analisar os disparosCanhao que existem no estado, e ver todas as posições que irão ocupar, até atingirem uma parede ou o jogador (Função perigoDisparos). Se um disparoCanhao estiver no trajetória de colisão com o jogador, á posição ocupada pelo jogador que há de ser atingida é associada uma lista de pares do tipo Maybe [(Int,Direcao)]. O primeiro número representa o número de ticks até ao impacto, e a direção é a direção do disparo. Uma posição pode ter vários pares associados.  O mesmo processo é feito para as 3 posições novas que o jogador ocuparia caso se movesse na direção em que está olhar (Função perigoDisparosFuturo). 

            O segundo passo é associar cada posição a uma direção cardinal . Este passo também é realizado nas funções perigoDisparos e perigoDisparosFuturo.

<< https://i.imgur.com/hM2QxNx.png Exemplo das direções cardinais associadas ás posições do tanque.>>

            De seguida, é calculada a lista de jogadas que o jogador pode fazer de modo a não ser atingido por disparos em qualquer uma das suas posições (função accessDanger).
Para isto, são primeiro analisados os pares (String,Maybe (Int,Int)) individualmente.
Dependendo da direção em que o jogador está a olhar, o número de ticks até a colisão e a direção cardinal a ser analisada, é criada uma lista de todas as jogadas que o jogador pode fazer de modo a não ser atingido pelo projétil.

 Por exemplo,

(“N”,Just (3,B)) 

 -Se a direção do jogador for C, a lista devolvida é: (accessDangerNSEWa  C  (“N”,Just (3,B))) 

[Just (Dispara Canhao),Just (Dispara Laser),Just (Dispara Choque),Nothing,Just (Movimenta D),Just (Movimenta E)]

Neste caso, o jogador é impedido de se movimentar para cima e para baixo.
Quando é impossível evitar um disparo, a lista devolvida corresponde à lista de todas as jogadas possíveis.

Depois é feita a “interseção” (função jogadasComuns) de todas a listas de jogadas possíveis.
Dependendo das condições em que o bot se encontra, uma jogada ideal é escolhida. Se esta jogada ideal pertencer à lista de jogadas possíveis, o bot escolhe essa jogada. Caso contrário, é selecionado o primeiro elemento da lista de jogadas possíveis. 

Em conclusão, o bot consegue encontrar um caminho para qualquer posição no mapa. Tem problemas a jogar quando têm um inimigo a disparar canhões à sua frente, pois os seus tiros ficam eternamente a ser anulados pelos tiros do adversário. Para resolver este problema, possivelmente poderia ser alterada a maneira de atacar inimigos em frente, ou até evitar estes confrontos diretos e só atacar um bot inimigo quando este está em desvantagem. 
       -}


module Bot where

import Data
import MapEditor
import Plays
import Data.List
import Data.Char
import Functions
import TimeChange
 


-- numero associada a cada posicao de um tanque:

---------------------
--------------------- 8---7---6
----------------------
----------------------5---9---4
----------------------
--------------------- 3---2---1

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e | pontos (mapaEstado e) ((filter (\(DisparoCanhao a b c) -> a /= n) ) $ disparosCa $ disparosEstado e) pJ dJ > 6 && lJ > 0 = decisaoFinal [Just (Dispara Laser)] possiveisJogadas'
        | temDxplodir = decisaoFinal (if (pontos (mapaEstado e) ((filter (\(DisparoCanhao a b c) -> a /= n) ) $ disparosCa $ disparosEstado e) pJ dJ > 6) && lJ > 0 then [Just (Dispara Laser)] else [(Just (Dispara Canhao))]) possiveisJogadas
        | (length $ filter id (eAlvo inimigos minhaLFogo)) == 1 = decisaoFinal [(Just (Dispara Canhao))] possiveisJogadas'
        | (length $ filter id (eAlvo inimigos minhaLFogo)) >= 2 = decisaoFinal [(if lJ > 0 then Just (Dispara Laser) else Just (Dispara Canhao))] possiveisJogadas'
        | minhaSegurancaR == 7 = decisaoFinal ((map (\x -> Just (Movimenta x))) $ map (encontraDir pJ) (pathfinder pJ1 pJ (mapaEstado e))) possiveisJogadas'
        | minhaSegurancaR == 5 = decisaoFinal ((map (\x -> Just (Movimenta x))) $ map (encontraDir pJ) (pathfinder pJ2 pJ (mapaEstado e))) possiveisJogadas'
        | otherwise = if temDxplodir then decisaoFinal [(Just (Dispara Canhao))] possiveisJogadas' else  decisaoFinal  ((map (\x -> Just (Movimenta x))) $ map (encontraDir pJ) (pathfinder  (inimigoMaisProximo) pJ (mapaEstado e))) possiveisJogadas'
 where (Jogador pJ dJ vJ lJ cJ) = encontraIndiceLista n (jogadoresEstado e)
       gridSeguranca = f1 n e -- :: [(PosicaoGrelha,[(Int,Int)]] Posicoes do mapa e seguranca 
       minhasPosD = (perigoDisparos (mapaEstado e) (criaPos2X 1 pJ) (analisaDisparos (disparosCa $ disparosEstado e))) ++ (perigoDisparosFuturo (mapaEstado e) (novas3 pJ dJ) dJ (analisaDisparos (disparosCa $ disparosEstado e)))
       possiveisJogadas = accessDanger minhasPosD dJ
       minhaSeguranca = concat $ map (\x -> x gridSeguranca) (map encontraSegurancaAux (criaPos2X 1 pJ))
       minhaSegurancaR = maximum' $ map (\(w,z) -> z) minhaSeguranca
       listaSegurancaR7 = filter (\(k,l) -> l == 7) minhaSeguranca
       indxInimigo7 = (fst (head listaSegurancaR7))
       (Jogador pJ1 dJ1 vJ1 lJ1 cJ1) = encontraIndiceLista indxInimigo7 (jogadoresEstado e)
       listaSegurancaR5 = filter (\(k,l) -> l == 5) minhaSeguranca
       indxInimigo5 = (fst (head listaSegurancaR5))
       (Jogador pJ2 dJ2 vJ2 lJ2 cJ2) = encontraIndiceLista indxInimigo5 (jogadoresEstado e)
       inimigos = map fst listaJogVivos
       minhaLFogo = minhasTau n e -- [PosicaoGrelha]
       inimigoMaisProximo = if length inimigos == 0 then (pJ) else head $ ordBy (dist pJ) (map posicaoJogador inimigos) 
       listaJogVivos = assignInd (jogadoresEstado e) 0 n -- :: [(Jogador,Int)]
       temDxplodir = testeMovimenta2 (peca1,peca2)
       (peca1,peca2) = checkB (mapaEstado e) (somaVetores pJ (direcaoParaVetor dJ)) dJ
       (peca1C,peca2C) = checkB (mapaEstado e) (somaVetores pJ (direcaoParaVetor C)) C
       (peca1B,peca2B) = checkB (mapaEstado e) (somaVetores pJ (direcaoParaVetor B)) B
       (peca1D,peca2D) = checkB (mapaEstado e) (somaVetores pJ (direcaoParaVetor D)) D
       (peca1E,peca2E) = checkB (mapaEstado e) (somaVetores pJ (direcaoParaVetor E)) E
       testeMovimenta (pecaT1,pecaT2) = pecaT1 /= Bloco Indestrutivel && pecaT2 /= Bloco Indestrutivel 
       testeMovimenta2 (pecaT1,pecaT2) = pecaT1 == Bloco Destrutivel || pecaT2 == Bloco Destrutivel 
       possiveisJogadas' = deleteImpMoves [(C,testeMovimenta (peca1C,peca2C)),(B,testeMovimenta (peca1B,peca2B)),(D,testeMovimenta (peca1D,peca2D)),(E,testeMovimenta (peca1E,peca2E))] possiveisJogadas

-- | Aplica a função maximum a uma lista. Se essa lista for a lista vazia, devolve o valor "0".
maximum' [] = 0
maximum' l = maximum l

-- | Recebe um par com uma Direcao e um Bool (decide se é possível mover-se nessa direção), e apaga jogadas que involvem mover-se em direções associadas com o valor "False".
deleteImpMoves :: [(Direcao,Bool)] -> [Maybe Jogada] -> [Maybe Jogada] 
deleteImpMoves (d:ds) [] = []
deleteImpMoves [] d = d
deleteImpMoves (x:xs) d = if snd x then deleteImpMoves xs d else  deleteImpMoves xs (delete (Just (Movimenta (fst x))) d) 

-- | Recebe uma listas de jogadas que o jogador quer fazer, uma lista de jogadas que o jogador pode fazer, e devolve a jogada que deverá fazer.
decisaoFinal (x:xs) [] = x
decisaoFinal [] p = head p
decisaoFinal (q:qs) (p:ps) = if elem q (p:ps) then q else decisaoFinal qs (p:ps)


-- | Sabendo a posicao em que está e a posicao que quer ir, devolve a direcao necessaria para mover
encontraDir a b = case c of (1,0) -> B
                            ((-1),0) -> C
                            (0,1) -> D
                            (0,(-1)) -> E
 where c = subtraiVetores b a 
        
-- | Verifica se vale a pena disparar um laser.
pontos :: Mapa -> [Disparo] -> PosicaoGrelha -> Direcao -> Int
pontos mp d p1 dir = if pdContinuar 
                     then blocoParaPontos blc1 + blocoParaPontos blc2 + (if elem p1 (map posicaoDisparo d) then 1 else 0) + (pontos mp d p2 dir) 
                     else 0
 where (pdContinuar,zz) = pdContinuarL (checkB mp p1 dir)
       vetorD = direcaoParaVetor dir 
       blocoParaPontos (Bloco Destrutivel) = 1
       blocoParaPontos _ = 0
       (blc1,blc2) = (checkB mp p1 dir)
       p2 = somaVetores p1 vetorD



    
-- | Direcao que se deve ir de modo a puder disparar num inimigo.   
botAux1 :: PosicaoGrelha -> PosicaoGrelha -> Direcao
botAux1 (a,b) (c,d) | abs y < abs x = if x > 0 then B else C
                    | abs y > abs x = if y > 0 then D else E
                    | abs y == abs x = D
                    | otherwise = error "???"
 where x = c-a
       y = d-b
              


-- | Sabendo uma lista de jogadores e uma lista de posiçoes, verifica se as posições correspondem a uma das 6 posicoes ocupadas por um jogador.
eAlvo :: [Jogador] -> [PosicaoGrelha] -> [Bool]
eAlvo (j:js) p = (any id $ (map (\x -> x p)) $ map (\x -> elem x) (criaPos2X 1 (posicaoJogador j))):(eAlvo js p)
eAlvo [] _ = []

-- | Ordena uma lista pelos valores resultantes da aplicação duma função. 
ordBy :: Ord b => (a -> b) -> [a] -> [a]
ordBy f [] = []
ordBy f (x:xs) = ordByX f x (ordBy f xs)

-- | Função auxiliar da função "ordBy"
ordByX :: Ord b => (a -> b) -> a -> [a] -> [a]
ordByX f a [] = [a]
ordByX f a (x:xs) = if (f a) < (f x) then (a:x:xs) else x : (ordByX f a xs)      

-- | Sabendo uma posicao, devolve a lista de pares que representam a segurança dessa posição. 
encontraSegurancaAux :: PosicaoGrelha -> [(PosicaoGrelha,[(Int,Int)])] -> [(Int,Int)]
encontraSegurancaAux p [] = []
encontraSegurancaAux p (x:xs) = if p == fst x then (snd x) else encontraSegurancaAux p xs




--- 1 - Recolher informação


-- [Just (Move C), Just (Move D), Just (Move E), Just (Move B) , (Dispara Canhao), (Dispara Laser), Nothing]

-- | Calcula a distancia entre 2 vetores
dist x y = abs a + abs b
 where (a,b) = subtraiVetores x y

-- | Remove o n-ésimo elemento de uma lista
rmIndx n [] = []
rmIndx 0 (x:xs) = xs
rmIndx n (x:xs) = x : (rmIndx (n-1) xs)
---------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------disparos
  
-- | Calcula as possíveis jogadas que um jogador pode fazer de modo a não ser atingido por disparos.
accessDanger :: [(String,Maybe [(Int,Direcao)])] -> Direcao -> [Maybe Jogada]
accessDanger (("SE",a):("S",b):("SW",c):("E",d):("null",e):("W",f):("NE",g):("N",h):("NW",i):xs) dir 
 = jogadasComuns $ hollowSlayer [(accessDangerDiag [("SE",a),("SW",c),("NE",g),("NW",i)] dir),(accessDangerNSEW [("S",b),("W",f),("E",d),("N",h)] dir),(accessDangerFuture xs)] 

-- | Verifica que jogadas é que se podem fazer de modo a não ser atingido por Disparos não vão atingir o jogador se ele não se mover.
accessDangerFuture :: [(String,Maybe [(Int,Direcao)])] -> [Maybe Jogada]
accessDangerFuture [(a,x),(b,y),(c,w)]  = jogadasComuns $ hollowSlayer [one,two,three]
 where f (n,Nothing) = [(n,Nothing)] 
       f (n,Just []) = []
       f (n,Just (x:xs)) = (n,Just x) : (f (n,Just xs)) 
       one = jogadasComuns $ hollowSlayer $ map (possiveisJogadas3Diag) (f (a,x))
       two = jogadasComuns $ hollowSlayer $ map (possiveisJogadas3NSEW) (f (b,y))
       three = jogadasComuns $ hollowSlayer $ map (possiveisJogadas3Diag) (f (c,w))
      


-- | Verifica as jogadas que se podem fazer, de modo a não ser atingido nas posições "Diagonais" de um tanque. Se um tanque estivesse na posicão (1,1), estas posicões seriam as posicões : (0,0),(2,0),(2,0),(2,2)
accessDangerDiag :: [(String,Maybe [(Int,Direcao)])] -> Direcao -> [Maybe Jogada]
accessDangerDiag [("SE",x),("SW",y),("NE",w),("NW",z)] dir = jogadasComuns $ hollowSlayer [one,two,three,four] 
 where f (n,Nothing) = [(n,Nothing)] 
       f (n,Just []) = []
       f (n,Just (x:xs)) = (n,Just x) : (f (n,Just xs)) 
       length' (a,Nothing) = 0
       length' (b, Just c) = length y
       one = jogadasComuns $ hollowSlayer $ map (accessDangerDiagA dir) (f ("SE",x))
       two = jogadasComuns $ hollowSlayer $ map (accessDangerDiagA dir) (f ("SW",y))
       three = jogadasComuns $ hollowSlayer $ map (accessDangerDiagA dir) (f ("NE",w))
       four = jogadasComuns $ hollowSlayer $ map (accessDangerDiagA dir) (f ("NW",z))



-- | Sabendo uma direção cardinal diagonal (NW,NE,SW,SE) associada a uma posição do tanque, e a direção e distãncia de um certo disparo, devolve as jogadas que pode fazer de modo a não ser atingido por esse projétil.
accessDangerDiagA ::  Direcao -> (String,Maybe (Int,Direcao)) -> [Maybe Jogada]
accessDangerDiagA _ (_ ,Nothing)  = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
accessDangerDiagA dirJ (a, Just (n,dir)) | n < 1 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | n == 1 = if  (elem dirJ) $ delete dir (fugasPossiveis a) then [Just (Movimenta dirJ)] else [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | n == 2 = if (elem dirJ) $ delete dir (fugasPossiveis a) then [Just (Movimenta dirJ),Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque) ] else (map (\x -> Just (Movimenta x))) $ delete dir $ (fugasPossiveis a)
                                         | n == 3 = if dirJ == dirOposta dir then (listaD dirJ) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque) ] else if (elem $ dirOposta dirJ) $ delete dir (fugasPossiveis a) then (listaD dirJ) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque) ] else [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
 where sentidosOpostos direcao = [direcao, (dirOposta direcao)]
       listaD direcao = (map (\x -> Just (Movimenta x))) $ delete direcao [C,B,D,E]        
       fugasPossiveis :: String -> [Direcao]
       fugasPossiveis "NW" = [B,D]
       fugasPossiveis "NE" = [B,E]
       fugasPossiveis "SW" = [C,D]
       fugasPossiveis "SE" = [C,E]

-- | Sabendo uma direção cardinal (N,S,E,W) associada a uma posição do tanque, e a direção e distãncia de um certo disparo, devolve as jogadas que pode fazer de modo a não ser atingido por esse projétil.

accessDangerNSEW :: [(String,Maybe [(Int,Direcao)])] -> Direcao -> [Maybe Jogada]
accessDangerNSEW [("S",x),("W",y),("E",w),("N",z)] dir = jogadasComuns $ hollowSlayer [one,two,three,four] 
 where f (n,Nothing) = [(n,Nothing)] 
       f (n,Just []) = []
       f (n,Just (x:xs)) = (n,Just x) : (f (n,Just xs)) 
       length' (a,Nothing) = 0
       length' (b, Just c) = length y
       one = jogadasComuns $ hollowSlayer $ map (accessDangerNSEWa dir) (f ("S",x))
       two = jogadasComuns $ hollowSlayer $ map (accessDangerNSEWa dir) (f ("W",y))
       three = jogadasComuns $ hollowSlayer $ map (accessDangerNSEWa dir) (f ("E",w))
       four = jogadasComuns $ hollowSlayer $ map (accessDangerNSEWa dir) (f ("N",z))

-- | Remove listas vazias numa lista de listas.
hollowSlayer :: [[a]] -> [[a]]
hollowSlayer [] = []
hollowSlayer ([]:xs) = hollowSlayer xs
hollowSlayer (x:xs) = x : (hollowSlayer xs)
 
-- | Recebe uma lista de listas de jogadas, e devolve as jogadas comuns a todas.
jogadasComuns :: [[Maybe Jogada]] -> [Maybe Jogada]
jogadasComuns ([]:xss) = [] 
jogadasComuns ((x:xs):xss) = if all id (map (elem x) xss) then x : (jogadasComuns (xs:xss)) else jogadasComuns (xs:xss)

-- | Função auxiliar da função accessDangerNSEW. 
accessDangerNSEWa :: Direcao -> (String,Maybe (Int,Direcao)) -> [Maybe Jogada]
accessDangerNSEWa _ (_, Nothing)  = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
accessDangerNSEWa dirJogador (a, Just (n,dir)) = if dirJogador == dirOposta dir then possiveisJogadas1 (a, Just (n,dir)) else possiveisJogadas2 (a, Just (n,dir)) dirJogador
 
-- bala vem duma posicao NSEW e o jogador esta a olhar para ela
possiveisJogadas1 (a, Just (n,dir)) | n < 2 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                    | n == 2 = [Just (Dispara Canhao), Just (Dispara Laser)]
                                    | n == 3 = [Just (Dispara Canhao), Just (Dispara Laser), Just (Dispara Choque), Nothing] ++ listaSemMove
                                    | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
 where listaSemMove = map (\x -> (Just (Movimenta x))) $ filter (\x -> x /= dir && x /= (dirOposta dir)) [C,B,D,E]


-- | Devolve a direção do mapa associada a uma direção cardinal. 
cardinalParaDir "N" = C
cardinalParaDir "S" = B
cardinalParaDir "W" = D
cardinalParaDir "E" = E

-- | Função inversa da função "cardinalParaDir".
dirParaCardinal C = "N"
dirParaCardinal B = "S"
dirParaCardinal D = "W"
dirParaCardinal E = "E"


-- | Devolve as jogadas que o jogador deve fazer de modo a não ser atingido por um disparo. Analisa casos em que o Disparo aparece nas posições N,S,E e W, e o jogador está virado para a direção oposta da bala.
possiveisJogadas2 (a, Just (n,dir)) dirJ | n < 1 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | n == 1 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | n == 2 = [Just (Movimenta dirJ)] 
                                         | n == 3 = if dirJ == dir then [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)] else listaPos  ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
                                         | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
 where listaPos = (map (\x -> Just (Movimenta x)) ) $ filter (\x -> x /= dir) [C,B,D,E]


-- | Devolve as jogadas que o jogador deve fazer de modo a não ser atingido por um disparo. Analisa os casos em que um disparo há de atingir a posicao central das 3 posições novas que um jogador iria ocupar no caso de se movimentar.
possiveisJogadas3NSEW (a,Nothing) = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
possiveisJogadas3NSEW (a, Just (n,dir)) | dir == (dirOposta (cardinalParaDir2 a)) = f
                                        | otherwise = ((map (\x -> Just (Movimenta x))) $ filter (\x -> x /= dirOposta dir) [C,B,D,E]) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
 where f   | n == 0 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | n <= 2 = ((map (\x -> Just (Movimenta x))) $ filter (\x -> x /= dirOposta dir) [C,B,D,E]) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]

-- | Semelhante á função cardinalParaDir - devolve direções associadas ás direções cardinais.
cardinalParaDir2 "NN" = C
cardinalParaDir2 "SS" = B
cardinalParaDir2 "WW" = E
cardinalParaDir2 "EE" = D

-- | Função inversa da função "cardinalParaDir2" 
dirParaCardinal2 C = "NN"
dirParaCardinal2 B = "SS"
dirParaCardinal2 D = "WW"
dirParaCardinal2 E = "EE"

-- | Devolve as jogadas que o jogador deve fazer de modo a não ser atingido por um disparo. Analisa os casos em que um disparo há de atingir um das posicões centrais das 3 posições novas que um jogador iria ocupar no caso de se movimentar.

possiveisJogadas3Diag (a, Nothing) = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]       
possiveisJogadas3Diag (a,Just (n,dir)) | dir == dirOposta (fst $ cardiagonalParaDir a) = f
                                       | dir == dirOposta (snd $ cardiagonalParaDir a) = g
                                       | otherwise = ((map (\x -> Just (Movimenta x))) $ filter (\x -> x /= dirOposta dir) [C,B,D,E]) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]

 where f   | n == 0 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | n <= 2 = ((map (\x -> Just (Movimenta x))) $ filter (\x -> x /= (fst $ cardiagonalParaDir a)) [C,B,D,E]) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
       g   | n == 0 = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | n <= 3 = ((map (\x -> Just (Movimenta x))) $ filter (\x -> x /= (fst $ cardiagonalParaDir a)) [C,B,D,E]) ++ [Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]
           | otherwise = [Just (Movimenta C), Just (Movimenta D), Just (Movimenta E), Just (Movimenta B) , Just (Dispara Canhao), Just (Dispara Laser), Nothing, Just (Dispara Choque)]       

-- | Sabendo uma cardinal diagonal (nor-noroeste = "NNW") , devolve um par de direções - a principal e a secundaria
cardiagonalParaDir :: String -> (Direcao,Direcao)
cardiagonalParaDir "NNW" = (C,E)
cardiagonalParaDir "NNE" = (C,D)
cardiagonalParaDir "EEN" = (D,C)
cardiagonalParaDir "EES" = (D,B)
cardiagonalParaDir "WWN" = (E,C)
cardiagonalParaDir "WWS" = (E,B)
cardiagonalParaDir "SSW" = (B,E)
cardiagonalParaDir "SSE" = (B,D)





-- | As 3 novas posicoes que um jogador ocupará caso se mover na direcao em que esta a olhar
novas3 :: PosicaoGrelha -> Direcao -> [PosicaoGrelha]
novas3 (x,y) dir = if dir == C || dir == B then map (somaVetores c) [(x,y-1),(x,y),(x,y+1)] else map (somaVetores c) [(x-1,y),(x,y),(x+1,y)]
 where c = (a*2,b*2)
       (a,b) = direcaoParaVetor dir
                               
           
analisaDisparos :: [Disparo] -> [(PosicaoGrelha,Direcao)]
analisaDisparos [] = []
analisaDisparos ((DisparoChoque a b):xs) = analisaDisparos xs
analisaDisparos (x:xs) = (posicaoDisparo x,direcaoDisparo x) : (analisaDisparos xs)

-- | Conhecendo o mapa, as posicoes que o jogador ocupa, e o par de posicão e direção de todos os disparos do estado, devolve, para cada posição, a sua direção cardinal correspondente á posição do jogador e a lista de pares (direção,número de tick até ao impacto) dos disparos que estão em rota de colisão.
perigoDisparos :: Mapa -> [PosicaoGrelha] -> [(PosicaoGrelha,Direcao)] -> [(String,Maybe [(Int,Direcao)])]
perigoDisparos mp myPos [] = [("SE",Nothing),("S",Nothing),("SW",Nothing),("E",Nothing),("null",Nothing),("W",Nothing),("NE",Nothing),("N",Nothing),("NW",Nothing)]
perigoDisparos mp myPos (x:xs) = (map (\(y,z) -> (intParaDirCardinal y,z) )) $ (contagemDecrescente 1) $ perigoDisparosAux mp trueMinhasPos myPos (x:xs)
 where trueMinhasPos :: [(PosicaoGrelha,Maybe [(Int,Direcao)])] 
       trueMinhasPos = map (\x -> (x,Nothing)) myPos
-- | Função parecida com a função perigoDisparos, em vez de trabalhar com as posições ocupadas pelo jogador, trabalha com as posições que seriam ocupadas no caso deste se mover.
perigoDisparosFuturo :: Mapa -> [PosicaoGrelha] -> Direcao -> [(PosicaoGrelha,Direcao)] -> [(String,Maybe [(Int,Direcao)])]
perigoDisparosFuturo mp myPosF dirJ [] = (map (\(y,z) -> (intParaCardiagonal y,z) )) $ (map (\(x,y) -> (3 * (dirPMult dirJ) + x,y) )) $ (contagemDecrescente 1) $ perigoDisparosAux mp trueMinhasPos myPosF []
 where trueMinhasPos :: [(PosicaoGrelha,Maybe [(Int,Direcao)])] 
       trueMinhasPos = map (\x -> (x,Nothing)) myPosF
       dirPMult C = 0
       dirPMult D = 1
       dirPMult B = 2
       dirPMult E = 3
perigoDisparosFuturo mp myPosF dirJ (x:xs) = (map (\(y,z) -> (intParaCardiagonal y,z) )) $ (map (\(x,y) -> (3 * (dirPMult dirJ) + x,y) )) $ (contagemDecrescente 1) $ perigoDisparosAux mp trueMinhasPos myPosF (x:xs)
 where trueMinhasPos :: [(PosicaoGrelha,Maybe [(Int,Direcao)])] 
       trueMinhasPos = map (\x -> (x,Nothing)) myPosF
       dirPMult C = 0
       dirPMult D = 1
       dirPMult B = 2
       dirPMult E = 3



intParaCardiagonal :: Int -> String
intParaCardiagonal 1 = "NNW"
intParaCardiagonal 2 = "NN"
intParaCardiagonal 3 = "NNE"
intParaCardiagonal 4 = "EEN"
intParaCardiagonal 5 = "EE"
intParaCardiagonal 6 = "EES"
intParaCardiagonal 7 = "SSW"
intParaCardiagonal 8 = "SS"
intParaCardiagonal 9 = "SSE"
intParaCardiagonal 10 = "WWN"
intParaCardiagonal 11 = "WW"
intParaCardiagonal 12 = "WWS"






-- | Função auxiliar da função perigoDisparos, ao número associado a uma posição pela função "contagemDecrescente", associa a direção cardinal correspondente.
intParaDirCardinal :: Int -> String
intParaDirCardinal 9 = "null"
intParaDirCardinal 8 = "NW"
intParaDirCardinal 7 = "N"
intParaDirCardinal 6 = "NE"
intParaDirCardinal 5 = "W"
intParaDirCardinal 4 = "E"
intParaDirCardinal 3 = "SW"
intParaDirCardinal 2 = "S"
intParaDirCardinal 1 = "SE"

-- | Associa um número a cada elemento duma lista. Irá ser usada, juntamente com a função intParaDirCardinal para associar posições ocupadas por um jogador a uma direção cardinal.
contagemDecrescente :: Int -> [(a,b)] -> [(Int,b)]
contagemDecrescente _ [] = []
contagemDecrescente 4 (x:y:xs) = (4,snd x) : (9,snd y) :(contagemDecrescente 5 xs)
contagemDecrescente n (x:xs) = (n,snd x) : (contagemDecrescente (n+1) xs)

-- | Função auxiliar da função perigoDisparos.
perigoDisparosAux :: Mapa ->  [(PosicaoGrelha,Maybe [(Int,Direcao)])] -> [PosicaoGrelha] -> [(PosicaoGrelha,Direcao)] -> [(PosicaoGrelha,Maybe [(Int,Direcao)])]
perigoDisparosAux mp myPos myAcc [] = myPos
perigoDisparosAux mp myPos myAcc (x:xs) = perigoDisparosAux mp (posShotD mp myPos myAcc 0 x) myAcc xs

-- | Para cada posição dum jogador, indica o par (ticks até colisão, direção) dos disparos que estão em rota de colisão com dita posição.
posShotD :: Mapa -> 
 [(PosicaoGrelha,Maybe [(Int,Direcao)])] ->
 [PosicaoGrelha] -> -- posicoes ocupadas pelo jogador
 Int -> -- numero de jogadas ate a bala atingir o jogador (acumula)
 (PosicaoGrelha,Direcao) -> 
 [(PosicaoGrelha,Maybe [(Int,Direcao)])]

posShotD mp myPos myAcc ind ((pD1,pD2),dD)
 | elem (pD1,pD2) myAcc = posShotDaux ind myPos ((pD1,pD2),dD)
 | a = (posShotD mp myPos myAcc (ind+1) ((pD1+c,pD2+d),dD))
 | otherwise = myPos
   where (a,b) = pdContinuarC (checkB mp (pD1,pD2) dD)
         (c,d) = direcaoParaVetor dD
-- | Função auxiliar da função posShotD. 
posShotDaux :: Int -> [(PosicaoGrelha,Maybe [(Int,Direcao)])] -> (PosicaoGrelha,Direcao) -> [(PosicaoGrelha,Maybe [(Int,Direcao)])]
posShotDaux _ [] _ = []
posShotDaux n (x:xs) (p,d) = if p == fst x then ((fst x, (updateSDisparos n d (snd x))) :xs)  else (x:(posShotDaux n xs (p,d)))
 where updateSDisparos :: Int -> Direcao -> Maybe [(Int,Direcao)] -> Maybe [(Int,Direcao)]
       updateSDisparos n dir Nothing = Just [(n,dir)]
       updateSDisparos n dir l =  (:) <$> Just ((n,dir)) <*> l 

---- -----------------------------------------------------------------------------------------jogadores
-- | Transforma o segundo elemento dum par numa lista. Se houverem elementos da lista com posições iguais, são juntas numa só.
agregator ::  [(PosicaoGrelha,(Int,Int))] -> [(PosicaoGrelha,[(Int,Int)])]
agregator [] = []
agregator (((l1,c2),ind1):xs) = if elem (l1,c2) (map (\((x,y),z) -> (x,y)) xs) 
                                then a : (agregator b)
                                else ((l1,c2),[ind1]) : agregator xs
 where (a,b) = (agregatorA ((l1,c2),[ind1]) xs) 

-- | Função auxiliar da função agregator.
agregatorA :: (PosicaoGrelha,[(Int,Int)]) -> [(PosicaoGrelha,(Int,Int))] -> ((PosicaoGrelha,[(Int,Int)]), [(PosicaoGrelha,(Int,Int))])
agregatorA ((l1,c1),id) [] = (((l1,c1), nub id),[])
agregatorA ((l1,c1),id) (((l2,c2),id2):xs) = if l1 == l2 && c1 == c2 then (c,d) else (a , (((l2,c2),id2):b)  )

 where (a,b) = agregatorA ((l1,c1),id) xs  
       (c,d) = agregatorA ((l1,c1),(id2:id)) xs  

------------------
-- | Devolve as posições que possivelmente iriam ser afetadas por um DisparoLaser dos restantes tanques.
truePosShots :: Int -> Estado -> [(PosicaoGrelha,Int)]
truePosShots ind (Estado mapa j d) = filter (\((x,y),z) -> z /= ind ) (possibleShots ind mapa j)

----------------------------------------------------------------------------------------------------------------------------

-- | Sabendo o indice do jogador, devolve as posicoes da grelha que seriam ocupadas se este disparasse um laser
minhasTau ind (Estado mapa j d) = map (\((x,y),z) -> (x,y)) (posShotA mapa pG dir ind)
 where (Jogador pG dir vj lJ cJ) = encontraIndiceLista ind j

--------------------------------------------------------------------------------------------------------------
-- | Devolve uma lista com o par (posição do mapa possivelmente afetada por um disparoLaser, indice do jogador) para todos os jogadores.
possibleShots :: Int -> Mapa -> [Jogador] -> [(PosicaoGrelha,Int)]
possibleShots _ _ [] = []
possibleShots ind mp ((Jogador (p1,p2) dir vJ lJ cJ):js) = (posShotA mp (p1,p2) dir ind) ++ (possibleShots (ind +1) mp js) 


-- | Devolve as posicaoGrelha afetadas se um jogador disparasse um projéctil, e os jogador respectivo.
posShotA :: Mapa -> PosicaoGrelha -> Direcao -> Int -> [(PosicaoGrelha,Int)]
posShotA mp (p1,p2) dir indx = if a then ((p1,p2),indx) : (posShotA mp (p1 + c,p2 + d) dir indx) else [((p1,p2),indx)]  
 where (a,b) = pdContinuarC (checkB mp (p1,p2) dir)
       (c,d) = direcaoParaVetor dir
------------------------------------------------------------------------------------------------------------------------

-- | Sabendo o indice do jogador, devolve as posições da grelha por ele ocupadas. (sem contar com a sua posicaoGrelha)
minhasPos :: Int -> Estado -> [PosicaoGrelha]
minhasPos ind (Estado mp (j:js) (d:ds)) = (criaPos2X 1 (pos1,pos2))
 where (Jogador (pos1,pos2) d e f g) = encontraIndiceLista ind (j:js)





-----------------------------------------------------------------------------------------------------------------------------

---- Segurança relativa
 {-
7 - Numa posicao na qual bastava uma ou menos jogadas por um tanque inimigo de modo a ficar em linha de fogo direta.
3 - Não estar protegido na horizontal (Movimento vertical)
2 - Não estar protegido na vertical   (Movimento horizontal)
-}

-- | Conhecendo o indice do jogador e o estado, devolve o mapa das posições da grelha associadas com a segurança relativa. 7 - se for apenas necessária uma jogada para o jogador ficar em linha de fogo, 5 - se não houver qualquer parede entre os jogadores , 3 - se houver uma parede posicionada na vertical , 2 - se houver uma parede posicionada na horizontal.
f1 :: Int -> Estado -> [((PosicaoGrelha),[(Int,Int)])]
f1 ind (Estado mp jgs dps) =  agregator $ concat $ zipWith (f3 mp) listaPF3 trueLInd
 where pJgs = map (posicaoJogador.fst) listaJogVivos -- :: [PosicaoGrelha] sem o man
       trueLInd = map snd listaJogVivos -- :: [Int] -> indices dos gaijos  excepto o man
       listaPF3 = map (f2 mp) (pJgs) -- :: [([PosicaoGrelha],[PosicaoGrelha])] 
       listaJogVivos = assignInd jgs 0 ind 
       
-- | Indica o indice respetivo a cada jogador, ignorando jogadores mortos e o jogador selecionado.
assignInd :: [Jogador] -> Int -> Int -> [(Jogador,Int)]
assignInd [] n indx = []
assignInd (x:xs) n indx = if (vidasJogador x)  > 0 && n /= indx then ((x),n) : (assignInd xs (n+1) indx) else assignInd xs (n+1) indx

--- | Auxiliar da f3 (+), cria as posições afetadas por 4 disparosLaser por todas as direções de um jogador.
f2 ::  Mapa -> PosicaoGrelha -> ([PosicaoGrelha],[PosicaoGrelha])
f2 mapa (l1,c1) = ( (map (\((x,y),z) -> (x,y)) a) , (map (\((x,y),z) -> (x,y)) b))
 where a =  (nub $ (init $ posShotA mapa (l1-1,c1) C 1) ++  (init $ posShotA mapa (l1+1,c1) B 1))
       b =  (nub $ (init $ posShotA mapa (l1,c1-1) E 1) ++  (init $ posShotA mapa (l1,c1+1) D 1 ))
       eBordaMatriz' a b = not (eBordaMatriz b a)


-- | Cria o mapa de posições grelha associados á segurança relativa.
f3 ::  Mapa -> ([PosicaoGrelha],[PosicaoGrelha]) ->  Int -> [(PosicaoGrelha,(Int,Int))] 
f3 mapa (a,b) ind = f3Aux [] [] (trueListAB ++ (concat $ (map (posShotT mapa E ind) a) ++ (map (posShotT mapa D ind) a) ++ (map (posShotT mapa C ind) b) ++ (map (posShotT mapa B ind) b)))
 where listAB = (a++b)
       trueListAB = (\(x,y)(w,z) -> ((x,y),(w,z))) <$> listAB <*> [(ind,7)]  


       

-- | Junta a segurança relativa de elementos com posições iguais.
f3Aux :: [(PosicaoGrelha,(Int,Int))] -> [(PosicaoGrelha,(Int,Int))] -> [(PosicaoGrelha,(Int,Int))] -> [(PosicaoGrelha,(Int,Int))]
f3Aux l1 l2 [] = l1
f3Aux [] [] (x:xs) = f3Aux [x] [x] xs
f3Aux [] l (x:xs) = f3Aux (x:l) (x:l) xs
f3Aux ((p1,(indx,sIndx)):xs) l ((p2,(ind2,sInd2)):ys)= if p1 == p2 
                                                       then f3Aux ((p1,(indx,sIndx+sInd2)):(filter (\(x,y) -> x /= p1) l)) ((p1,(indx,sIndx+sInd2)):(filter (\(x,y) -> x /= p1) l)) ys
                                                       else f3Aux xs l ((p2,(ind2,sInd2)):ys) 
-- | Função auxiliar da função f3, chama a função posShotAS
posShotT :: Mapa -> Direcao -> Int -> PosicaoGrelha -> [(PosicaoGrelha,(Int,Int))]
posShotT mp dir int (l1,c1) = posShotAS mp (l1+a,c1+b) dir int
 where (a,b) = direcaoParaVetor dir 
-- | 
posShotAS :: Mapa -> PosicaoGrelha -> Direcao -> Int -> [(PosicaoGrelha,(Int,Int))]
posShotAS mp (p1,p2) dir indx = if a then ((p1,p2),(indx,sIndx)) : (posShotAS mp (p1 + c,p2 + d) dir indx) else []  
 where (a,b) = pdContinuarC (checkB mp (p1,p2) dir)
       (c,d) = direcaoParaVetor dir
       sIndx = dirParaSeguranca dir

dirParaSeguranca :: Direcao -> Int
dirParaSeguranca C = 2
dirParaSeguranca B = 2
dirParaSeguranca E = 3
dirParaSeguranca D = 3




-------------------- pathfinder

-- | Cria uma lista  com todas as posicoes da grelha de um mapa que podem ser ocupadas por um tanque
posGM :: Mapa -> [PosicaoGrelha]
posGM m = g1Aux (l,c)  m 
 where l = (length m)-3
       c = (length $ head m )-3

-- | Auxiliar da funcao posGM
g1Aux :: (Int, Int) -> Mapa -> [PosicaoGrelha]
g1Aux (a,b) mp = filter (  (h).(posicoesBG mp)  ) (criaPosX' (a,b) (1,1))
 where h l = not (elem (Bloco Indestrutivel) l)

-- | Ve os blocos a volta de uma PosicaGrelha
posicoesBG :: Mapa -> PosicaoGrelha -> [Peca]
posicoesBG mp (l1,c1) = map (encontraPosicaoMatriz' mp) [(l1,c1),(l1,c1+1),(l1+1,c1),(l1+1,c1+1)]


encontraPosicaoMatriz' ::  Matriz a -> Posicao -> a
encontraPosicaoMatriz' a (l,c)  = encontraIndiceLista c (encontraIndiceLista l a) 


criaPosY' :: Int -> PosicaoGrelha -> [PosicaoGrelha]
criaPosY' z (x,y) = if z > y then ((x,y) : (criaPosY' z (x,y+1)))
                             else [(x,y)]

criaPosX' :: (Int,Int) -> PosicaoGrelha -> [PosicaoGrelha]
criaPosX' (a,b) (x,y) = if a > x then (criaPosY' b (x,y)) ++ (criaPosX' (a,b) (x+1,y))
                        else criaPosY' b (x,y) 

-- | Sabendo um mapa, dá as posições juntas ao jogador ás quais este se deve mover de modo a chegar a um destino indicado.
pathfinder :: PosicaoGrelha -> PosicaoGrelha -> Mapa -> [PosicaoGrelha]
pathfinder dest part mp = (map (\(x,y) -> x)) $ filter (\(a,(Just b)) -> b+1 == mb) listaEscolha
 where inicial = jogadasNecessarias1 dest (posGM mp)
       final = assignJNa3 0 inicial
       (partM,(Just mb)) = pegaPG part final
       listaEscolha = filter (\(x,z) -> elem x (posicoesAVolta part)) final
       (x,z) = escolheM [] listaEscolha

-- | Verifica se uma posGrelha está a beira de outra
estaBeira :: PosicaoGrelha -> PosicaoGrelha -> Bool
estaBeira (l1,c1) (l2,c2) = ((abs (l1 - l2) == 1) && (c1 == c2)) ||  ((abs (c1 - c2) == 1) && (l1 == l2))

-- | Assinala 0 a posicaoGrelha destino e nothing ao resto
jogadasNecessarias1 :: PosicaoGrelha -> [PosicaoGrelha] -> [(PosicaoGrelha, Maybe Int)]
jogadasNecessarias1 _ [] = []
jogadasNecessarias1 p1 (p2:xs) = (if p1 == p2 then (p2,Just 0) else (p2,Nothing)) : (jogadasNecessarias1 p1 xs)

-- | Função auxiliar da funcao assignJNa3. Substitui o valor "Nothing" de uma posicaoGrelha que está a beira duma posicaoGrelha com o valor "Just n" por "Just n+1". Se já estiver um valor assinalado, é ignorada.
assignJN :: (PosicaoGrelha,Maybe Int) -> [(PosicaoGrelha, Maybe Int)] -> [(PosicaoGrelha, Maybe Int)]
assignJN x [] = []
assignJN (pG,Just d) ((x,Nothing):xs) = (if (estaBeira pG x) then (x,Just (d+1)) else (x,Nothing)) : (assignJN (pG, Just d) xs) 
assignJN (pG,Just d) ((x,a):xs) = (x,a) : (assignJN (pG, Just d) xs) 

-- | Função auxiliar da função assignJNa3
assignJNa2 :: [(PosicaoGrelha, Maybe Int)] -> [(PosicaoGrelha, Maybe Int)] -> [(PosicaoGrelha, Maybe Int)]
assignJNa2 [] l = l
assignJNa2 (x:xs) (y:ys) = assignJNa2 xs (assignJN x (y:ys))


-- | Faz um mapa de posiçõesGrelha associadas ao respectivo número de turnos necessarios para chegar ao ponto desejado. Se não houver nenhum caminho possível, o valor associado é "Nothing".
assignJNa3 :: Int -> [(PosicaoGrelha, Maybe Int)] -> [(PosicaoGrelha, Maybe Int)]
assignJNa3 n l = if (any' estaBeira (map (\(x,y) -> x)  trueL) (posOnlyL $ filter (\(x,y) -> y == Nothing) l))
 then assignJNa3 (n+1) updtL else l
 where trueL = filter (\(x,y) -> y == (Just n)) l
       updtL = (assignJNa2 trueL l)
       posOnlyL listaPGM = map (\(x,y) -> x) listaPGM
       h = estaBeira

-- | Função de ordem superior, trabalha com uma função que recebe 2 elementos e devolve um bool e aplica-a a 2 listas.
any' :: (a -> a -> Bool) -> [a] -> [a] -> Bool
any' f _ [] = False
any' f [] _ = False
any' f (x:xs) (y:ys) = any (f x) (y:ys) || any' f xs (y:ys) 


-- | Faz um caminho entre duas "posicaoGrelha". A primeira que recebe é a final, a segunda que recebe é a inicial.
testePathFinder :: PosicaoGrelha -> PosicaoGrelha -> Mapa -> [PosicaoGrelha]
testePathFinder dest part mp = if dest == part then [dest] else (part :  (testePathFinder dest x mp)  ) 
 where inicial = jogadasNecessarias1 dest (posGM mp)
       final = assignJNa3 0 inicial
       (partM,(Just mb)) = pegaPG part final
       listaEscolha = filter (\(x,z) -> elem x (posicoesAVolta part)) final
       (x,z) = escolheM [] listaEscolha

-- | Recebe uma lista com pares de "posicaoGrelha" e um valor Maybe n e devolve aposicaoGrelha com o valor n minimo.
escolheM :: [(PosicaoGrelha, Maybe Int)] -> [(PosicaoGrelha, Maybe Int)] -> (PosicaoGrelha, Maybe Int)
escolheM [x] [] = x
escolheM [] (x:xs) = escolheM [x] xs
escolheM [(p,(Just x))] ((p2,(Just z)):xs) = escolheM [(if x > z then (p2,(Just z)) else (p,(Just x)) )] xs
escolheM [(p,(Just x))] ((p2,Nothing):xs) = escolheM [(p,(Just x))] xs

-- | Sabendo uma posicaoGrelha, devolve o par dessa posicao com o número de jogadas necessárias para chegar a uma posicaoGrelha.
pegaPG :: PosicaoGrelha -> [(PosicaoGrelha, Maybe Int)] -> (PosicaoGrelha, Maybe Int)
pegaPG p [] = error "erro"
pegaPG p ((p2,mInt):xs) = if p == p2 then (p2,mInt) else pegaPG p xs

-- | Devolve as 4 posições a distância 1 duma posicaoGrelha conhecida.
posicoesAVolta (l1,c1) = [(l1+1,c1),(l1-1,c1),(l1,c1+1),(l1,c1-1)]


-----------------2

