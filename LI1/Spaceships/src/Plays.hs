-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Plays where

import Data
import Functions
import MapEditor
import Data.List
-- * Testes


-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta C,(Estado (mapaInicial (5,5)) [(Jogador (1,1) D 1 1 1)] [])),
 (0, Movimenta B,(Estado (mapaInicial (5,5)) [(Jogador (1,1) C 1 1 1)] [])), -- mover para uma direção sem estar virado para essa direção
 (0, Movimenta D,(Estado (mapaInicial (5,5)) [(Jogador (1,1) C 0 1 1)] [])), -- mover para uma direção sem estar virado para essa direção morto
 (0, Movimenta D,(Estado (mapaInicial (5,5)) [(Jogador (1,1) D 1 1 1)] [])), -- mover para uma direção virado para essa direção
 (0, Movimenta D,(Estado (mapaInicial (5,5)) [(Jogador (1,1) D 0 1 1)] [])), -- mover para uma direção virado para essa direção morto
 (0, Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (3,4) D 1 1 1),(Jogador (3,6) D 1 1 1)] [])), --mover para onde ja há um tanque
 (0, Movimenta C, (Estado (mapaInicial (10,10)) [(Jogador (3,1) C 1 1 1)] [])), -- mover para cima onde há uma parede 
 (0, Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (3,1) B 1 1 1),(Jogador (3,6) D 1 1 1)] [(DisparoChoque 1 3)])), 
  -- mudar direção imobilizado
 (0, Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 1 1 1),(Jogador (3,6) D 1 1 1)] [(DisparoChoque 1 3)])),
  -- mover imobilizado
 (0, Movimenta D, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 1 1 1),(Jogador (3,6) D 1 1 1)] [(DisparoChoque 0 3)])),
  -- mover com o seu areaChoque
 (0, Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 0 1 1)] [])), -- dispara Choque Morto
 (0, Dispara Canhao, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 0 1 1)] [])), -- dispara Canhao morto
 (0, Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 0 1 1)] [])),  -- dispara Laser morto
 (0, Dispara Choque, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 1 1 0)] [])),  -- dispara Choque sem Choques
 (0, Dispara Laser, (Estado (mapaInicial (10,10)) [(Jogador (3,1) D 1 0 1)] []))] -- dispara Laser sem lasers

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada n (Movimenta d) (Estado m (j:js) dis)
 = if vJ > 0 then 
     if (d /= dJ) 
     then (Estado m (atualizaIndiceLista n (Jogador (pJ1, pJ2) d vJ lJ chqJ) (j:js)) dis) 
     else if (podeMover d n (Estado m (j:js) dis)) 
          then (Estado m (atualizaIndiceLista n (Jogador (pJ1 + a, pJ2 + b) dJ vJ lJ chqJ) (j:js)) dis) 
          else (Estado m (j:js) dis) 
   else (Estado m (j:js) dis)
                where (Jogador (pJ1,pJ2) dJ vJ lJ chqJ) = (encontraIndiceLista n (j:js))
                      (a,b) = direcaoParaVetor d

jogada n (Dispara Choque) (Estado mp (j:js) d) | vJ /= 0 && chqJ >= 1 = (Estado mp (atualizaIndiceLista n (Jogador pJ dJ vJ lJ (chqJ-1)) (j:js))  ((DisparoChoque n 5):d))
                                               | otherwise = (Estado mp (j:js) (d))
  where (Jogador pJ dJ vJ lJ chqJ) = (encontraIndiceLista n (j:js))

jogada n (Dispara Laser)
         (Estado mp (j:js) d)
         = if (vJ /= 0 && lJ >= 1) then (Estado mp (atualizaIndiceLista n (Jogador (pJ1,pJ2) dJ vJ (lJ-1) chqJ) (j:js))  
                                        ((DisparoLaser n (pJ1+dJ1,pJ2+dJ2) dJ):d))
                                   else (Estado mp (j:js) (d)) 
                                          where (Jogador (pJ1,pJ2) dJ vJ lJ chqJ) = (encontraIndiceLista n (j:js))
                                                (dJ1,dJ2) = (direcaoParaVetor dJ)

jogada n (Dispara Canhao) (Estado mp (j:js) d) | vJ /= 0 = (Estado mp (j:js)  ((DisparoCanhao n (pJ1+dJ1,pJ2+dJ2) dJ):d))
                                               | otherwise = (Estado mp (j:js) (d))
 where (Jogador (pJ1,pJ2) dJ vJ lJ chqJ) = (encontraIndiceLista n (j:js))
       (dJ1,dJ2) = (direcaoParaVetor dJ)

-- ** Funções auxiliares da função 'jogada'

-- | Verifica se o 'Jogador' pode mover para uma dada 'Direcao'
podeMover :: Direcao -> Int -> Estado -> Bool
podeMover dJnv nJ (Estado mp (j:js) d) =  (verificaMovimento (pGrelhaParaMapa (xj+a,yj+b) mp)) && 
                                          (verificaPosicao (xj,yj,nJ) (areaChoque (Estado mp (j:js) d))) &&
                                          (not (elem (xj + a, yj + b) (juntaListasOS (criaPos2X 1) (delete (xj,yj) (map posicaoJogador (retirasemvidas(j:js)))))))
                   
   where (Jogador (xj,yj) dJ vj lJ chqJ ) = encontraIndiceLista nJ (j:js) 
         (a,b) = direcaoParaVetor dJnv

-- | Retira os 'Jogador's sem vidas da lista de jogadores
retirasemvidas :: [Jogador] -> [Jogador]
retirasemvidas [] = []
retirasemvidas (j:js) = if (vidasJogador j) == 0 then (retirasemvidas js) else (j:(retirasemvidas js))

-- | Condição usada para verificar se existe 'Parede' na direção do 'Jogador' (É usada no 'podeMover')
verificaMovimento :: Mapa -> Bool
verificaMovimento [[Vazia,Vazia],[Vazia,Vazia]] = True
verificaMovimento _ = False


juntaListasOS :: (a -> [a]) -> [a] -> [a]
juntaListasOS _ [] = []
juntaListasOS f (l:ls) = (juntaListasOS f ls) ++ (f l) 

-- |Funcao auxiliar da função podeMover. A partir de um triplo (Posicao, Indice do jogador), determina se um jogador está dentro da área de choque de outro jogador. 
verificaPosicao :: (Int,Int,Int) -> [(Int,Int,Int)] -> Bool
verificaPosicao (x,y,z) [] = True
verificaPosicao (x,y,z) ((a,b,c):xs) |x == a && y == b = if z == c then verificaPosicao (x,y,z) xs else False
                                     |otherwise = verificaPosicao (x,y,z) xs

-- | Apartir de uma 'PosicaoGrelha' indica as 4 'Pecas' correspondentes num certo 'Mapa'
pGrelhaParaMapa :: PosicaoGrelha -> Mapa -> Mapa
pGrelhaParaMapa (x,y) m = [[(encontraPosicaoMatriz (x,y) m),(encontraPosicaoMatriz (x,(y+1)) m)],[(encontraPosicaoMatriz ((x+1),y) m),(encontraPosicaoMatriz ((x+1),(y+1)) m)]]

-- | Apartir de um 'Estado' devolve uma lista de posicoes afetadas pelos 'DisparoChoque'
areaChoque :: Estado 
           -> [(Int,Int,Int)] -- ^ (Posicao X,Posicao Y,Indice do 'Jogador' que disparou)
areaChoque (Estado _ _ []) = []
areaChoque (Estado mp lJ ((DisparoChoque nJ t):ds)) = if t > 0 then ((map (assign nJ) (criaPosX 3 pJ)) ++ (areaChoque (Estado mp lJ ds)))                                                               else (areaChoque (Estado mp lJ ds)) 
 where (Jogador pJ dJ vJ lsrJ chqJ) = (encontraIndiceLista nJ lJ)
areaChoque (Estado mp lJ (_:ds)) = areaChoque (Estado mp lJ ds)

-- | Adiciona um número inteiro a um duplo formando assim um tuplo (função auxiliar usada para associar indices a posições na função 'areaChoque')
assign :: Int ->  (Int,Int) -> (Int,Int,Int)
assign n (a,b) = (a,b,n)

criaPos2Y :: Int -> PosicaoGrelha -> [(Int,Int)]
criaPos2Y (-1) (x,y) = [(x,y-1)]
criaPos2Y z (x,y) = ((x,y+z) : criaPos2Y (z-1) (x,y))

criaPos2X :: Int -> PosicaoGrelha -> [(Int,Int)]
criaPos2X (-1) (x,y) = (criaPos2Y 1 (x-1,y))
criaPos2X z (x,y) = (criaPos2Y 1 (x+z,y)) ++ (criaPos2X (z-1) (x,y))

criaPosY :: Int -> PosicaoGrelha -> [(Int,Int)]
criaPosY (-3) (x,y) = [(x,y-3)]
criaPosY z (x,y) = ((x,y+z) : criaPosY (z-1) (x,y))

criaPosX :: Int -> PosicaoGrelha -> [(Int,Int)]
criaPosX (-3) (x,y) = (criaPosY 3 (x-3,y))
criaPosX z (x,y) = (criaPosY 3 (x+z,y)) ++ (criaPosX (z-1) (x,y))
