{-| Este módulo define funções comuns da Tarefa 5 do trabalho prático.

O objetivo desta tarefa é juntar as funções criadas em tarefas anteriores e criar uma interface gráfica para fazer um jogo.

- Desafio : Escolher o tema e arranjar imagens para o jogo.

Como tema do nosso jogo escolhemos o espaço, cada jogador é uma nave espacial e os Blocos Destrutíves são asteroides e os Blocos Indestrutíveis são pedras Impenetráveis

A maior partes as imagens são desenhadas com um editor de imagem (Gimp) à excessão de alguns elementos

- Desafio : Como adicionar vários menus

Para conseguirmos cumprir este desafio aumentamos o nosso estado de modo a que possuísse um número que representasse o estado

- Entre 0 e 1 -> Menu Inicial
- Entre 1 e 2 -> Jogo em andamento
- Entre 2 e 3 -> Jogo em Pausa
- Entre 3 e 4 -> Jogo Terminou

Os pontos acima indicam o que cada numero representa

- Desafio : Como fazer um contador de tempo

Para conseguirmos cumprir este desafio aumentamos o nosso estado de modo a que recebesse um inteiro a qual era adicionado uma unidade sempre que passava um tick.
Como a framerate do nosso jogo é 4 frames por segundo é necessário dividir este número por quatro antes de ser desenhado.

- Desafio : Opções de jogo.

O jogador pode escolher jogar com outros amigos no mesmo PC (PvP) ou contra 3 bots programados na tarefa 6.
Também pode escolher de entre 5 mapas, reiniciar ou para jogo.
O jogo acaba assim que passar um minuto desde o início do jogo ou se só restar apenas um ou nenhum jogador.

Em conclusão, a nossa interface gráfica é simples mas de fácil interpretação.
            -}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Maybe
import Data.List
import Data.Char
import LI11819
import Tarefa1_2018li1g077
import Tarefa2_2018li1g077
import Tarefa0_2018li1g077
import Tarefa4_2018li1g077
import Tarefa6_2018li1g077

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.

type EstadoMain = (Estado,Float,Int,Int,[Picture])

-- | Função para carregar as imagens

loadIMG :: IO[Picture]
loadIMG = do imgMenu0 <- loadJuicy "Images/maintitle.png"                            --0
             blocoDestrutivel <- loadJuicy "Images/BlocoDestrutivel.png"             --1
             blocoIndestrutivel <- loadJuicy "Images/BlocoIndestrutivel.png"         --2
             player0canon <- loadJuicy "Images/player0canon.png"                     --3
             player1canon <- loadJuicy "Images/player1canon.png"                     --4
             player2canon <- loadJuicy "Images/player2canon.png"                     --5
             player3canon <- loadJuicy "Images/player3canon.png"                     --6
             player0choque <- loadJuicy "Images/player0choque.png"                   --7
             player1choque <- loadJuicy "Images/player1choque.png"                   --8
             player2choque <- loadJuicy "Images/player2choque.png"                   --9
             player3choque <- loadJuicy "Images/player3choque.png"                   --10
             player0 <- loadJuicy "Images/player0.png"                               --11
             player1 <- loadJuicy "Images/player1.png"                               --12
             player2 <- loadJuicy "Images/player2.png"                               --13
             player3 <- loadJuicy "Images/player3.png"                               --14
             maintitle1 <- loadJuicy "Images/MainTitle1.png"                         --15
             maintitle2 <- loadJuicy "Images/MainTitle2.png"                         --16
             player0laser <- loadJuicy "Images/player0laser.png"                     --17
             player1laser <- loadJuicy "Images/player1laser.png"                     --18
             player2laser <- loadJuicy "Images/player2laser.png"                     --19
             player3laser <- loadJuicy "Images/player3laser.png"                     --20
             background <- loadJuicy "Images/spacebackground.png"                    --21
             score <- loadJuicy "Images/score.png"                                   --22
             zero <- loadJuicy "Images/0.png"                                        --23
             one <- loadJuicy "Images/1.png"                                         --24
             two <- loadJuicy "Images/2.png"                                         --25
             three <- loadJuicy "Images/3.png"                                       --26
             four <- loadJuicy "Images/4.png"                                        --27
             five <- loadJuicy "Images/5.png"                                        --28
             pause1 <- loadJuicy "Images/pause1.jpg"                                 --29
             pause2 <- loadJuicy "Images/pause2.jpg"                                 --30
             final0 <- loadJuicy "Images/final0.jpg"                                 --31
             final1 <- loadJuicy "Images/final1.jpg"                                 --32
             final2 <- loadJuicy "Images/final2.jpg"                                 --33
             final3 <- loadJuicy "Images/final3.jpg"                                 --34
             final4 <- loadJuicy "Images/final4.jpg"                                 --35
             return (map fromJust [imgMenu0,blocoDestrutivel,blocoIndestrutivel,player0canon,player1canon,player2canon,player3canon,player0choque,player1choque,player2choque,player3choque,player0,player1,player2,player3,maintitle1,maintitle2,player0laser,player1laser,player2laser,player3laser,background,score,zero,one,two,three,four,five,pause1,pause2,final0,final1,final2,final3,final4])

-- | Cria o ecrã do jogo

disM :: Display
disM = InWindow "Spaceships" (1920,1080) (0,0)

-- | Dá o estado inicial do jogo

estadoInicial :: [Picture] -> EstadoMain
estadoInicial loadedIMG = ((Estado [] [] []),0,0,0,loadedIMG)

-- | Desenha o jogo dependendo do seu 'EstadoMain'

desenhaEstado :: EstadoMain -> Picture
desenhaEstado (s,0,_,_,pics) = (pics !! 0)
desenhaEstado (s,(0.1),_,_,pics) = (pics !! 16)
desenhaEstado (s,(0.2),_,_,pics) = (pics !! 15)
desenhaEstado (s,n,a,b,pics) | (n < 2) && (n >1) = (Pictures ((pics !! 21):(Translate (-770) (-30) (pics !! 22)):[(Translate (-455) (400)(scale (1.4) (1.4) (Pictures (drawall (drawmap (s,n,a,b,pics) (0,0)) (drawplayers (s,n,a,b,pics) 0) (drawshots (s,n,a,b,pics))(concat (drawlasers (s,n,a,b,pics)))))))]++[(drawscore(drawnumboflives (s,n,a,b,pics) 0)(drawnumboflazers (s,n,a,b,pics) 0)(drawnumbofchoque (s,n,a,b,pics) 0))]++[(Translate 650 330 (text (show (div a 4))))]))
desenhaEstado (s,(2.1),_,_,pics) = (Translate 0 (-80) (pics !! 29))
desenhaEstado (s,(2.3),_,_,pics) = (Translate 0 (-80) (pics !! 29))
desenhaEstado (s,(2.2),_,_,pics) = (Translate 0 (-80) (pics !! 30))
desenhaEstado (s,(2.4),_,_,pics) = (Translate 0 (-80) (pics !! 30))
desenhaEstado (s,(3.0),_,_,pics) = (Translate 0 (-80) (pics !! 31))
desenhaEstado (s,(3.1),_,_,pics) = (Translate 0 (-80) (pics !! 32))
desenhaEstado (s,(3.2),_,_,pics) = (Translate 0 (-80) (pics !! 33))
desenhaEstado (s,(3.3),_,_,pics) = (Translate 0 (-80) (pics !! 34))
desenhaEstado (s,(3.4),_,_,pics) = (Translate 0 (-80) (pics !! 35))

-- | Junta um conjunto de figuras

drawall :: [Picture] -> [Picture] -> [Picture] -> [Picture] -> [Picture]
drawall pics1 pics2 pics3 pics4 = pics1 ++ pics2 ++ pics3 ++ pics4

-- | Escolhe a imagem do choque a usar dependendo do jogador que o usou

escolhechoque :: Int -> [Picture] -> Picture
escolhechoque j pics = case j of 0 -> (pics !! 7)
                                 1 -> (pics !! 8)
                                 2 -> (pics !! 9)
                                 3 -> (pics !! 10)

-- | Escolhe a imagem do canhão a usar dependendo da sua direção e do jogador que o usou 

escolhecanhao :: Int -> Direcao -> [Picture] -> Picture
escolhecanhao j d pics = case j of 0 -> case d of C -> (pics !! 3)
                                                  D -> rotate 90(pics !! 3)
                                                  B -> rotate 180(pics !! 3)
                                                  E -> rotate 270(pics !! 3)
                                   1 -> case d of C -> (pics !! 4)
                                                  D -> rotate 90(pics !! 4)
                                                  B -> rotate 180(pics !! 4)
                                                  E -> rotate 270(pics !! 4)
                                   2 -> case d of C -> (pics !! 5)
                                                  D -> rotate 90(pics !! 5)
                                                  B -> rotate 180(pics !! 5)
                                                  E -> rotate 270(pics !! 5)
                                   3 -> case d of C -> (pics !! 6)
                                                  D -> rotate 90(pics !! 6)
                                                  B -> rotate 180(pics !! 6)
                                                  E -> rotate 270(pics !! 6)

-- | Escolhe a imagem do jogador a usar dependendo do seu indice na lista de jogadores e da sua direção

escolhejogador :: Int -> Direcao -> [Picture] -> Picture
escolhejogador x d pics = case x of 0 -> case d of C -> (pics !! 11)
                                                   D -> rotate 90(pics !! 11)
                                                   B -> rotate 180(pics !! 11)
                                                   E -> rotate 270(pics !! 11)
                                    1 -> case d of C -> (pics !! 12)
                                                   D -> rotate 90(pics !! 12)
                                                   B -> rotate 180(pics !! 12)
                                                   E -> rotate 270(pics !! 12)
                                    2 -> case d of C -> (pics !! 13)
                                                   D -> rotate 90(pics !! 13)
                                                   B -> rotate 180(pics !! 13)
                                                   E -> rotate 270(pics !! 13)
                                    3 -> case d of C -> (pics !! 14)
                                                   D -> rotate 90(pics !! 14)
                                                   B -> rotate 180(pics !! 14)
                                                   E -> rotate 270(pics !! 14)

-- | Escolhe a imagem do laser a usar dependendo da sua direção e do jogador que o usou

escolhelaser :: Int -> Direcao -> [Picture] -> Picture
escolhelaser x d pics = case x of 0 -> case d of C -> (pics !! 17)
                                                 D -> rotate 90(pics !! 17)
                                                 B -> rotate 180(pics !! 17)
                                                 E -> rotate 270(pics !! 17)
                                  1 -> case d of C -> (pics !! 18)
                                                 D -> rotate 90(pics !! 18)
                                                 B -> rotate 180(pics !! 18)
                                                 E -> rotate 270(pics !! 18)
                                  2 -> case d of C -> (pics !! 19)
                                                 D -> rotate 90(pics !! 19)
                                                 B -> rotate 180(pics !! 19)
                                                 E -> rotate 270(pics !! 19)
                                  3 -> case d of C -> (pics !! 20)
                                                 D -> rotate 90(pics !! 20)
                                                 B -> rotate 180(pics !! 20)
                                                 E -> rotate 270(pics !! 20)

drawscore :: [Picture] -> [Picture] -> [Picture] -> Picture
drawscore pics1 pics2 pics3 = Pictures((Translate (-920) (390) (Pictures pics1)):(Translate (-740) (335) (Pictures pics3)):[(Translate (-920) (290) (Pictures pics2))])

-- | Desenha o número de vidas dos 'Jogador'es

drawnumboflives :: EstadoMain -> Int -> [Picture]
drawnumboflives ((Estado _ [] _),_,_,_,_) _ = []
drawnumboflives ((Estado mp ((Jogador (x,y) d v l c):xs) ds),n,ab,ac,pics) a | (v == 0) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 23))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                             | (v == 1) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 24))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                             | (v == 2) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 25))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                             | (v == 3) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 26))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                             | (v == 4) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 27))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                             | (v == 5) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 28))): drawnumboflives ((Estado mp xs ds),n,ab,ac,pics) (a+1)

-- | Desenha o número de lasers dos 'Jogador'es

drawnumboflazers :: EstadoMain -> Int -> [Picture]
drawnumboflazers ((Estado _ [] _),_,_,_,_) _ = []
drawnumboflazers ((Estado mp ((Jogador (x,y) d v l c):xs) ds),n,ab,ac,pics) a | (l == 0) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 23))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (l == 1) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 24))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (l == 2) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 25))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (l == 3) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 26))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (l == 4) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 27))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (l == 5) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 28))): drawnumboflazers ((Estado mp xs ds),n,ab,ac,pics) (a+1)

-- | Desenha o número de choques dos 'Jogador'es

drawnumbofchoque :: EstadoMain -> Int -> [Picture]
drawnumbofchoque ((Estado _ [] _),_,_,_,_) _ = []
drawnumbofchoque ((Estado mp ((Jogador (x,y) d v l c):xs) ds),n,ab,ac,pics) a | (c == 0) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 23))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (c == 1) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 24))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (c == 2) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 25))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (c == 3) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 26))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (c == 4) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 27))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                              | (c == 5) = (Translate 0 (fromIntegral (a*(-240))) (scale (0.4) (0.4) (pics !! 28))): drawnumbofchoque ((Estado mp xs ds),n,ab,ac,pics) (a+1)

-- | Desenha o 'Mapa'

drawmap :: EstadoMain -> Posicao -> [Picture]
drawmap ((Estado [] _ _),_,_,_,_) _ = []
drawmap ((Estado ([]:xs) jd ds),n,a,b,loadedIMG) (x,y) = drawmap ((Estado xs jd ds),n,a,b,loadedIMG) (0,(y+1))
drawmap ((Estado ((x1:x2):xs) jd ds),n,a,b,loadedIMG) (x,y) | (x1 == Bloco Destrutivel) = (Translate (fromIntegral(x*30)) (fromIntegral(y*(-30))) (loadedIMG !! 1)): (drawmap ((Estado (x2:xs) jd ds),n,a,b,loadedIMG) (x+1,y))
                                                            | (x1 == Bloco Indestrutivel) = (Translate (fromIntegral(x*30)) (fromIntegral(y*(-30))) (loadedIMG !! 2)): (drawmap ((Estado (x2:xs) jd ds),n,a,b,loadedIMG) (x+1,y))
                                                            | (x1 == Vazia) = (drawmap ((Estado (x2:xs) jd ds),n,a,b,loadedIMG) (x+1,y))

-- | Desenha os 'DisparoCanhao' e 'DiparoChoque'

drawshots :: EstadoMain -> [Picture]
drawshots ((Estado _ _ []),_,_,_,_) = []
drawshots ((Estado mp jd ((DisparoCanhao j (x,y) d):xs)),n,a,b,loadedIMG) = (Translate ((fromIntegral(y*30)+15)) (fromIntegral(x*(-30))+(-15)) (escolhecanhao j d loadedIMG)):drawshots ((Estado mp jd xs),n,a,b,loadedIMG)
drawshots ((Estado mp jd ((DisparoChoque j time):xs)),n,a,b,loadedIMG) = (Translate (fromIntegral (y*30)+15) (fromIntegral (x*(-30))+(-15)) (escolhechoque j loadedIMG)):drawshots ((Estado mp jd xs),n,a,b,loadedIMG)
    where (x,y) = case j of 0 -> posicaoJogador (jd !! 0)
                            1 -> posicaoJogador (jd !! 1)
                            2 -> posicaoJogador (jd !! 2)
                            3 -> posicaoJogador (jd !! 3)
drawshots ((Estado mp jd (_:xs)),n,a,b,loadedIMG) = drawshots ((Estado mp jd xs),n,a,b,loadedIMG)

-- | Desenha todos os 'DisparoLaser'

drawlasers :: EstadoMain -> [[Picture]]
drawlasers ((Estado _ _ []),_,_,_,_) = []
drawlasers ((Estado mp jd ((DisparoLaser j (x,y) d):xs)),n,a,b,loadedIMG) = (drawonelaser j d (posicoesLaser (x,y) d mp) loadedIMG):(drawlasers ((Estado mp jd xs),n,a,b,loadedIMG))
drawlasers ((Estado mp jd (_:xs)),n,a,b,loadedIMG) = drawlasers ((Estado mp jd xs),n,a,b,loadedIMG)

-- | Desenha um 'DisparoLaser'

drawonelaser :: Int -> Direcao -> [PosicaoGrelha] -> [Picture] -> [Picture]
drawonelaser _ _ [] _ = []
drawonelaser j d ((x,y):xs) pics = (Translate ((fromIntegral(y*30)+15)) (fromIntegral(x*(-30))+(-15)) (escolhelaser j d pics)):(drawonelaser j d xs pics)

-- | Encontra as posições afetadas por um 'DisparoLaser'

posicoesLaser :: PosicaoGrelha -> Direcao -> Mapa -> [PosicaoGrelha]
posicoesLaser (x,y) C mp |(((encontraPosicaoMatriz (x,y) mp) == (Bloco Indestrutivel)) || ((encontraPosicaoMatriz (x,(y+1)) mp) == (Bloco Indestrutivel))) = []
                         | otherwise = ((x,y):(posicoesLaser ((x-1),y) C mp))
posicoesLaser (x,y) D mp |(((encontraPosicaoMatriz (x,(y+1)) mp) == (Bloco Indestrutivel)) || ((encontraPosicaoMatriz ((x+1),(y+1)) mp) == (Bloco Indestrutivel))) = []
                         | otherwise = ((x,y):(posicoesLaser (x,(y+1)) D mp))
posicoesLaser (x,y) B mp |(((encontraPosicaoMatriz ((x+1),y) mp) == (Bloco Indestrutivel)) || ((encontraPosicaoMatriz ((x+1),(y+1)) mp) == (Bloco Indestrutivel))) = []
                         | otherwise = ((x,y):(posicoesLaser ((x+1),y) B mp))
posicoesLaser (x,y) E mp |(((encontraPosicaoMatriz (x,y) mp) == (Bloco Indestrutivel)) || ((encontraPosicaoMatriz ((x+1),y) mp) == (Bloco Indestrutivel))) = []
                         | otherwise = ((x,y):(posicoesLaser (x,(y-1)) E mp))

-- | Desenha os 'Joagadore's

drawplayers :: EstadoMain -> Int -> [Picture]
drawplayers ((Estado _ [] _),_,_,_,_) _ = []
drawplayers ((Estado mp ((Jogador (x,y) d v l c):xs) ds),n,ab,ac,pics) a | (v /= 0) = (Translate (fromIntegral (y*30)+15) (fromIntegral (x*(-30))+(-15)) (escolhejogador a d pics)): drawplayers ((Estado mp xs ds),n,ab,ac,pics) (a+1)
                                                                         | otherwise = drawplayers ((Estado mp xs ds),n,ab,ac,pics) (a+1)

-- | Altera o 'EstadoMain' de acordo com a tecla pressionada e o o tipo de estado atual

eventChange :: Event -> EstadoMain -> EstadoMain
eventChange (EventKey a Down _ _) (s,n,c,b,pics) = case a of (SpecialKey KeyUp) |(n == 1.1) -> ((jogada 1 (Movimenta C) s),n,c,b,pics)
                                                             (SpecialKey KeyDown) |(n == 1.1) -> ((jogada 1 (Movimenta B) s),n,c,b,pics)
                                                             (SpecialKey KeyLeft) |(n == 1.1) -> ((jogada 1 (Movimenta E) s),n,c,b,pics)
                                                                                  | (n < 1) -> (s,(0.1),b,c,pics)
                                                                                  | (n == 2.1) || (n == 2.2) -> (s,(2.1),b,c,pics)
                                                                                  | (n == 2.3) || (n == 2.4) -> (s,(2.3),b,c,pics)
                                                             (SpecialKey KeyRight) |(n == 1.1) -> ((jogada 1 (Movimenta D) s),n,c,b,pics)
                                                                                   | (n < 1) -> (s,(0.2),b,c,pics)
                                                                                   | (n == 2.1) || (n == 2.2) -> (s,(2.2),b,c,pics)
                                                                                   | (n == 2.3) || (n == 2.4) -> (s,(2.4),b,c,pics)
                                                             (Char '3') -> ((jogada 0 (Dispara Choque) s),n,c,b,pics)
                                                             (Char '2') -> ((jogada 0 (Dispara Laser) s),n,c,b,pics)
                                                             (Char '1') -> ((jogada 0 (Dispara Canhao) s),n,c,b,pics)
                                                             (Char '7') |(n == 1.1) -> ((jogada 3 (Dispara Canhao) s),n,c,b,pics)
                                                             (Char '4') |(n == 1.1) -> ((jogada 2 (Dispara Canhao) s),n,c,b,pics)
                                                             (Char '.') |(n == 1.1) -> ((jogada 1 (Dispara Laser) s),n,c,b,pics)
                                                             (Char ',') |(n == 1.1) -> ((jogada 1 (Dispara Canhao) s),n,c,b,pics)
                                                             (Char '5') |(n == 1.1) -> ((jogada 2 (Dispara Laser)s),n,c,b,pics)
                                                             (Char '8') |(n == 1.1) -> ((jogada 3 (Dispara Laser)s),n,c,b,pics)
                                                             (Char '-') |(n == 1.1) -> ((jogada 1 (Dispara Choque) s),n,c,b,pics)
                                                             (Char '6') |(n == 1.1) -> ((jogada 2 (Dispara Choque) s),n,c,b,pics)
                                                             (Char '9') |(n == 1.1) -> ((jogada 3 (Dispara Choque) s),n,c,b,pics)
                                                             (Char 'w') -> ((jogada 0 (Movimenta C) s),n,c,b,pics)
                                                             (Char 's') -> ((jogada 0 (Movimenta B) s),n,c,b,pics)
                                                             (Char 'a') -> ((jogada 0 (Movimenta E) s),n,c,b,pics)
                                                             (Char 'd') -> ((jogada 0 (Movimenta D) s),n,c,b,pics)
                                                             (Char 't') |(n == 1.1) -> ((jogada 2 (Movimenta C) s),n,c,b,pics)
                                                             (Char 'g') |(n == 1.1) -> ((jogada 2 (Movimenta B) s),n,c,b,pics)
                                                             (Char 'f') |(n == 1.1) -> ((jogada 2 (Movimenta E) s),n,c,b,pics)
                                                             (Char 'h') |(n == 1.1) -> ((jogada 2 (Movimenta D) s),n,c,b,pics)
                                                             (Char 'i') |(n == 1.1) -> ((jogada 3 (Movimenta C) s),n,c,b,pics)
                                                             (Char 'k') |(n == 1.1) -> ((jogada 3 (Movimenta B) s),n,c,b,pics)
                                                             (Char 'j') |(n == 1.1) -> ((jogada 3 (Movimenta E) s),n,c,b,pics)
                                                             (Char 'l') |(n == 1.1) -> ((jogada 3 (Movimenta D) s),n,c,b,pics)
                                                             (Char 'p') |(n == 1.1) -> (s,(2.1),c,b,pics)
                                                                        |(n == 1.2) -> (s,(2.3),c,b,pics)
                                                             (SpecialKey KeySpace) | (b == 1) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bi,bi,bi,bi,bd,bd,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bd,bd,bi,bi,bi,bi,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (7,7) E 5 5 5),(Jogador (7,11) C 5 5 5),(Jogador (11,7) B 5 5 5),(Jogador (11,11) D 5 5 5)] []),n,0,b,pics)
                                                                                   | (b == 2) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,v,v,v,bd,v,v,bi,v,v,bi,v,v,bd,v,v,v,v,bi],[bi,v,v,v,bd,v,v,v,bi,v,v,bi,v,v,v,bd,v,v,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,bd,v,bd,v,v,v,bd,v,v,bd,v,v,v,bd,v,bd,v,bi],[bi,bd,v,v,v,bd,v,bd,bi,v,v,bi,bd,v,bd,v,v,v,bd,bi],[bi,v,v,v,v,v,bd,v,bi,v,v,bi,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,bd,v,v,v,v,bd,v,bd,v,v,v,v,bi],[bi,bi,bi,v,bd,bi,bi,v,bd,v,v,bd,v,bi,bi,bd,v,bi,bi,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,bi,bi,v,bd,bi,bi,v,bd,v,v,bd,v,bi,bi,bd,v,bi,bi,bi],[bi,v,v,v,v,bd,v,bd,v,v,v,v,bd,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bi,v,v,bi,v,bd,v,v,v,v,v,bi],[bi,bd,v,v,v,bd,v,bd,bi,v,v,bi,bd,v,bd,v,v,v,bd,bi],[bi,v,bd,v,bd,v,v,v,bd,v,v,bd,v,v,v,bd,v,bd,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,v,v,bd,v,v,v,bi,v,v,bi,v,v,v,bd,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bi,v,v,bi,v,v,bd,v,v,v,v,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (1,1) D 5 5 5),(Jogador (1,17) B 5 5 5),(Jogador (17,1) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,b,pics) 
                                                                                   | (b == 3) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bi,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,bd,v,bd,bd,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bd,bd,bd,bd,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,bd,bi,bd,bi,bd,bi,bd,bi,v,v,bd,v,v,v,bd,v,v,v,bi],[bi,v,v,v,bd,v,v,v,bd,v,v,bi,bd,bi,bd,bi,bd,bi,bd,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bd,bd,bd,bd,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,bd,bd,v,bd,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,v,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (1,1) D 5 5 5),(Jogador (1,17) B 5 5 5),(Jogador (17,1) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,b,pics)
                                                                                   | (b == 4) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,bi,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,bi,v,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,v,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,bd,bd,bd,bi,bi,v,bi,v,v,v,v,bi,v,bi,bi,bd,bd,bd,bi],[bi,v,v,v,v,v,bi,bi,v,v,v,v,bi,bi,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,bd,bd,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,bd,bd,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bi,bi,v,v,v,v,bi,bi,v,v,v,v,v,bi],[bi,bd,bd,bd,bi,bi,v,bi,v,v,v,v,bi,v,bi,bi,bd,bd,bd,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,v,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (4,4) C 5 5 5),(Jogador (4,14) C 5 5 5),(Jogador (14,4) B 5 5 5),(Jogador (14,14) B 5 5 5)] []),n,0,b,pics)
                                                                                   | (b == 5) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,bi,bi,bd,bd,bi,bi,bi,v,v,v,v,v,v,bi,bi,bi,bd,bd,bi,bi,bi],[bi,bd,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,bi,bi,v,v,v,v,v,bd,bd,v,v,v,v,v,bi,bi,v,v,bi],[bi,v,v,bi,bi,v,v,v,v,v,bd,bd,v,v,v,v,v,bi,bi,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bd,bi],[bi,bi,bi,bd,bd,bi,bi,bi,v,v,v,v,v,v,bi,bi,bi,bd,bd,bi,bi,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (3,3) D 5 5 5),(Jogador (3,17) B 5 5 5),(Jogador (17,3) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,b,pics)
                                                                                   where bi=Bloco Indestrutivel   
                                                                                         bd=Bloco Destrutivel
                                                                                         v=Vazia
                                                             (Char 'm') | (b == 1) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,v,v,v,bd,v,v,bi,v,v,bi,v,v,bd,v,v,v,v,bi],[bi,v,v,v,bd,v,v,v,bi,v,v,bi,v,v,v,bd,v,v,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,bd,v,bd,v,v,v,bd,v,v,bd,v,v,v,bd,v,bd,v,bi],[bi,bd,v,v,v,bd,v,bd,bi,v,v,bi,bd,v,bd,v,v,v,bd,bi],[bi,v,v,v,v,v,bd,v,bi,v,v,bi,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,bd,v,v,v,v,bd,v,bd,v,v,v,v,bi],[bi,bi,bi,v,bd,bi,bi,v,bd,v,v,bd,v,bi,bi,bd,v,bi,bi,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,bi,bi,v,bd,bi,bi,v,bd,v,v,bd,v,bi,bi,bd,v,bi,bi,bi],[bi,v,v,v,v,bd,v,bd,v,v,v,v,bd,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bi,v,v,bi,v,bd,v,v,v,v,v,bi],[bi,bd,v,v,v,bd,v,bd,bi,v,v,bi,bd,v,bd,v,v,v,bd,bi],[bi,v,bd,v,bd,v,v,v,bd,v,v,bd,v,v,v,bd,v,bd,v,bi],[bi,v,v,bd,v,v,v,v,v,bd,bd,v,v,v,v,v,bd,v,v,bi],[bi,v,v,v,bd,v,v,v,bi,v,v,bi,v,v,v,bd,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bi,v,v,bi,v,v,bd,v,v,v,v,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (1,1) D 5 5 5),(Jogador (1,17) B 5 5 5),(Jogador (17,1) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,(b+1),pics)
                                                                        | (b == 2) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bi,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,bd,v,bd,bd,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bd,bd,bd,bd,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,bd,bi,bd,bi,bd,bi,bd,bi,v,v,bd,v,v,v,bd,v,v,v,bi],[bi,v,v,v,bd,v,v,v,bd,v,v,bi,bd,bi,bd,bi,bd,bi,bd,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,bd,bd,bd,bd,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,bd,bd,v,bd,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bi,v,v,v,v,v,v,v,v,v,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (1,1) D 5 5 5),(Jogador (1,17) B 5 5 5),(Jogador (17,1) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,(b+1),pics)
                                                                        | (b == 3) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,v,bi,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,bi,v,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,v,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,bd,bd,bd,bi,bi,v,bi,v,v,v,v,bi,v,bi,bi,bd,bd,bd,bi],[bi,v,v,v,v,v,bi,bi,v,v,v,v,bi,bi,v,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,bd,bd,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,v,v,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,bd,v,v,bd,bd,bd,bd,v,v,bd,v,v,v,v,bi],[bi,v,v,v,v,v,bi,bi,v,v,v,v,bi,bi,v,v,v,v,v,bi],[bi,bd,bd,bd,bi,bi,v,bi,v,v,v,v,bi,v,bi,bi,bd,bd,bd,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,v,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,bi,v,v,v,v,bd,v,v,v,v,v,v,bd,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (4,4) C 5 5 5),(Jogador (4,14) C 5 5 5),(Jogador (14,4) B 5 5 5),(Jogador (14,14) B 5 5 5)] []),n,0,(b+1),pics)
                                                                        | (b == 4) -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,bi,bi,bd,bd,bi,bi,bi,v,v,v,v,v,v,bi,bi,bi,bd,bd,bi,bi,bi],[bi,bd,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bi],[bi,v,v,bi,bi,v,v,v,v,v,bd,bd,v,v,v,v,v,bi,bi,v,v,bi],[bi,v,v,bi,bi,v,v,v,v,v,bd,bd,v,v,v,v,v,bi,bi,v,v,bi],[bi,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,v,bd,bi],[bi,bi,bi,bd,bd,bi,bi,bi,v,v,v,v,v,v,bi,bi,bi,bd,bd,bi,bi,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bd,v,v,bi,bi,v,v,bd,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi,v,v,v,v,v,v,bi],[bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi,bd,v,v,v,v,bd,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (3,3) D 5 5 5),(Jogador (3,17) B 5 5 5),(Jogador (17,3) C 5 5 5),(Jogador (17,17) E 5 5 5)] []),n,0,(b+1),pics)
                                                                        | otherwise -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bi,bi,bi,bi,bd,bd,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bd,bd,bi,bi,bi,bi,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (7,7) E 5 5 5),(Jogador (7,11) B 5 5 5),(Jogador (11,7) C 5 5 5),(Jogador (11,11) D 5 5 5)] []),n,0,1,pics) 
                                                                        where bi=Bloco Indestrutivel   
                                                                              bd=Bloco Destrutivel
                                                                              v=Vazia
                                                             (SpecialKey KeyEnter) | n == 0.1 -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bi,bi,bi,bi,bd,bd,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bd,bd,bi,bi,bi,bi,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (7,7) E 5 5 5),(Jogador (7,11) C 5 5 5),(Jogador (11,7) B 5 5 5),(Jogador (11,11) D 5 5 5)] []),(1.1),b,(c+1),pics)
                                                                                   | n == 0.2 -> ((Estado [[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bi,bi,bi,bi,bd,bd,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,v,v,bi,bi,v,v,bi,v,v,v,v,v,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,bi,bd,bd,bd,bd,bi,bi,bi,v,v,bi,bi,bi,bd,bd,bd,bd,bi,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bi,v,v,bi,bi,v,v,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,bd,bd,bd,bi,bi,bi,bi,bd,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,v,v,v,v,v,v,v,v,bd,bd,v,v,v,v,v,v,v,v,bi],[bi,bi,v,v,v,v,v,v,v,bi,bi,v,v,v,v,v,v,v,bi,bi],[bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi,bi]] [(Jogador (7,7) E 5 5 5),(Jogador (7,11) C 5 5 5),(Jogador (11,7) B 5 5 5),(Jogador (11,11) D 5 5 5)] []),(1.2),b,(c+1),pics)
                                                                                   | (n == 2.2) || (n == 2.4) -> (s,0,c,b,pics)
                                                                                   | n == 2.1 -> (s,1.1,c,b,pics)
                                                                                   | n == 2.3 -> (s,1.2,c,b,pics)
                                                                                   where bi=Bloco Indestrutivel   
                                                                                         bd=Bloco Destrutivel
                                                                                         v=Vazia
                                                             _ | (n >= 3) -> (s,0,c,b,pics)
                                                               | otherwise -> (s,n,c,b,pics)
eventChange _ s = s

-- | Aplica o bot da Tarefa6 ao jogador de indice 1

bot1 :: Estado -> Estado
bot1 s = (jogada 1 (fromJust(bot 1 s)) s)

-- | Aplica o bot da Tarefa6 ao jogador de indice 2

bot2 :: Estado -> Estado
bot2 s = (jogada 2 (fromJust(bot 2 s)) s)

-- | Aplica o bot da Tarefa6 ao jogador de indice 3

bot3 :: Estado -> Estado
bot3 s = (jogada 3 (fromJust(bot 3 s)) s)

-- | Acaba com um jogo (se chegar a um certo tempo ou se só ouver um jogador ou menos no estado)

endgame :: EstadoMain -> EstadoMain
endgame ((Estado mp jd dp),a,d,e,g) | (d == 240) || (onlyoneplayer jd 0) = ((Estado mp jd dp),(winner jd (0,0,0)),d,e,g)
                             | otherwise = ((Estado mp jd dp),a,d,e,g)
                          
-- | altera o 'EstadoMain' para a apresentar o vencedor do jogo

winner :: [Jogador] -> (Int,Int,Int) -> Float
winner (x1:x2:xs) (a,b,c) | (vidasJogador x1 > vidasJogador x2) = winner (x1:xs) (a,b+1,c)
                          | (vidasJogador x1 == vidasJogador x2) = winner (x1:xs) (a+1,b+1,c+1)
                          | (vidasJogador x1 < vidasJogador x2) = winner (x2:xs) (b+1,b+1,0)
winner [x] (a,b,c) | (c /= 0) = 3.4
                   | (a == 0) = 3.0
                   | (a == 1) = 3.1
                   | (a == 2) = 3.2
                   | (a == 3) = 3.3

-- | Verifica se só há um ou zero jogadores no 'Estado'

onlyoneplayer :: [Jogador] -> Int -> Bool
onlyoneplayer (x:xs) a | (vidasJogador x == 0) = onlyoneplayer xs a
                       | otherwise = onlyoneplayer xs (a+1)
onlyoneplayer [] a | a <= 1 = True
                   | otherwise = False

-- | Alera o 'Estado' a cada frame

timeChange :: Float -> EstadoMain -> EstadoMain
timeChange f (s,(1.1),d,e,g) = (endgame ((tick s),(1.1),(d+1),e,g))
timeChange f (s,(1.2),d,e,g) = (endgame (tick(bot3(bot2(bot1(s)))),(1.2),(d+1),e,g))
timeChange f s = s

-- | Cria o jogo

main :: IO()
main = do loadedIMG <- loadIMG
          play disM
               black
               4
               (estadoInicial loadedIMG)
               desenhaEstado
               eventChange
               timeChange

