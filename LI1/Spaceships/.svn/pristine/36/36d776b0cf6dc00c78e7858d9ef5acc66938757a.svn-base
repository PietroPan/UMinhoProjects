-- | Teste
-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g077 where

import LI11819
-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (l1,c1) (l2,c2) = (l1 + l2, c1 + c2)


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (l1,c1) (l2,c2) = (l1-l2,c1-c2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor a (l,c) = (l * a,c * a)
-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (l,c) = (c,(-l))

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (l,c) = (l,(-c))
-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (l,c) = ((-l), c)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = ((-1),0)
direcaoParaVetor B = (1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor E = (0,(-1))

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido ind (x:xs) = if ind == 0 then True else eIndiceListaValido (ind - 1) xs 
-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz (x:xs) = if tamanhoListasM (x:xs) == True then ((tamanhoLista (x:xs), tamanhoLista x)) else (0,0)

tamanhoListasM :: Matriz a -> Bool
tamanhoListasM [] = False
tamanhoListasM [[]] = False
tamanhoListasM [a] = True
tamanhoListasM (x:y:xs) = (tamanhoLista x == tamanhoLista y && tamanhoLista x /= 0) && tamanhoListasM (y : xs)

tamanhoLista :: [a] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (l,c) [] = False
ePosicaoMatrizValida (l,c) (x:xs) = (l >= 0) && (c >= 0) && l < (tamanhoLista (x:xs)) && c < (tamanhoLista x)

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (l,c) (x:xs) = (ePosicaoMatrizValida (l,c) (x:xs) && (l == 0 || l == ((tamanhoLista (x:xs))-1) || c == 0 || c == (tamanhoLista x)-1))

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = [[False,True,False,False], [False,True,False,False], [False,True,False,False],[False,True,False,False]]
tetrominoParaMatriz J = [[False,True,False], [False,True,False], [True,True,False]]
tetrominoParaMatriz L = [[False,True,False], [False,True,False], [False,True,True]]
tetrominoParaMatriz O = [[True,True], [True,True]]
tetrominoParaMatriz S = [[False,True,True], [True,True,False], [False,False,False]]
tetrominoParaMatriz T = [[False,False,False], [True,True,True], [False,True,False]]
tetrominoParaMatriz Z = [[True,True,False], [False,True,True], [False,False,False]]
-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista a [] = error "Não existe"
encontraIndiceLista 0 (x:xs) = x
encontraIndiceLista a (x:xs) = encontraIndiceLista (a-1) xs

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista n a (x:xs) = (if n == 0 then a else x) : atualizaIndiceLista (n-1) a xs

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--

-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>

rodaMatriz :: Matriz a -> Matriz a
rodaMatriz [] = []
rodaMatriz (x:xs) = if tamanhoListasM (x:xs)  then (rodaMatrizAux (x:xs))  else []

rodaMatrizAux :: Matriz a -> Matriz a
rodaMatrizAux [] = []
rodaMatrizAux (x:xs) = procuraCabecas (x:xs) : rodaMatrizAux (removeCabecas [] (x:xs))

procuraCabecas :: Matriz a -> [a]
procuraCabecas []=[]
procuraCabecas (m:ms) = procuraCabecas ms ++ [head m]

removeCabecas :: [a] -> Matriz a -> Matriz a
removeCabecas l [] = []
removeCabecas l ((x:[]):ys)=[]
removeCabecas l ((x:xs):ys)=(l++xs):(removeCabecas l ys)
-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [] = []
inverteMatrizH (x:xs) = inverteListas [] x : inverteMatrizH xs 
 where inverteListas lista [] = lista
       inverteListas lista (x:xs) = inverteListas (x:lista) xs
       

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV l = inverteListas [] l
 where inverteListas lista [] = lista
       inverteListas lista (x:xs) = inverteListas (x:lista) xs
       
-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (l,c) a = criaListas l (criaListas c a)

criaListas :: Int -> a -> [a]
criaListas 0 a = []
criaListas n a = a : criaListas (n-1) a

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (l,c) a = encontraIndiceLista c (encontraIndiceLista l a) 

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (l,c) elm (x:xs) = (if l == 0 then criaListasMudaElem (l,c) elm x 
                                                    else x) : (atualizaPosicaoMatriz (l - 1,c) elm xs)
 where criaListasMudaElem :: Posicao -> a -> [a] -> [a]
       criaListasMudaElem _ _ [] =  []
       criaListasMudaElem (l,c) elm (x:xs) = (if c == 0 then elm else x) : (criaListasMudaElem (l,c - 1) elm xs)    
