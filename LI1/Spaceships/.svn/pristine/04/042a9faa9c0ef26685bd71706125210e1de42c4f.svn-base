-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g077 where
import Tarefa0_2018li1g077
import LI11819

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[(Move C),(Move E),(Move B),(Move D)],[Roda,Roda,Roda,Roda],[MudaParede,MudaParede],[Roda,Roda,Roda,Desenha],[MudaTetromino,Roda,Roda,Desenha],[MudaTetromino,MudaTetromino,Roda,Desenha],[MudaTetromino,MudaTetromino,MudaTetromino,Roda,Roda,Desenha],[MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Roda,Roda,Desenha],[MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Roda,Desenha],[MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Desenha]]




-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
--
-- __NB:__ Deve assumir que o cursor e tetrominós cabem __sempre__ dentro das bordas do 'Mapa', e que o mapa __nunca__ necessita de ser redimensionado.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao MudaTetromino (Editor (px,py) d t p m) = (Editor (px,py) d (if t == Z then I else succ t) p m)
instrucao MudaParede (Editor (px,py) d t p m) = (Editor (px,py) d t (if p == Indestrutivel then Destrutivel else Indestrutivel) m)
instrucao (Move di) (Editor (px,py) d t p m) = (Editor (px+a,py+b) d t p m)   
    where (a,b) = direcaoParaVetor di
instrucao Roda (Editor (px,py) d t p m) = (Editor (px,py) (if d == E then C else succ d) t p m)

instrucao Desenha (Editor (px,py) d t p m) = (Editor (px,py) d t p (atualizaIndiceMatriz 0 (px,py) (direcaoTetromino d t) m p))

-- ** Funções auxiliares para a 'Instrucao' 'Desenha'
--
-- | Verifica se a Matriz é válida antes de a rodar (Usa a 'tamanhoListasM')
rodaMatrizInvT :: Matriz a -> Matriz a
rodaMatrizInvT x = if tamanhoListasM x then rodaMatrizInv x else []

-- | Roda a Matriz 90º no sentido contrário dos ponteiros do relógio
rodaMatrizInv [x] = reverse (map (:[]) x)
rodaMatrizInv (x:xs) = (zipWith (:) (reverse x) (rodaMatrizInv xs)) 
rodaMatrizInv _ = []

-- | Transforma um 'Tetromino' em 
direcaoTetromino :: Direcao -> Tetromino -> Matriz Bool
direcaoTetromino C tetro = tetrominoParaMatriz tetro
direcaoTetromino D tetro = rodaMatriz (tetrominoParaMatriz tetro)
direcaoTetromino B tetro = rodaMatriz (rodaMatriz (tetrominoParaMatriz tetro))
direcaoTetromino E tetro = rodaMatrizInvT (tetrominoParaMatriz tetro)

-- | Altera um elemento de uma linha de um Mapa
mudaListaTtr :: Int -- ^ Indice do elemento a mudar
          -> [Bool] -- ^ Linha de um tetrómino na forma em Matriz de Bool
          -> [Peca] -- ^ Linha do mapa a ser alterada
           -> Peca  -- ^ Peca que vai ser inserida no mapa
          -> [Peca] -- ^ Linha resultante da alteração de um elemento
mudaListaTtr py [] linha blc = linha
mudaListaTtr py (t:ts) (m:ms) blc = mudaListaTtr (py + 1) ts (if t == True then (atualizaIndiceLista py (blc) (m:ms)) else (m:ms)) blc

-- | Insere um 'Tetromino' num 'Mapa'
--
atualizaIndiceMatriz ::  Int -- ^ Acumulador
                     -> Posicao -- ^ Posicão do Editor
                     -> Matriz Bool -- ^ Matriz Booleana correspondente ao 'Tetromino' a inserir
                     -> Mapa -- ^ Mapa onde vai ser inserido o 'Tetromino'
                     -> Parede -- ^ Tipo de Parede
                     -> Mapa -- ^ 'Mapa' resultante a após aplicar a 'Instrucao' 'Desenha'
atualizaIndiceMatriz _ _ [] m _ = m
atualizaIndiceMatriz n (l,c) (t:ts) (m:ms) prd = if n >= l 
 then mudaListaTtr c t m (Bloco prd) : (atualizaIndiceMatriz (n+1) (l,c) ts ms (prd))
 else m : atualizaIndiceMatriz (n+1) (l,c) (t:ts) ms (prd)
                                                                 
                                                 
-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] edtr = edtr
instrucoes (x:xs) edtr = instrucoes xs (instrucao x edtr)

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) | (x<=0) || (y<=0) = []
                  | x==1 = [(criaListas y (Bloco Indestrutivel))]
                  | otherwise = ((criaListas y (Bloco Indestrutivel)) :(criaListas (x-2) (listasIniciais y))) ++ [(criaListas y (Bloco Indestrutivel))]
    where
    listasIniciais :: Int -> [Peca]
    listasIniciais 0 = []
    listasIniciais n | (n==y || n==1) = (Bloco Indestrutivel) : listasIniciais (n-1)
                     | (n < 0) = []
                     | otherwise = Vazia : listasIniciais (n-1)

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial is = (Editor (posicaoInicial is) C I Indestrutivel (mapaInicial (dimensaoInicial is)))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = mapaEditor (instrucoes is (editorInicial is))
