-- | Módulo responsável pela lógica central do jogo.
--
-- Este módulo implementa a lógica do Campo Minado:
-- leitura de entrada do teclado, criação e manipulação
-- do tabuleiro, geração de bombas, cálculo de vizinhança
-- e conversão entre coordenadas visuais e lógicas.
--
-- Nenhuma funcionalidade de interface gráfica é tratada aqui.
module Jogo.Logica where

import System.IO (getChar)
import System.Random (randomRIO)
import Jogo.Types

--------------------------------------------------------------------------------
-- | Leitura de entrada do teclado
--------------------------------------------------------------------------------

-- | Lê a entrada bruta do teclado.
--
-- Trata tanto teclas simples (como ENTER ou espaço)
-- quanto sequências de escape usadas pelas setas do teclado.
--
-- Retorno:
--   String representando o comando pressionado.
getKey :: IO String
getKey = do
    c1 <- getChar
    if c1 == '\ESC' then do
        c2 <- getChar
        c3 <- getChar
        return ('\ESC' : c2 : [c3])
    else return [c1]

--------------------------------------------------------------------------------
-- | Criação do tabuleiro base
--------------------------------------------------------------------------------

-- | Representa uma célula vazia inicial.
--
-- Uma célula vazia não possui bomba, não está revelada,
-- não está marcada e ainda não possui vizinhos calculados.
celulaVazia :: Celula
celulaVazia = Celula False 0 False False

-- | Gera um tabuleiro vazio com as dimensões informadas.
--
-- Todas as células começam com o mesmo estado inicial
-- definido por 'celulaVazia'.
--
-- Parâmetros:
--   * número de linhas
--   * número de colunas
geraTabuleiro :: Int -> Int -> Tabuleiro
geraTabuleiro numLinhas numColunas =
    replicate numLinhas (replicate numColunas celulaVazia)

--------------------------------------------------------------------------------
-- | Validação de posição
--------------------------------------------------------------------------------

-- | Verifica se uma posição está dentro dos limites do tabuleiro.
--
-- Evita acessos inválidos à matriz de células.
posicaoValida :: Tabuleiro -> Posicao -> Bool
posicaoValida tab (l,c) =
    l >= 0 && c >= 0 &&
    l < length tab &&
    c < length (head tab)

--------------------------------------------------------------------------------
-- | Clamp (proteção contra índices inválidos)
--------------------------------------------------------------------------------

-- | Garante que um valor permaneça dentro de um intervalo.
--
-- Muito útil para impedir acesso fora dos limites do tabuleiro.
clamp :: Int -> Int -> Int -> Int
clamp minimo maximo x
    | x < minimo = minimo
    | x > maximo = maximo
    | otherwise  = x

-- | Aplica o clamp a uma posição (linha, coluna).
--
-- Garante que a posição resultante seja válida
-- dentro do tabuleiro.
clampPosicao :: Tabuleiro -> Posicao -> Posicao
clampPosicao tab (l,c) =
    ( clamp 0 (length tab - 1) l
    , clamp 0 (length (head tab) - 1) c
    )

--------------------------------------------------------------------------------
-- | Cálculo de vizinhança
--------------------------------------------------------------------------------

-- | Retorna todas as posições vizinhas válidas de uma célula.
--
-- Considera as 8 direções ao redor da posição informada.
vizinhos :: Tabuleiro -> Posicao -> [Posicao]
vizinhos tab (l,c) =
    filter (posicaoValida tab)
        [ (l+dl, c+dc)
        | dl <- [-1..1]
        , dc <- [-1..1]
        , (dl,dc) /= (0,0)
        ]

-- | Conta quantas bombas existem nas células vizinhas
-- de uma posição específica.
contaBombas :: Tabuleiro -> Posicao -> Int
contaBombas tab pos =
    length
        [ ()
        | p <- vizinhos tab pos
        , let cel = tab !! fst p !! snd p
        , temBomba cel
        ]

--------------------------------------------------------------------------------
-- | Geração das bombas
--------------------------------------------------------------------------------

-- | Gera posições aleatórias para as bombas.
--
-- Garante que não existam posições duplicadas.
--
-- Parâmetros:
--   * número de linhas
--   * número de colunas
--   * quantidade de bombas
geraBombas :: Int -> Int -> Int -> IO [Posicao]
geraBombas numLinhas numColunas qtd = gera []
  where
    gera acc
        | length acc == qtd = return acc
        | otherwise = do
            l <- randomRIO (0, numLinhas - 1)
            c <- randomRIO (0, numColunas - 1)
            let p = (l,c)
            if p `elem` acc
                then gera acc
                else gera (p:acc)

--------------------------------------------------------------------------------
-- | Inserção das bombas no tabuleiro
--------------------------------------------------------------------------------

-- | Insere bombas no tabuleiro de acordo com a lista de posições.
--
-- As células correspondentes são atualizadas para indicar
-- a presença de bomba.
insereBombas :: Tabuleiro -> [Posicao] -> Tabuleiro
insereBombas tab bombas =
    [ [ if (i,j) `elem` bombas
          then cel { temBomba = True }
          else cel
      | (j, cel) <- zip [0..] linha ]
    | (i, linha) <- zip [0..] tab ]

--------------------------------------------------------------------------------
-- | Atualização da vizinhança
--------------------------------------------------------------------------------

-- | Atualiza o número de bombas vizinhas de cada célula.
--
-- Apenas células que não possuem bomba são atualizadas.
atualizaVizinhos :: Tabuleiro -> Tabuleiro
atualizaVizinhos tab =
    [ [ if temBomba cel
          then cel
          else cel { vizinhas = contaBombas tab (i,j) }
      | (j, cel) <- zip [0..] linha ]
    | (i, linha) <- zip [0..] tab ]

--------------------------------------------------------------------------------
-- | Geração completa do tabuleiro
--------------------------------------------------------------------------------

-- | Cria o tabuleiro completo do jogo.
--
-- Etapas:
--   1. Gera a matriz base vazia
--   2. Sorteia as posições das bombas
--   3. Insere as bombas no tabuleiro
--   4. Calcula o número de bombas vizinhas
geraTabuleiroComBombas :: Int -> Int -> Int -> IO Tabuleiro
geraTabuleiroComBombas numLinhas numColunas qtdBombas = do
    let base = geraTabuleiro numLinhas numColunas
    posBombas <- geraBombas numLinhas numColunas qtdBombas
    let comBombas = insereBombas base posBombas
    return (atualizaVizinhos comBombas)

--------------------------------------------------------------------------------
-- | Consulta de posição
--------------------------------------------------------------------------------

-- | Consulta uma posição do tabuleiro.
--
-- Retorna:
--   * @Left "Posição inválida"@ se a posição não existir
--   * @Left "BOMBA"@ se a célula contiver uma bomba
--   * @Right n@ com o número de bombas vizinhas caso contrário
consultaPosicao :: Tabuleiro -> Posicao -> Either String Int
consultaPosicao tab pos
    | not (posicaoValida tab pos) = Left "Posição inválida"
    | temBomba cel                = Left "BOMBA"
    | otherwise                   = Right (vizinhas cel)
  where
    cel = tab !! fst pos !! snd pos

--------------------------------------------------------------------------------
-- | Conversão do cursor para posição lógica
--------------------------------------------------------------------------------

-- | Converte a posição do cursor na tela para uma posição lógica no tabuleiro.
--
-- O cálculo leva em conta o layout ASCII do tabuleiro,
-- onde cada célula ocupa duas linhas e quatro colunas.
cursorParaPosicao :: (Int, Int) -> (Int, Int) -> Tabuleiro -> Posicao
cursorParaPosicao (linhaCur, colunaCur) (inicioL, inicioC) tab =
    clampPosicao tab posBruta
  where
    posBruta =
        ( (linhaCur  - inicioL) `div` 2
        , (colunaCur - inicioC) `div` 4
        )
