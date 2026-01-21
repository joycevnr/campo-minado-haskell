-- | Módulo responsável pelo controle visual do cursor no tabuleiro.
--
-- Este módulo cuida exclusivamente da interface ASCII:
-- movimentação do cursor, marcação de células, limpeza visual
-- e desenho de elementos como bandeiras, bombas e números.
--
-- Ele não contém lógica de jogo, apenas representação visual.
module Interface.Cursor where

import Control.Monad (when)
import System.Console.ANSI
import System.IO

--------------------------------------------------------------------------------
-- | Utilitário: clamp para coordenadas visuais
--------------------------------------------------------------------------------

-- | Garante que um valor permaneça dentro de um intervalo fechado.
--
-- É utilizado para impedir que o cursor ultrapasse
-- os limites visuais do tabuleiro.
--
-- Parâmetros:
--   * @minV@ – valor mínimo permitido
--   * @maxV@ – valor máximo permitido
--   * @x@    – valor a ser ajustado
--
-- Retorno:
--   Valor ajustado para o intervalo [minV, maxV].
clamp :: Int -> Int -> Int -> Int
clamp minV maxV x
    | x < minV = minV
    | x > maxV = maxV
    | otherwise = x

--------------------------------------------------------------------------------
-- | Movimento do cursor no tabuleiro (visual)
--------------------------------------------------------------------------------

-- | Move o cursor de acordo com o comando pressionado.
--
-- Parâmetros:
--   * @(linha, coluna)@ – posição atual do cursor na tela
--   * @((limSup, limEsq),(limInf, limDir))@ – limites visuais do tabuleiro
--   * @comando@ – tecla pressionada (setas direcionais ou WASD)
--
-- Comportamento:
--   - Limpa a célula atual antes do movimento
--   - Calcula a nova posição considerando o layout ASCII
--   - Aplica limites para evitar posições inválidas
--   - Marca visualmente a nova posição
--
-- Retorno:
--   Nova posição do cursor @(Int, Int)@.
moveCursor :: (Int,Int)
           -> ((Int,Int),(Int,Int))
           -> String
           -> IO (Int,Int)
moveCursor (linha, coluna)
           ((limSup, limEsq),(limInf, limDir))
           comando = do

    -- Limpa a célula atual antes de mover o cursor
    limpaPosicao (linha, coluna)

    --------------------------------------------------------------------------
    -- Cálculo da nova LINHA
    --------------------------------------------------------------------------

    -- Cada célula ocupa duas linhas no ASCII,
    -- portanto o cursor se move de 2 em 2.
    let linhaBruta
            | comando `elem` ["\ESC[A","w","W"] =
                if linha == limSup then limInf else linha - 2
            | comando `elem` ["\ESC[B","s","S"] =
                if linha == limInf then limSup else linha + 2
            | otherwise = linha

    -- Garante que a linha esteja dentro dos limites visuais
    let novaLinha = clamp limSup limInf linhaBruta

    --------------------------------------------------------------------------
    -- Cálculo da nova COLUNA
    --------------------------------------------------------------------------

    -- Cada célula ocupa 4 colunas no desenho ASCII.
    let colunaBruta
            | comando `elem` ["\ESC[C","d","D"] =
                if coluna == limDir then limEsq else coluna + 4
            | comando `elem` ["\ESC[D","a","A"] =
                if coluna == limEsq then limDir else coluna - 4
            | otherwise = coluna

    -- Garante que a coluna esteja dentro dos limites visuais
    let novaColuna = clamp limEsq limDir colunaBruta

    -- Marca visualmente a nova posição
    marcaPosicao (novaLinha, novaColuna)

    return (novaLinha, novaColuna)

--------------------------------------------------------------------------------
-- | Limpeza de célula
--------------------------------------------------------------------------------

-- | Remove o destaque visual da célula atual.
--
-- Redesenha o contorno padrão da célula no tabuleiro ASCII,
-- sendo utilizada antes do cursor se mover.
limpaPosicao :: (Int,Int) -> IO ()
limpaPosicao (linha, coluna) = do
    -- Proteção para evitar acesso fora da tela
    when (linha > 0) $ do
        setCursorPosition (linha-1) (coluna-2)
        putStr "+---+"

    setCursorPosition linha (coluna-2)
    putStr "|   |"

    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"

    hFlush stdout

--------------------------------------------------------------------------------
-- | Marcação de célula
--------------------------------------------------------------------------------

-- | Destaca visualmente a célula atual do cursor.
--
-- Utiliza a cor vermelha para indicar a posição selecionada.
marcaPosicao :: (Int,Int) -> IO ()
marcaPosicao (linha, coluna) = do
    setSGR [SetColor Foreground Vivid Red]

    when (linha > 0) $ do
        setCursorPosition (linha-1) (coluna-2)
        putStr "+---+"

    setCursorPosition linha (coluna-2)
    putStr "|   |"

    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"

    setSGR [Reset]
    hFlush stdout

--------------------------------------------------------------------------------
-- | Desenho de bandeira
--------------------------------------------------------------------------------

-- | Desenha uma bandeira na célula atual. 
--
-- Indica que o jogador marcou a célula
-- como possível posição de bomba.
desenhaBandeira :: (Int, Int) -> IO ()
desenhaBandeira (linha, coluna) = do
    setSGR [SetColor Foreground Vivid Yellow]
    setCursorPosition linha (coluna-2)
    putStr "| P |"
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Red]
    when (linha > 0) $ do
        setCursorPosition (linha-1) (coluna-2)
        putStr "+---+"
    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"
    setSGR [Reset]

    hFlush stdout

--------------------------------------------------------------------------------
-- | Desenho de bomba
--------------------------------------------------------------------------------

-- | Desenha visualmente uma bomba na célula.
--
-- Utiliza um emoji para facilitar a identificação.
desenhaBomba :: (Int, Int) -> IO ()
desenhaBomba (l,c) = do
    setCursorPosition l (c-2)
    putStr "| 💣 |"
    hFlush stdout

--------------------------------------------------------------------------------
-- | Desenho do número de bombas vizinhas
--------------------------------------------------------------------------------

-- | Exibe o número de bombas adjacentes a uma célula.
--
-- Utilizado após a célula ser revelada.
desenhaNumero :: (Int, Int) -> Int -> IO ()
desenhaNumero (l,c) n = do
    setCursorPosition l (c-2)
    putStr ("| " ++ show n ++ " |")
    hFlush stdout
