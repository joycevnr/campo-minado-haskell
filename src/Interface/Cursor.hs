module Interface.Cursor where
import System.Console.ANSI
import System.IO
import Jogo.Logica

--------------------------------------------------------------------------------
-- | Move o cursor no tabuleiro de acordo com os comandos das setas.
--
-- Parâmetros:
--   * @(linha, coluna)@ – posição atual do cursor
--   * @((limiteSuperior, limiteEsq), (limiteInferior, limiteDir))@ – limites do tabuleiro
--   * @comando@ – código da tecla pressionada:
--       * "\ESC[A" -> cima
--       * "\ESC[B" -> baixo
--       * "\ESC[C" -> direita
--       * "\ESC[D" -> esquerda
--
-- Comportamento:
--   - Limpa a célula antiga (limpaPosicao)
--   - Calcula a nova posição com wrap-around nos limites
--   - Marca a nova posição em vermelho (marcaPosicao)
--
-- Retorno:
--   Nova posição do cursor @(Int, Int)@
--------------------------------------------------------------------------------
moveCursor :: (Int,Int) -> ((Int,Int),(Int,Int)) -> String -> IO(Int,Int)
moveCursor (linha, coluna) ((limiteSuperior,limiteEsq),(limiteInferior, limiteDir)) comando = do
    limpaPosicao (linha,coluna)
    let novaLinha
            | comando == "\ESC[A" = if linha == limiteSuperior then limiteInferior else linha - 2
            | comando == "\ESC[B" = if linha == limiteInferior then limiteSuperior else linha + 2
            | otherwise           = linha
    let novaColuna
            | comando == "\ESC[C" = if coluna == limiteDir then limiteEsq else coluna + 4
            | comando == "\ESC[D" = if coluna == limiteEsq then limiteDir else coluna - 4
            | otherwise           = coluna
    marcaPosicao (novaLinha, novaColuna)
    return(novaLinha,novaColuna)

--------------------------------------------------------------------------------
-- | Limpa a célula na posição informada, removendo qualquer destaque.
--
-- Parâmetros:
--   * @(linha, coluna)@ – posição da célula a limpar
--
-- Comportamento:
--   - Desenha a célula em seu estado normal ASCII
--   - Usado antes de mover o cursor
--------------------------------------------------------------------------------
limpaPosicao:: (Int,Int) -> IO()
limpaPosicao (linha, coluna) = do
    setCursorPosition (linha-1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha) (coluna-2)
    putStr "|   |"
    hFlush stdout

--------------------------------------------------------------------------------
-- | Destaca a célula na posição informada.
--
-- Parâmetros:
--   * @(linha, coluna)@ – posição da célula a destacar
--
-- Comportamento:
--   - Desenha a célula em vermelho
--   - Usado após mover o cursor para indicar seleção atual
--------------------------------------------------------------------------------
marcaPosicao:: (Int,Int) -> IO()
marcaPosicao (linha, coluna) = do
    setSGR [SetColor Foreground Vivid Red]
    setCursorPosition (linha-1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha) (coluna-2)
    putStr "|   |"
    setSGR [Reset]
    hFlush stdout
