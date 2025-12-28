module Interface.Cursor where
import System.Console.ANSI
import System.IO

--------------------------------------------------------------------------------
-- | Move o cursor no tabuleiro de acordo com os comandos das setas e WASD.
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
    
    -- Lógica da LINHA (Cima/Baixo)
    let novaLinha
            -- Cima (Seta ACIMA ou W)
            | comando == "\ESC[A" || comando == "w" || comando == "W" = 
                if linha == limiteSuperior then limiteInferior else linha - 2
            -- Baixo (Seta ABAIXO ou S)
            | comando == "\ESC[B" || comando == "s" || comando == "S" = 
                if linha == limiteInferior then limiteSuperior else linha + 2
            | otherwise = linha

    -- Lógica da COLUNA (Esquerda/Direita)
    let novaColuna
            -- Direita (Seta DIREITA ou D)
            | comando == "\ESC[C" || comando == "d" || comando == "D" = 
                if coluna == limiteDir then limiteEsq else coluna + 4
            -- Esquerda (Seta ESQUERDA ou A)
            | comando == "\ESC[D" || comando == "a" || comando == "A" = 
                if coluna == limiteEsq then limiteDir else coluna - 4
            | otherwise = coluna

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

--------------------------------------------------------------------------------
-- | Desenha uma Bandeira na posição atual
--
-- Comportamento:
--   - Desenha o caractere 'P' (de "plantar bandeira") em amarelo.
--   - Mantém o cursor em cima.
--------------------------------------------------------------------------------
desenhaBandeira :: (Int, Int) -> IO ()
desenhaBandeira (linha, coluna) = do
    setSGR [SetColor Foreground Vivid Yellow] 
    setCursorPosition linha (coluna-2)
    putStr "| P |"
    setSGR [Reset]
    setSGR [SetColor Foreground Vivid Red]
    setCursorPosition (linha-1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"
    setSGR [Reset]
    hFlush stdout

--------------------------------------------------------------------------------
-- | Simula a abertura de uma casa (Visualização da Ação)
--
-- Comportamento:
--   - Desenha um símbolo (ex: '0') para indicar que foi aberto.
--   - (implemntar ainda o número de bombas vizinhas)
--------------------------------------------------------------------------------
desenhaAbrir :: (Int, Int) -> IO ()
desenhaAbrir (linha, coluna) = do
    setCursorPosition linha (coluna-2)
    putStr "| 0 |"
    setSGR [SetColor Foreground Vivid Red]
    setCursorPosition (linha-1) (coluna-2)
    putStr "+---+"
    setCursorPosition (linha+1) (coluna-2)
    putStr "+---+"
    setSGR [Reset]
    hFlush stdout
