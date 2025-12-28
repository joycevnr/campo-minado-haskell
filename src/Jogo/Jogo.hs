module Jogo.Jogo where
import System.Console.ANSI
import System.IO 
import Interface.Cursor
import Jogo.Logica

--------------------------------------------------------------------------------
-- | Exibe as instruções do jogo na tela.
--
-- Parâmetros:
--   * @linha@ – linha inicial onde as instruções serão desenhadas
--
-- Comportamento:
--   - Mostra "INSTRUÇÕES:" na linha indicada
--   - Mostra as instruções de movimentação e de saída
--------------------------------------------------------------------------------
instrucoes ::  Int -> IO()
instrucoes linha = do
    setCursorPosition linha 0
    putStr "INSTRUÇÕES:"
    setCursorPosition (linha+1) 0
    putStr "+ Use WASD ou SETAS para se mover"
    setCursorPosition (linha+2) 0
    putStr "+ ESPAÇO: Bandeira | ENTER: Abrir"
    setCursorPosition (linha+3) 0
    putStr "+ Pressione q para sair"
    hFlush stdout

--------------------------------------------------------------------------------
-- | Loop principal do jogo, trata movimentos do jogador e atualização do cursor.
--
-- Parâmetros:
--   * @posicaoAtual@ – posição atual do cursor (linha, coluna)
--   * @limites@ – limites do tabuleiro ((linhaSup, colunaEsq), (linhaInf, colunaDir))
--   * @bombas@ – lista de posições das bombas
--
-- Comportamento:
--   - Lê comando do teclado com getKey
--   - Se o comando for uma seta ou WASD), move o cursor usando moveCursor
--   - Se o comando for 's', sai do jogo
--   - Caso contrário, mantém a posição e continua o loop
--------------------------------------------------------------------------------
jogo :: (Int, Int)->((Int,Int),(Int,Int))-> [(Int,Int)] -> IO()
jogo posicaoAtual limites bombas = do
    comando <- getKey
    if (last comando `elem` ['A','B','C','D']) || (comando `elem` ["w","a","s","d","W","A","S","D"])
        then do
            novaPosicao <- moveCursor posicaoAtual limites comando
            jogo novaPosicao limites bombas
    else if comando == " " 
        then do
            desenhaBandeira posicaoAtual
            jogo posicaoAtual limites bombas
    else if comando == "\n"
        then do
            desenhaAbrir posicaoAtual
            jogo posicaoAtual limites bombas
    else if comando == "q" || comando == "Q"
        then do
            setCursorPosition (fst (snd limites) + 4) 0
            putStrLn "Jogo encerrado!"
            showCursor
            return ()
    else
        jogo posicaoAtual limites bombas
