-- | Módulo responsável pelo loop principal do jogo.
--
-- Este módulo conecta a lógica do jogo com a interface visual,
-- interpretando comandos do usuário e disparando ações como
-- movimentação do cursor, marcação de bandeiras, abertura de
-- células e encerramento da partida.
module Jogo.Jogo where

import System.Console.ANSI
import System.IO
import Jogo.Logica 
import Jogo.Types
import Data.Time (getCurrentTime)
import Control.Concurrent.Async (cancel)
import Interface.FinalizaJogo
import Interface.Cursor
    ( moveCursor
    , desenhaBandeira
    , desenhaBomba
    , desenhaNumero
    )

--------------------------------------------------------------------------------
-- | Exibição de instruções
--------------------------------------------------------------------------------

-- | Exibe as instruções do jogo na tela.
--
-- Parâmetros:
--   * @linha@ – linha inicial onde as instruções serão desenhadas
--
-- Comportamento:
--   - Mostra os comandos disponíveis ao jogador
--   - Não interfere na lógica do jogo
instrucoes :: Int -> IO ()
instrucoes linha = do
    setCursorPosition linha 0
    putStr "INSTRUÇÕES:"
    setCursorPosition (linha + 1) 0
    putStr "+ Use WASD ou SETAS para se mover"
    setCursorPosition (linha + 2) 0
    putStr "+ ESPAÇO: Bandeira | ENTER: Abrir"
    setCursorPosition (linha + 3) 0
    putStr "+ Pressione q para sair"
    hFlush stdout

--------------------------------------------------------------------------------
-- | Loop principal do jogo
--------------------------------------------------------------------------------

-- | Executa o loop principal do jogo.
--
-- Parâmetros:
--   * @estado@  – estado lógico atual do jogo
--   * @cursorT@ – posição atual do cursor na tela
--   * @limites@ – limites visuais do tabuleiro ASCII:
--       @((linhaInicial, colunaInicial), (linhaFinal, colunaFinal))@
--   * @estadoAuxiliar@ – estado com dados do cronometro e do ranking
--
-- Comportamento:
--   - Lê comandos do teclado
--   - Atualiza apenas a posição visual do cursor
--   - Executa ações visuais e lógicas conforme o comando
--   - Encerra o jogo quando solicitado
--
-- Observação:
--   A lógica do jogo e a interface visual são tratadas de forma
--   separada. O movimento do cursor afeta apenas a tela.
jogo :: EstadoJogo
     -> (Int, Int)
     -> ((Int, Int), (Int, Int))
     -> EstadoAuxiliar
     -> IO ()
jogo estado cursorT limites estadoAuxiliar = do
    comando <- getKey

    --------------------------------------------------------------------------
    -- Movimento do cursor (apenas visual)
    --------------------------------------------------------------------------
    if comando `elem`
        ["w","a","s","d","W","A","S","D","\ESC[A","\ESC[B","\ESC[C","\ESC[D"]
    then do
        novoCursor <- moveCursor cursorT limites comando
        jogo estado novoCursor limites estadoAuxiliar

    --------------------------------------------------------------------------
    -- Plantar bandeira 
    --------------------------------------------------------------------------
    else if comando == " " then do
        let posLogica =
                cursorParaPosicao
                    cursorT
                    (fst limites)
                    (tabuleiro estado)

        let novoEstado = acrescentaBandeira estado posLogica
        desenhaBandeira cursorT
        desenhaBombasFaltando novoEstado (displayLinha novoEstado) (displayColuna novoEstado)

        if status novoEstado == Vitoria
            then do
                cancel (idExecucao estadoAuxiliar)
                tempoFinal <- getCurrentTime 
                telaVitoria
                encerramentoVitoria (tempoInicio estadoAuxiliar) tempoFinal (modo estadoAuxiliar) (rankingLinha estadoAuxiliar) (rankingColuna estadoAuxiliar)

            else
                jogo novoEstado cursorT limites estadoAuxiliar

    --------------------------------------------------------------------------
    -- Abrir célula
    --------------------------------------------------------------------------
    else if comando == "\n" then do
        let posLogica =
                cursorParaPosicao
                    cursorT
                    (fst limites)
                    (tabuleiro estado)

        let novoEstado = atualizaStatusAposAbrir estado cursorT

        case consultaPosicao (tabuleiro estado) posLogica of
            Left "BOMBA" -> do
                desenhaBomba cursorT

                cancel (idExecucao estadoAuxiliar)      -- encerra a execução do cronômetro

                telaDerrota
                encerramentoDerrota (modo estadoAuxiliar) (rankingLinha estadoAuxiliar) (rankingColuna estadoAuxiliar)      -- exibe o ranking

            Right n -> do
                desenhaNumero cursorT n
                let novoEstado = atualizaStatusAposAbrir estado cursorT
                jogo novoEstado cursorT limites estadoAuxiliar

            _ -> jogo estado cursorT limites estadoAuxiliar
        
    --------------------------------------------------------------------------
    -- Encerramento do jogo
    --------------------------------------------------------------------------
    else if comando `elem` ["q","Q"] then do
        cancel (idExecucao estadoAuxiliar)      -- encerra a execução do cronômetro

        setCursorPosition (snd (snd limites) + 4) 0
        putStrLn "Jogo encerrado!"
        showCursor

    --------------------------------------------------------------------------
    -- Qualquer outro comando é ignorado
    --------------------------------------------------------------------------
    else
        jogo estado cursorT limites estadoAuxiliar
