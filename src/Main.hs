module Main (main) where

import System.Console.ANSI
import Interface.PreparaJogo
import System.IO
import Control.Concurrent (threadDelay)

--------------------------------------------------------------------------------
-- | Função recursiva que verifica o tamanho do terminal.
--
-- Comportamento:
-- 1. Se o terminal for menor que 85x40, ela exibe um aviso e espera 1 segundo
-- antes de verificar novamente. Isso impede o jogo de quebrar a renderização
-- e dá tempo para o usuário ajustar a janela.
--
-- Retorno:
--   Retorna uma tupla (Linhas, Colunas) assim que o tamanho for suficiente.
--------------------------------------------------------------------------------
garantirTamanho :: IO (Int, Int)
garantirTamanho = do
    tamanho <- getTerminalSize
    case tamanho of
        Just (linhas, colunas) -> do
            -- Requisito mínimo: 85 colunas x 40 linhas (para caber o modo 'Difícil')
            if colunas < 85 || linhas < 40
                then do
                    clearScreen
                    setCursorPosition 0 0
                    putStrLn "┌──────────────────────────────────────────────────┐"
                    putStrLn "│  TAMANHO DE TELA INSUFICIENTE!                   │"
                    putStrLn "│                                                  │"
                    putStrLn "│  Por favor, maximize o terminal ou arraste       │"
                    putStrLn "│  as bordas para aumentar a janela.               │"
                    putStrLn "│                                                  │"
                    putStrLn ("│  Atual: " ++ show colunas ++ "x" ++ show linhas ++ "  |  Necessário: 85x40               │")
                    putStrLn "└──────────────────────────────────────────────────┘"
                    hFlush stdout
                    threadDelay 1000000 
                    garantirTamanho
                else do
                    return (linhas, colunas)
        Nothing -> do
            putStrLn "Erro fatal: Não foi possível ler o tamanho do terminal."
            return (0,0)

--------------------------------------------------------------------------------
-- | Ponto de entrada  do jogo Campo Minado.
--
-- Fluxo de Execução:
--   1. Configura o terminal (remove cursor, tira echo).
--   2. Entra no loop de verificação de tamanho (garantirTamanho).
--   3. Exibe o menu principal.
--   4. Inicia o jogo na dificuldade escolhida.
--   5. Ao encerrar, limpa a tela e restaura o cursor do sistema.
--------------------------------------------------------------------------------
main :: IO ()
main = do
    setUpTerminal
    (linhas, colunas) <- garantirTamanho
    clearScreen
    menu ((linhas `div` 2),(colunas `div` 2))
    comecarJogo ((linhas `div` 2),(colunas `div` 2))
    clearScreen
    showCursor

