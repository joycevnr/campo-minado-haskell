-- | Módulo responsável pela finalização gráfica do jogo.
--
-- Este módulo cuida da exibição final da partida,
-- incluindo a impressão das mensagens de vitória e derrota,
-- e a listagem do ranking.
module Interface.FinalizaJogo where

import System.Console.ANSI
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Interface.Cronometro
import Interface.Ranking
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------
-- | Exibições para finalização do jogo
--------------------------------------------------------------------------------

-- | Exibe a tela de vitória

telaVitoria :: IO ()
telaVitoria = do
    clearScreen
    setCursorPosition 10 10
    putStrLn "VITÓRIA!"
    setCursorPosition 12 6
    putStrLn "Você identificou todas as bombas!"
    _ <- getChar
    return ()

-- | Exibe a tela de derrota

telaDerrota :: IO ()
telaDerrota = do
    clearScreen
    setCursorPosition 10 10
    putStrLn "GAME OVER"
    setCursorPosition 12 6
    putStrLn "Você pisou em uma bomba!"
    _ <- getChar
    return ()


-- | Finaliza os detalhes de tempo/ranking da partida em caso de vitória.
--
-- Parâmetros:
--   * @tempoIni@ – tempo em que a partida foi iniciada
--   * @tempoFinal@ – tempo em que a partida foi encerrada
--   * @dificuldade@ – dificuldade da partida
--   * @lin@ – linha base do ranking
--   * @col@ – coluna base do ranking
--
-- Comportamento:
--   - Armazena a duração da partida no arquivo de tempos
--   - Exibe o ranking dos tempos e uma mensagem acerca do seu tempo realizado na partida
encerramentoVitoria :: UTCTime -> UTCTime -> Int -> Int -> Int -> IO ()
encerramentoVitoria tempoIni tempoFinal dificuldade lin col = do
    let duracao = diffUTCTime tempoFinal tempoIni
    salvaTempo duracao dificuldade

    clearScreen
    exibeRanking dificuldade lin col
    exibeMensagemRanking duracao dificuldade (lin + 15) (col + 20)
    hFlush stdout
    _ <- getChar
    return ()

-- | Finaliza os detalhes de tempo/ranking da partida em caso de derrota.
--
-- Parâmetros:
--   * @dificuldade@ – dificuldade da partida
--   * @lin@ – linha base do ranking
--   * @col@ – coluna base do ranking
--
-- Comportamento:
--   - Exibe o ranking dos tempos
encerramentoDerrota :: Int -> Int -> Int -> IO ()
encerramentoDerrota dificuldade lin col = do
    clearScreen
    exibeRanking dificuldade lin col
    hFlush stdout
    _ <- getChar
    return ()