-- | Módulo responsável pelo ranking do jogo.
--
-- Este módulo cuida de tudo relacionado ao ranking.
module Interface.Ranking where

import Data.Time
import Control.Exception (evaluate)
import Jogo.Logica
import Interface.Cronometro
import System.Console.ANSI

-- | Imprime o ranking.
--
-- Parâmetros:
--   * @lista@ – o ranking de tempos
--   * @indice@ – posicao do tempo atual no ranking
--   * @lin@ – linha base do ranking
--   * @col@ – coluna base do ranking
--
-- Comportamento:
--   - Desenha de forma recursiva o ranking
--   - Se não tiver nenhum tempo no ranking, um aviso é impresso
desenhaRanking :: [NominalDiffTime] -> Int -> Int -> Int -> IO ()
desenhaRanking [] 1 lin col = do
    setCursorPosition lin col
    putStrLn "Nenhum tempo registrado nessa dificuldade!"

desenhaRanking [] _ lin col = do
    setCursorPosition lin col
    putStrLn ("+" ++ take 83 (cycle "-") ++ "+")

desenhaRanking (x:xs) indice lin col = do
    setCursorPosition lin col
    putStrLn ("+" ++ take 83 (cycle "-") ++ "+")
    setCursorPosition (lin + 1) col
    putStrLn ("|  " ++ show indice ++ "° " ++ take 25 (cycle ".") ++ take 23 (formataTempo x) ++ take 28 (cycle ".") ++ "  |")
    desenhaRanking xs (indice + 1) (lin + 2) col

-- | Exibe o ranking correspondente.
--
-- Parâmetros:
--   * @indice@ – valor referente a dificuldade do jogo
--   * @lin@ – linha base do ranking
--   * @col@ – coluna base do ranking
--
-- Comportamento:
--   - Obtem a estrutura em que os tempos são armazenados
--   - Seleciona a lista de tempos referente a dificuldade recebida
--   - Desenha o ranking referente a essa dificuldade
exibeRanking :: Int -> Int -> Int -> IO ()
exibeRanking indice lin col = do
    dados <- lerArquivo
    evaluate (length dados)
    let (temposF, temposM, temposD) = read dados :: ([NominalDiffTime], [NominalDiffTime], [NominalDiffTime])
    let temposSelecionado
            | indice == 9 = temposF
            | indice == 16 = temposM
            | otherwise = temposD
    desenhaRanking temposSelecionado 1 lin col

-- | Exibe uma mensagem sobre a atualização do ranking.
--
-- Parâmetros:
--   * @tempo@ – tempo obtido na partida
--   * @indice@ – valor referente a dificuldade do jogo
--   * @lin@ – linha da mensagem
--   * @col@ – coluna base da mensagem
--
-- Comportamento:
--   - Obtem a estrutura em que os tempos são armazenados
--   - Seleciona a lista de tempos referente a dificuldade recebida
--   - Imprime a mensagem referente a situação do novo tempo no ranking
exibeMensagemRanking :: NominalDiffTime -> Int -> Int -> Int -> IO ()
exibeMensagemRanking tempo indice lin col = do
    dados <- lerArquivo
    evaluate (length dados)
    let (temposF, temposM, temposD) = read dados :: ([NominalDiffTime], [NominalDiffTime], [NominalDiffTime])
    let temposSelecionado
            | indice == 9 = temposF
            | indice == 16 = temposM
            | otherwise = temposD
    
    if tempo == head temposSelecionado
        then do
            setCursorPosition lin col
            putStrLn "Parabéns, novo recorde!"
    else if tempo `elem` temposSelecionado
        then do
            setCursorPosition lin col
            putStrLn "Você obteve um dos seus 5 melhores tempos!"
    else do
        setCursorPosition lin col
        putStrLn "Que pena, o tempo obtido não está entre os melhores." 
