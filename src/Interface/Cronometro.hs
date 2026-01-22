-- | Módulo responsável pelo cronometro do jogo.
--
-- Este módulo cuida de tudo relacionado a cronometro e tempo.
module Interface.Cronometro where

import Data.Time
import System.IO (hFlush, stdout)
import Control.Concurrent
import Control.Exception (evaluate)
import Jogo.Logica
import System.Console.ANSI

-- | Exibe o cronômetro do jogo.
--
-- Parâmetros:
--   * @tempoIni@ – tempo em que a partida foi iniciada
--   * @lin@ – linha do cronômetro
--   * @col@ – coluna base do cronômetro
--
-- Comportamento:
--   - Calcula o tempo decorrido através da subtração do tempo atual pelo inicial
--   - Exibe o tempo na posição recebida
--   - Espera 1 segundo para realizar essa lógica novamente
cronometro :: UTCTime -> Int -> Int -> IO ()
cronometro tempoIni lin col= do
    tempoCorrente <- getCurrentTime
    let duracao = diffUTCTime tempoCorrente tempoIni
    setCursorPosition lin col
    putStr (formataTempo duracao)
    hFlush stdout
    threadDelay 1000000
    cronometro tempoIni lin col

-- | Formata o tempo de acordo com sua ordem de grandeza.
--
-- Parâmetros:
--   * @tempo@ – tempo não formatado
--
-- Retorno:
--   String que representa o tempo formatado
formataTempo :: NominalDiffTime -> String
formataTempo tempo
    | tempo < 60 = formatTime defaultTimeLocale "         %S seg         " tempo
    | tempo < 3600 = formatTime defaultTimeLocale "      %M min %S seg      " tempo
    | otherwise = formatTime defaultTimeLocale "   %h hr %M min %S seg    " tempo

-- | Atualiza uma lista de tempos adicionando um novo tempo.
--
-- A atualização feita mantém a natureza decrescente da lista
-- e não permite mais elementos do que o limite da lista,
-- descartando o maior tempo caso o limite seja atingido.
--
-- Parâmetros:
--   * @novoTempo@ – tempo a ser inserido na lista
--   * @lista@ – lista de tempos não atualizada
--   * @limite@ – quantidade de tempos permitidos na lista
--
-- Retorno:
--   Lista de tempos atualizada
adicionaTempo :: NominalDiffTime -> [NominalDiffTime] -> Int -> [NominalDiffTime]
adicionaTempo _ _ 0 = []
adicionaTempo novoTempo [] _ = [novoTempo]
adicionaTempo novoTempo (x:xs) quant
    | novoTempo < x = novoTempo : adicionaTempo x xs (quant - 1)
    | otherwise = x : adicionaTempo novoTempo xs (quant - 1)

-- | Salva um novo tempo.
--
-- Parâmetros:
--   * @novoTempo@ – tempo a ser salvo
--   * @indice@ – valor referente a dificuldade do jogo
--
-- Comportamento:
--   - Obtem a estrutura em que os tempos são armazenados
--   - Seleciona a lista de tempos referente a dificuldade recebida
--   - Acrescenta o novo tempo na lista selecionada e grava a nova estrutura no arquivo "tempos.txt"
--  
-- Observação:
--   Estrutura com os tempos: ([tempos do modo fácil], [tempos do modo médio], [tempos do modo difícil])
salvaTempo :: NominalDiffTime -> Int -> IO ()
salvaTempo novoTempo indice = do
    dados <- lerArquivo
    evaluate (length dados)
    let (temposF, temposM, temposD) = read dados :: ([NominalDiffTime], [NominalDiffTime], [NominalDiffTime])
    if indice == 9
        then gravaArquivo (show (adicionaTempo novoTempo temposF 5, temposM, temposD))
    else if indice == 16
        then gravaArquivo (show (temposF, adicionaTempo novoTempo temposM 5, temposD))
    else
        gravaArquivo (show (temposF, temposM, adicionaTempo novoTempo temposD 5))