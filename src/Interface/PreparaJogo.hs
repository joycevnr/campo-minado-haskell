-- | Módulo responsável pela preparação do jogo.
--
-- Este módulo cuida da configuração inicial do terminal,
-- exibição do menu principal, seleção de dificuldade
-- e inicialização da partida, incluindo o desenho do
-- tabuleiro ASCII e a criação do estado inicial do jogo.
module Interface.PreparaJogo where

import System.Console.ANSI
import System.IO
import Jogo.Logica
import Jogo.Jogo (jogo, instrucoes)
import Jogo.Types

--------------------------------------------------------------------------------
-- | Configuração do terminal
--------------------------------------------------------------------------------

-- | Configura o terminal para o modo de jogo.
--
-- Comportamento:
--   - Oculta o cursor do terminal
--   - Desabilita o echo de entrada
--   - Remove o buffering da entrada padrão
--   - Limpa a tela
--
-- Deve ser chamada antes de iniciar o menu ou o jogo.
setUpTerminal :: IO ()
setUpTerminal = do
    hideCursor
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    clearScreen

--------------------------------------------------------------------------------
-- | Menu principal
--------------------------------------------------------------------------------

-- | Desenha o menu principal do jogo.
--
-- Parâmetros:
--   * @(l, c)@ – posição base para centralizar o menu na tela
--
-- Comportamento:
--   - Exibe o título do jogo
--   - Mostra as opções de dificuldade
--   - Posiciona a seta inicial de seleção
menu :: (Int, Int) -> IO ()
menu (l, c) = do
    setCursorPosition l (c - 10)
    putStr "->"
    hFlush stdout

    setCursorPosition (l - 4) (c - 6)
    putStrLn "CAMP💣  MINAD💥"

    setCursorPosition (l - 2) (c - 37)
    putStrLn "Use W/S ou Setas e ENTER para escolher a dificuldade"

    setCursorPosition l     (c - 2) >> putStrLn "FÁCIL"
    setCursorPosition (l+1) (c - 2) >> putStrLn "MÉDIO"
    setCursorPosition (l+2) (c - 3) >> putStrLn "DIFÍCIL"
    setCursorPosition (l+3) (c - 2) >> putStrLn "SAIR"

--------------------------------------------------------------------------------
-- | Início do jogo
--------------------------------------------------------------------------------

-- | Inicia o fluxo principal do jogo após o menu.
--
-- Parâmetros:
--   * @(l, c)@ – posição base usada no menu
--
-- Comportamento:
--   - Executa a seleção de dificuldade
--   - Limpa a tela
--   - Encerra o programa se a opção "SAIR" for escolhida
--   - Caso contrário, inicia a partida com o nível selecionado
comecarJogo :: (Int, Int) -> IO ()
comecarJogo (l, c) = do
    nivel <- dificuldade (l, c - 10) l
    clearScreen
    if nivel == 0
        then return ()
        else iniciarPartida nivel (l, c)

--------------------------------------------------------------------------------
-- | Inicialização da partida
--------------------------------------------------------------------------------

-- | Inicializa uma nova partida.
--
-- Parâmetros:
--   * @nivel@ – tamanho do tabuleiro e dificuldade escolhida
--   * @(l, c)@ – posição base para desenhar o tabuleiro
--
-- Comportamento:
--   - Desenha o tabuleiro ASCII
--   - Exibe instruções ao jogador
--   - Calcula os limites visuais do cursor
--   - Gera o tabuleiro lógico com bombas
--   - Cria o estado inicial do jogo
--   - Inicia o loop principal do jogo
iniciarPartida :: Int -> (Int, Int) -> IO ()
iniciarPartida nivel (l, c) = do
    let inicioL = l - nivel
    let inicioC = c - (nivel * 2)

    let linhasDesenho = nivel * 2

    -- Desenha tabuleiro ASCII
    desenhaTabuleiro inicioL inicioC nivel linhasDesenho
    instrucoes (inicioL + linhasDesenho + 2)

    -- Calcula limites corretos do cursor (visual)
    let limites = calculaLimites inicioL inicioC nivel

    -- Gera o tabuleiro lógico
    tab <- geraTabuleiroComBombas nivel nivel nivel

    let estadoInicial = EstadoJogo
            { tabuleiro = tab
            , linhas    = nivel
            , colunas   = nivel
            , bombas    = []
            , cursor    = fst limites
            , status    = EmJogo
            }

    jogo estadoInicial (fst limites) limites

--------------------------------------------------------------------------------
-- | Desenho do tabuleiro ASCII
--------------------------------------------------------------------------------

-- | Desenha o tabuleiro ASCII de forma recursiva.
--
-- Alterna entre linhas horizontais e linhas de células
-- até completar a altura total do tabuleiro.
desenhaTabuleiro :: Int -> Int -> Int -> Int -> IO ()
desenhaTabuleiro linha coluna n limite
    | limite == 0 = linhaHorizontal linha coluna n
    | even limite = linhaHorizontal linha coluna n >> proximo
    | otherwise   = linhaCelulas   linha coluna n >> proximo
  where
    proximo = desenhaTabuleiro (linha + 1) coluna n (limite - 1)

-- | Desenha uma linha horizontal do tabuleiro.
linhaHorizontal :: Int -> Int -> Int -> IO ()
linhaHorizontal l c n = do
    setCursorPosition l c
    putStrLn (take ((n * 4) + 1) (cycle "+---"))

-- | Desenha uma linha de células do tabuleiro.
linhaCelulas :: Int -> Int -> Int -> IO ()
linhaCelulas l c n = do
    setCursorPosition l c
    putStrLn (take ((n * 4) + 1) (cycle "|   "))

--------------------------------------------------------------------------------
-- | Cálculo de limites do cursor
--------------------------------------------------------------------------------

-- | Calcula os limites visuais do cursor no tabuleiro.
--
-- Retorna:
--   * Posição inicial do cursor
--   * Posição máxima permitida
calculaLimites :: Int -> Int -> Int -> ((Int, Int), (Int, Int))
calculaLimites inicioL inicioC nivel =
    ( inicio
    , limite
    )
  where
    inicio =
        ( inicioL + 1
        , inicioC + 2
        )

    limite =
        ( (inicioL + 1) + 2 * (nivel - 1)
        , (inicioC + 2) + 4 * (nivel - 1)
        )

--------------------------------------------------------------------------------
-- | Lógica de seleção de dificuldade
--------------------------------------------------------------------------------

-- | Controla a navegação no menu de dificuldade.
--
-- Permite mover a seta com W/S ou setas direcionais
-- e confirma a seleção com ENTER.
dificuldade :: (Int, Int) -> Int -> IO Int
dificuldade (linha, coluna) limite = do
    comando <- getKey

    limpaSeta linha coluna

    let index     = linha - limite
    let novaLinha = calculaNovaLinha comando index limite

    desenhaSeta novaLinha coluna

    if comando == "\n"
        then selecionaNivel linha limite
        else dificuldade (novaLinha, coluna) limite

--------------------------------------------------------------------------------
-- | Funções auxiliares do menu
--------------------------------------------------------------------------------

-- | Remove a seta da posição atual.
limpaSeta :: Int -> Int -> IO ()
limpaSeta l c = do
    setCursorPosition l c
    putStr "  "
    hFlush stdout

-- | Desenha a seta indicadora no menu.
desenhaSeta :: Int -> Int -> IO ()
desenhaSeta l c = do
    setCursorPosition l c
    putStr "->"
    hFlush stdout

-- | Calcula a nova posição da seta no menu.
--
-- Implementa navegação circular entre as opções.
calculaNovaLinha :: String -> Int -> Int -> Int
calculaNovaLinha comando index limite
    | comando `elem` ["\ESC[A","w","W"] = (index - 1) `mod` 4 + limite
    | comando `elem` ["\ESC[B","s","S"] = (index + 1) `mod` 4 + limite
    | otherwise = index + limite

-- | Retorna o nível correspondente à opção selecionada.
--
-- Valores retornados:
--   * 9   → Fácil
--   * 16  → Médio
--   * 21  → Difícil
--   * 0   → Sair
selecionaNivel :: Int -> Int -> IO Int
selecionaNivel linha limite
    | linha == limite     = return 9
    | linha == limite + 1 = return 16
    | linha == limite + 2 = return 21
    | otherwise           = return 0
