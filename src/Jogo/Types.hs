module Jogo.Types where

-- Representa uma posição no tabuleiro usando índices (linha, coluna).
-- Sempre em coordenadas lógicas, começando em (0,0).
type Posicao = (Int, Int)

-- Representa uma célula do campo minado.
-- Cada célula sabe:
--  - se contém uma bomba
--  - quantas bombas existem nas células vizinhas
--  - se já foi revelada pelo jogador
--  - se está marcada com bandeira
data Celula = Celula
    { temBomba :: Bool   -- True se houver uma bomba nesta célula
    , vizinhas :: Int    -- Quantidade de bombas ao redor
    , revelada :: Bool   -- Indica se a célula já foi aberta
    , marcada  :: Bool   -- Indica se o jogador marcou com bandeira
    } deriving (Show)

-- O tabuleiro do jogo é uma matriz de células.
-- O acesso é feito como tabuleiro !! linha !! coluna
type Tabuleiro = [[Celula]]

-- Representa o estado geral da partida.
-- Usado para controlar o fluxo do jogo.
data Status
    = EmJogo   -- Partida em andamento
    | Vitoria  -- Todas as células seguras foram abertas
    | Derrota  -- Uma bomba foi acionada
    deriving (Show, Eq)

-- Estrutura principal que guarda todo o estado do jogo.
-- Centraliza informações lógicas e facilita a passagem
-- de estado entre as funções.
data EstadoJogo = EstadoJogo
    { tabuleiro :: Tabuleiro  -- Tabuleiro lógico do jogo
    , linhas    :: Int        -- Quantidade de linhas do tabuleiro
    , colunas   :: Int        -- Quantidade de colunas do tabuleiro
    , bombas    :: [Posicao]  -- Posições das bombas (pode ser usado para debug)
    , cursor    :: Posicao    -- Posição atual do cursor no tabuleiro lógico
    , status    :: Status     -- Situação atual da partida
    } deriving (Show)
