module Interface.PreparaJogo where
import System.Console.ANSI
import System.IO
import Jogo.Logica
import Jogo.Jogo (jogo, instrucoes)

--------------------------------------------------------------------------------
-- | Configura o terminal para a execução do jogo.
--
-- Ações realizadas:
--   1. Esconde o cursor;
--   2. Desativa o 'echo';
--   3. Desativa o buffering;
--   4. Limpa a tela inicial.
--------------------------------------------------------------------------------
setUpTerminal :: IO()
setUpTerminal = do
    hideCursor
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    clearScreen

--------------------------------------------------------------------------------
-- | Exibe o menu principal do jogo.
--
-- Parâmetros:
--   @(l, c)@ – posição central onde o menu será desenhado.
--
-- O menu apresenta as opções:
--   * FÁCIL
--   * MÉDIO
--   * DIFÍCIL
--   * SAIR
--
-- A seta "->" começa posicionada sobre a primeira opção.
--------------------------------------------------------------------------------
menu :: (Int,Int) -> IO ()
menu (l,c)= do
    setCursorPosition l (c-10)
    putStr "->"
    hFlush stdout
    setCursorPosition (l-4) (c-6)
    putStrLn "CAMP💣  MINAD💥"
    setCursorPosition (l-2) (c-37)
    putStrLn "Use W/S ou Setas e ENTER para escolher a dificuldade"
    
    -- Opções
    setCursorPosition l (c-2)
    putStrLn "FÁCIL"
    setCursorPosition (l+1) (c-2)
    putStrLn "MÉDIO"
    setCursorPosition (l+2) (c-3)
    putStrLn "DIFÍCIL"
    setCursorPosition (l+3) (c-2)
    putStrLn "SAIR"

--------------------------------------------------------------------------------
-- | Gerencia o fluxo de início de jogo.
--
-- Parâmetros:
--   @(l, c)@ – posição central da tela.
--
-- A função:
--   1. Chama 'dificuldade' para obter o nível (9, 16, 20 ou 0).
--   2. Limpa a tela.
--   3. Desenha o tabuleiro adequado ao nível.
--   4. Desenha instruções do jogo.
--   5. Inicia a função principal 'jogo'.
--
-- Retorno:
--   Se o jogador escolher SAIR, retorna imediatamente.
--------------------------------------------------------------------------------
comecarJogo :: (Int, Int) -> IO ()
comecarJogo (l,c) = do
    nivel <- dificuldade (l, c - 10) l
    clearScreen
    if nivel == 0
        then return ()
        else do
            let inicioL = l - (nivel)
            let inicioC = c - (nivel*2)
            let limite  = (nivel * 2) 
            
            tabuleiro (inicioL, inicioC) nivel limite
            
            instrucoes (l + nivel + 2)
            
            let limiteSuperior = inicioL + 1
            let limiteInferior = (nivel - 1) + l
            let limiteEsquerdo = inicioC + 2
            let limiteDireito  = (nivel * 2) + c - 2
            
            jogo (limiteSuperior, limiteEsquerdo) ((limiteSuperior, limiteEsquerdo), (limiteInferior, limiteDireito)) []

--------------------------------------------------------------------------------
-- | Função recursiva que desenha o grid do tabuleiro/ o tabuleiro ASCII do jogo.
--
-- Alterna entre desenhar linhas de separação (+---+) e linhas de células (|   |).
--
-- Parâmetros:
--   * @(linha, coluna)@ – posição de início do tabuleiro
--   * @n@ – largura lógica (relacionada ao tamanho real em colunas)
--   * @limite@ – número de linhas restantes para desenhar
--
-- O tabuleiro é construído recursivamente, alternando entre linhas:
--   * ímpares -> '|   |   | ... |'
--   * pares   -> '+---+---+ ... +'
--
-- Quando limite chega a 0, a última linha horizontal é desenhada.
--------------------------------------------------------------------------------
tabuleiro :: (Int, Int) -> Int -> Int -> IO ()
tabuleiro (linha,coluna) n limite = do
    setCursorPosition linha coluna
    if limite == 0 then
        putStrLn (take ((n*4)+1) (cycle "+---"))
    else do
        if even limite
            then putStrLn (take ((n*4)+1) (cycle "+---"))
            else putStrLn (take ((n*4)+1) (cycle "|   "))

        tabuleiro (linha + 1, coluna) n (limite - 1)

--------------------------------------------------------------------------------
-- | Lógica de seleção do Menu (Input).
--
-- Permite navegar entre as opções usando Setas ou W/S.
-- Parâmetros:
--   * @(linha, coluna)@ – posição atual do cursor "->"
--   * @limite@ – linha onde começa a primeira opção do menu
--
-- Comandos suportados:
--   * @\"\ESC[A\"@ -> seta para cima
--   * @\"\ESC[B\"@ -> seta para baixo
--   * @\"\n\"@ -> Enter
--
-- A seta move pelas 4 opções usando aritmética modular:
--
-- > (mod (index ± 1) 4) + limite
--
-- Retorno (Int):
--   * 9  -> Fácil
--   * 16 -> Médio
--   * 20 -> Difícil
--   * 0  -> Sair
--------------------------------------------------------------------------------
dificuldade :: (Int,Int) -> Int-> IO Int
dificuldade (linha,coluna) limite = do
    comando <- getKey
    setCursorPosition linha coluna
    putStr "  "
    hFlush stdout
    
    let index = linha - limite
    let novaLinha
            -- Cima (Seta UP ou W)
            | comando == "\ESC[A" || comando == "w" || comando == "W" = (mod (index-1) 4) + limite
            -- Baixo (Seta DOWN ou S)
            | comando == "\ESC[B" || comando == "s" || comando == "S" = (mod (index+1) 4) + limite
            | otherwise = linha
    setCursorPosition novaLinha coluna
    putStr "->"
    hFlush stdout

    -- Verifica seleção
    if comando == "\n" -- Enter
        then 
            if linha == limite then return 9        -- Fácil
            else if linha == (limite+1) then return 16 -- Médio
            else if linha == (limite+2) then return 20 -- Difícil
            else return 0                              -- Sair
        else dificuldade (novaLinha, coluna) limite