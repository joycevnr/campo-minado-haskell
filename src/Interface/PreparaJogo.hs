module Interface.PreparaJogo where
import System.Console.ANSI
import System.IO
import Jogo.Logica
import Jogo.Jogo

--------------------------------------------------------------------------------
-- | Configura o terminal para a execução do jogo.
--
--  - Esconde o cursor
--  - Desativa o echo do teclado
--  - Desativa o buffering
--  - Limpa a tela
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
    putStrLn "Use as setas do seu teclado e Enter para escolher sua díficuldade ou sair"
    setCursorPosition l (c-2)
    putStrLn "FÁCIL"
    setCursorPosition (l+1) (c-2)
    putStrLn "MÉDIO"
    setCursorPosition (l+2) (c-3)
    putStrLn "DIFÍCIL"
    setCursorPosition (l+3) (c-2)
    putStrLn "SAIR"

--------------------------------------------------------------------------------
-- | Inicia o jogo após o jogador escolher a dificuldade.
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
            instrucoes l
            jogo ((inicioL+1),(inicioC+2)) (((inicioL+1),(inicioC+2)),(((nivel-1)+l),((nivel*2)+c-2))) []
    
--------------------------------------------------------------------------------
-- | Desenha o tabuleiro ASCII do jogo.
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
        putStrLn (take ((n*4)+1) "+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+")
    else do
        if even limite
            then putStrLn (take ((n*4)+1) "+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+")
            else putStrLn (take ((n*4)+1) "|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |")

        tabuleiro (linha + 1, coluna) n (limite - 1)
    
--------------------------------------------------------------------------------
-- | Menu de seleção de dificuldade.
--
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
-- Retorno ao pressionar Enter:
--   * FÁCIL -> 9
--   * MÉDIO -> 16
--   * DIFÍCIL -> 20
--   * SAIR -> 0
--------------------------------------------------------------------------------
dificuldade :: (Int,Int) -> Int-> IO Int
dificuldade (linha,coluna) limite = do
    comando <- getKey
    setCursorPosition linha coluna
    putStr "  "
    hFlush stdout
    let index = linha-limite
        novaLinha
            | comando == "\ESC[A" = (mod (index-1) 4) + limite
            | comando == "\ESC[B" = (mod (index+1) 4) + limite
            | otherwise           = linha

    setCursorPosition novaLinha coluna
    putStr "->"
    hFlush stdout

    if comando == "\n"
        then 
            if linha == limite then return 9
            else if linha == (limite+1) then return 16
            else if linha == (limite+2) then return 20
            else return 0
        else dificuldade (novaLinha, coluna) limite