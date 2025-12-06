module Main (main) where
import System.Console.ANSI
import System.IO
import Interface.PreparaJogo

--------------------------------------------------------------------------------
-- | Ponto de entrada do jogo Campo Minado.
--
-- Comportamento:
--   1. Obtém o tamanho do terminal com getTerminalSize
--   2. Verifica se o terminal tem tamanho suficiente:
--        - mínimo de 91 colunas e 43 linhas
--        - caso contrário, imprime mensagem de erro
--   3. Configura o terminal chamando setUpTerminal
--   4. Exibe o menu centralizado
--   5. Inicia o jogo com comecarJogo
--   6. Limpa a tela e mostra o cursor ao final
--
-- Observação:
--   - Caso getTerminalSize falhe, exibe mensagem de erro.
--------------------------------------------------------------------------------
main :: IO ()
main = do
    tamanho <- getTerminalSize
    case tamanho of
        Just (linhas,colunas) -> do
            if (colunas<91) || (linhas<43) 
                then do
                    print "Tamanho de tela insuficiente!"
                else do
                    setUpTerminal
                    menu ((linhas `div` 2),(colunas `div` 2))
                    comecarJogo ((linhas `div` 2),(colunas `div` 2))
                    clearScreen
                    showCursor
        Nothing -> print "Não foi possível obter o tamanho do terminal."
    
    
 

