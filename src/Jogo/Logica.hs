module Jogo.Logica where

--------------------------------------------------------------------------------
-- | Lê um comando do teclado.
--
-- Comportamento:
--   - Se a tecla pressionada for ESC, lê os próximos dois caracteres
--     para capturar sequências de setas (códigos ANSI).
--   - Caso contrário, retorna o caractere pressionado como string.
--
-- Retorno:
--   - Uma string representando a tecla ou sequência de tecla:
--       * Setas → "\ESC[A", "\ESC[B", "\ESC[C", "\ESC[D"
--       * Outros caracteres → "a", "s", etc.
--------------------------------------------------------------------------------
getKey :: IO String
getKey = do
    c1 <- getChar
    if c1 == '\ESC' then do
        c2 <- getChar
        c3 <- getChar
        return ("\ESC" ++ [c2] ++ [c3])
    else return [c1]