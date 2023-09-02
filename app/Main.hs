module Main (main) where

import Lib

{-Nome: Lucas Gomes dos Santos , Matricula: 20.1.4108-}

{-
Faça os 9 exercícios no arquivo Lib.hs e depois volte à este.

EXERCÍCIO 10
Combine tudo que fez neste projeto e implemente
a seguinte função que deve: 
  1. Ler um número n do usuário.
  2. Ler n linhas.
  3. Mostrar o tabuleiro correspondente, 
     que ignorará os caracteres inválidos.
  4. Mostrar a contagem das cores no tabuleiro.
  5. Mostrar o tabuleiro correspondente trocando todos 
     os vermelhos por amarelos.
-}

main :: IO ()
main = do
   linhas <- readLines {-readLines implementado na lib lê um numero n e depois n linhas (1 e 2) -}
   let xss = createBoard(readColorLines linhas)
   putStrLn $ show xss {- 3 - mostra o tabuleiro correspondente-}
   printCounters xss {- 4 - printCounters printa a contagem de cores do tabuleiro -}
   let xsss = fill Red Yellow xss {- 5 - Troca Red por Yellow -}
   putStrLn $ show xsss
   



{-
EXERCÍCIO OPCIONAL
Incremente esta aplicação com funcionalidades adicionais. 
Seja criativo e mantenha a boa qualidade do seu código 
e do estilo funcional. Escreva as funções no Lib.hs
e adapte a interação com o usuário no Main.hs.
-}