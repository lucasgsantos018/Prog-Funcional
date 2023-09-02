module Lib where

import Data.Maybe
import Control.Monad


{-Nome: Lucas Gomes dos Santos , Matricula: 20.1.4108-}


{- 
Para todos os exercícios, você pode criar funções
auxiliares se achar conveniente. Não se esqueça de dar
nome aos parâmetros que for utilizar.

Considere o seguinte tipo de dado algébrico
que representa 4 cores.
-}

data Color = Red | Green | Blue | Yellow
            deriving (Eq)

-- Exemplo de valor
c1 :: Color
c1 = Yellow

{-
Por mera questão visual, definiremos que a forma 
de apresentação delas se dará pela primeira letra da cor, 
em maiúsculo. Red será "R", Green será "G", e assim por diante.

Exercício 1: Termine a instância de Show abaixo.
Não se esqueça de apagar o undefined.
-}

instance Show Color where
    show Red    = "R"
    show Green  = "G"
    show Blue   = "B"
    show Yellow = "Y" 

{-
Considere o seguinte sinônimo de tipo
que representa uma espécie de tabuleiro
de cores. Este "tabuleiro" não necessariamente é
uma matriz quadrada.
-}

type Board = [[Color]]

-- Exemplo de tabuleiro
t1 :: Board
t1 = [[Red, Blue, Blue, Green], 
      [Yellow, Red], 
      [Blue, Green, Red]]

{-
Exercício 2: Implemente a seguinte função
que deve trocar todas as ocorrências da primeira
cor no tabuleiro pela segunda cor, mantendo todas 
as outras cores inalteradas.
-}

{- Função auxiliar p/ questão 2, troca uma cor pela outra. Depois basta aplicar na matriz usando map-}
trocaCor :: Color -> Color -> Color -> Color
trocaCor cor1 cor2 corAtual = 
    if corAtual == cor1 then cor2
    else corAtual

fill :: Color -> Color -> Board -> Board
fill cor1 cor2 tabuleiro = 
    map (map (trocaCor cor1 cor2)) tabuleiro

{-
Exercício 3: Implemente a seguinte função que deve 
retornar o número de ocorrências de uma cor no tabuleiro.
-}

{- Função concat concatena todas as listas presentes no tabuleiro para transforma-lás em uma lista apenas-}
countColor :: Color -> Board -> Int
countColor cor1 tabuleiro = length (filter (== cor1) (concat tabuleiro))

{-
Exercício 4: Implemente a seguinte função que deve 
converter uma letra na cor correspondente. Estaremos 
considerando a possibilidade do caractere informado
não representar uma cor.
-}

readColor :: Char -> Maybe Color
readColor letraCor
  | letraCor == 'R' = Just Red
  | letraCor == 'G' = Just Green
  | letraCor == 'B' = Just Blue
  | letraCor == 'Y' = Just Yellow
  | otherwise = Nothing

{-
Exercício 5: Implemente a seguinte função que deve 
converter uma sequência de caracteres numa lista de 
possíveis cores correspondentes.
-}

{-Basta aplicar a função da questão 4 na string inteira usando map para criar uma lista de possiveis cores. -}
readColors :: String -> [Maybe Color]
readColors xs = map readColor xs

-- readColors "BBHYGB" ~= [Just B, Just B, Nothing, Just Y, Just G, Just B] 

{-
Exercício 6: Implemente a seguinte função que deve converter
uma lista de sequências de caracteres num tabuleiro de possíveis
cores.
-}

{- Basta aplicar a função da questão 5 na lista de string usando map para criar uma matriz de possíveis cores. -}
readColorLines :: [String] -> [[Maybe Color]]
readColorLines xss = map readColors xss 

{- 
    readColorLines ["BBHYGB", "JYG", "BKKGBGY"]
       ~= [[Just B,Just B,Nothing,Just Y,Just G,Just B],
           [Nothing,Just Y,Just G],
           [Just B,Nothing,Nothing,Just G,Just B,Just G,Just Y]]
-}

{-
Exercício 7: Implemente a seguinte função que deve converter
um tabuleiro de possíveis cores em um tabuleiro comum, simplesmente
eliminando todas as cores invalidadas no processo.
-}

{- Função auxiliar para remover cores invalidas, catmaybe remove Nothing e retorna apenas Just's  -}
retirarInvalidos :: [Maybe Color] -> [Color]
retirarInvalidos xs = catMaybes xs

{- retirarInvalidos é usado com map para cada lista da matriz-}
createBoard :: [[Maybe Color]] -> Board
createBoard xss = map retirarInvalidos xss

{-
    createBoard (readColorLines ["BBHYGB", "JYG", "BKKGBGY"])
        ~= [[B,B,Y,G,B],
            [Y,G],
            [B,G,B,G,Y]]
-}

{-
Exercício 8: Implemente a seguinte função que lê um número n 
digitado do teclado e depois lê n linhas, retornando-as em uma lista.
-}
{- Transforma n em um inteiro para ler n linhas e transformar em lista-}

readLines :: IO [String]
readLines = do
    nLinhas <- getLine -- 
    let n = read nLinhas :: Int
    replicateM n getLine

{-
Exercício 9: Implemente a seguinte função que mostra na tela
a contagem de cada uma das cores, exibindo inclusive as cores
cuja contagem for zero.
-}

printCounters :: Board -> IO ()
printCounters tabuleiro = do
    let r = countColor Red tabuleiro
        g = countColor Green tabuleiro
        b = countColor Blue tabuleiro
        y = countColor Yellow tabuleiro
    putStrLn $ "Red: " ++ show r ++ ", Green: " ++ show g ++ ", Blue: " ++ show b ++ ", Yellow: " ++ show y
    

{- 
Exercício 10: Vá ao arquivo Main.hs e faça o que se pede.
-}