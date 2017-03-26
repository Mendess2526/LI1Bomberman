{-|
Module      : Tarefa 1
Description : Modulo Haskell contendo funções para gerar um mapa no jogo BomberMan.
Copyright   : Pedro Mendes a79003;
              Francisco Reinolds a82982

Um módulo contendo definicoes Haskell para gerar um mapa no jogo BomberMan.
-}
module Mapa where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.Char

{-|Função principal à qual fornecemos o tamanho do mapa e uma seed (baseada na seed obteremos o esquema de Bombs e Flames presentes no mapa).

Recorrendo a funções auxiliares, esta função ultimamente dar-nos-á o layout final do mapa tanto a nível de estrutura como a nível de Bombs e 
Flames e as suas devidas coordenadas. 

Quando o size do mapa é 5, é invocada a função 'mapa5' que nos irá dar o único layout possível para o mapa de tamanho 5.

A função 'numPedras' irá ser invocada para podermos calcular o tamanho da string necessária para preencher o mapa. 

==Funções auxiliares:
*'walls'
*'mapa5'
*'numPedras'
*'mapaGen'

==Variáveis:
*@size@: Tamanho do mapa 
*@seed@: Número que determina o resultado aleatório
-}
mapa :: Int -- ^size
        -> Int -- ^seed 
        -> [String] -- ^resultado
mapa size seed = if (size /= 5) 
                    then mapaGen (walls (take (size*size - ((size*size - ((size-2)*(size-2))) + 12 + (numPedras size))) (randomRs (0,99) (mkStdGen seed))) size size) size
                    else mapa5 

{-|Função que recebe a string que formará o mapa, a largura e a altura na qual se encontra atualmente 
(na primeira invocação da função será @(0,0)@). Com essa string, estende-a e insere -1's que representam walls indestrutíveis. Coloca 
também os 12 espaços vazios obrigatórios (os 3 localizados em cada canto do mapa). 

==Funções Auxiliares:
*'interior'

==Variáveis:
*@lista@: Lista que irá compor o mapa 
*@width@: Largura do mapa
*@height@: Altura do mapa
-}
walls :: [Int] -- ^ lista que irá compor o mapa
           -> Int -- ^ width do mapa
           -> Int -- ^ height do mapa
           -> [Int] -- ^resultado
walls lista _ 0 = []
walls lista width height |height == width ||height == 1     = (replicate width (-1)) ++ walls lista width (height-1)
                         |height == (width-1) ||height == 2 = [-1,99,99] ++ take (width-6) lista ++ [99,99,-1] ++ walls (drop (width-6) lista) width (height-1)
                         |height == (width-2) ||height == 3 = [-1,99] ++ interior (take (div width 2-2) lista) ++ [99,-1] ++ walls (drop (div width 2-2) lista) width (height-1)
                         |mod height 2 == 0                 = [-1] ++ (take (width-2) lista) ++ [-1] ++ walls (drop (width-2) lista) width (height-1)
                         |mod height 2 == 1                 = interior (take (div width 2) lista) ++ walls (drop (div width 2) lista) width (height-1)

{-|Função que faz a terceira e a antepenúltima linhas do mapa (pedra, não pedra, pedra, não pedra, pedra ,... ).

==Variáveis:
*@(h:t)@: Lista que irá compor o mapa 
-}
interior :: [Int] -- ^ lista que irá compor o mapa
              -> [Int] -- ^resultado
interior [] = [-1]
interior (h:t) = [-1,h] ++ interior t 

{-|Função que calcula o número de walls necessárias para o interior do mapa (walls alternadas nas filas ímpares).

==Variáveis:
*@x@: Tamanho do Mapa 
-} 
numPedras :: Int -- ^tamanho do mapa
             -> Int -- ^resultado
numPedras x = (2 + (div (x-7) 2))*(2 + (div (x-7) 2))

{-|Função que quando o tamanho do mapa é 5x5, é invocada com o único layout possível para um mapa dessas dimensões.

@
[['#','#','#','#','#'],['#',' ',' ',' ','#'],['#',' ','#',' ','#'],['#',' ',' ',' ','#'],['#','#','#','#','#']]
@
-}
mapa5 :: [String] -- ^ resultado
mapa5 = [['#','#','#','#','#'],['#',' ',' ',' ','#'],['#',' ','#',' ','#'],['#',' ',' ',' ','#'],['#','#','#','#','#']]

{-|Função que recebe uma lista já com o número de elementos pertencentes ao mapa que não as paredes indestrutíveis e a 
largura do mapa em questão.

==Funções auxiliares:
*'writeMap'
*'writeBuff'

==Variáveis:
*@lista@: Lista de números que irão formar o mapa 
*@width@: Largura do mapa
-}
mapaGen :: [Int] -- ^lista de números que irão formar o mapa
            -> Int  -- ^width do mapa 
            -> [String] -- ^resultado
mapaGen [] _  = [[]]
mapaGen lista width = writeMap lista width ++ writeBuff lista (width-1) 0 0 '+' ++ writeBuff lista (width-1) 0 0 '!'

{-|Função que recebe a lista com os números que após serem substituidos formarão o mapa e os seus componentes. Recebe também a width 
e com ela quebra a lista em listas mais pequenas e cada uma delas com o mesmo número de elementos que a width do mapa. 

==Funções auxiliares:
*'writeMapAux'

==Variáveis:
*@lista@: Lista de números que irão formar o mapa 
*@width@: Largura do mapa
-}
writeMap :: [Int] -- ^lista de números que irão formar o mapa 
                -> Int -- ^width do mapa
                -> [String] -- ^resultado
writeMap [] _  = []
writeMap lista width = writeMapAux (take width lista) width : writeMap (drop width lista) width 

{-|Função que recebe uma lista e a width do mapa e com esses dados cria a string que forma o mapa já com a simbologia correta. 
Ex: Bloco indestrutível @#@, Flame @?@. 

==Variáveis:
*@(h:t)@: Lista de números que irão formar o mapa 
*@width@: Largura do mapa
-}
writeMapAux :: [Int] -- ^lista de números que irão formar o mapa
               -> Int  -- ^width do mapa
               -> [Char] -- ^resultado
writeMapAux [] _ = []
writeMapAux (h:t) width = if (h > -1 && h < 40) 
                            then '?' : writeMapAux t (width-1)
                            else if (h < 0) 
                                    then '#' : writeMapAux t (width-1)
                                    else ' ' : writeMapAux t (width-1)

{-|Função que cria uma lista no final da string original com os powerups e as suas respetivas coordenadas. Esta função é chamada duas vezes, uma com o caracter @'+'@ para escrever os /powerup bombs/ e outra com o caracter @'!'@ para escrever os /powerup flames/, assim garantindo que ficam ordenados por tipo.

==Ex: 
>>> Flames na posição 2 3
! 2 3

==Variáveis:
*@(h:t)@: Lista fornecida pela função writeMapAux 
*@defaultWidth@: Largura do mapa
*@height@: Altura do mapa 
*@width@: Largura do mapa no momento 
*@c@: Caracter que indica o tipo de powerup que vai ser escrito
-}
writeBuff :: [Int] -- ^ lista originária da função writeMapAux
             -> Int -- ^ defaultWidth do mapa
             -> Int -- ^ height do mapa
             -> Int -- ^ width do mapa
             -> Char
             -> [String] -- ^resultado
writeBuff [] _ _ _ _ = []
writeBuff (h:t) defaultWidth height width c |defaultWidth == width      = writeBuff t defaultWidth (height+1) 0 c
                                            |(h == 0 ||h == 14) && c=='+'= (['+',' ']++show width++" "++ show height) : writeBuff t defaultWidth height (width+1) c
                                            |(h == 15 ||h == 29) && c=='!'= (['!',' ']++show width++" "++ show height) : writeBuff t defaultWidth height (width+1) c
                                            |otherwise                  = writeBuff t defaultWidth height (width+1) c
{-
{-|main-}
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"
             -}