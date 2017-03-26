{-|
Module      : Tarefa 2
Description : Modulo Haskell contendo funções para a execução de comandos no jogo BomberMan.
Copyright   : Pedro Mendes a79003;
              Francisco Reinolds a82982;

Um módulo contendo definições Haskell para a execução de comandos no jogo BomberMan.
-}
module Move where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.Char

-- |Estado do Mapa
type MapState = [String]
-- | Par de coordenadas
type Pos = (Int, Int)
-- |((x,y),número do jogador,raio,tempo)
type BombState = (Pos,Int,Int,Int) 
-- |(número,(x,y),bombBuffs,flameBuffs)
type PlayerInfo = (Int,Pos,Int,Int) 

{-|Esta função executa todas as funções necessárias para a alteração do Estado do Mapa caso seja possível /ver 'validPlay'/ e por fim devolve o estado o novo Estado do Mapa./ver 'execPlay'/

==Funções auxiliares:
*'validPlay'
*'execPlay'

==Variáveis:
* mapstate: O Estado do Mapa
* player: Número do jogador
* key: Tecla premida pelo jogador
-}
move :: MapState -- ^Estado do Mapa
           -> Int -- ^Jogador
           -> Char -- ^Tecla
           -> MapState -- ^Resultado
move mapstate player key = if (validPlay mapstate (getPlayerInfo mapstate player) key)
                              then (execPlay mapstate (getPlayerInfo mapstate player) key)
                              else mapstate

{-|Função que ao receber o Estado do Mapa e o Número de Identificação do Jogador dá como Resultado a informação relativa a este.

==Funções auxiliares:
*'auxPlayerState'

==Variáveis:
*@((h0:hs):t@: Estado do Mapa 
*@n@:Número de Identificação do Jogador 
-}
getPlayerInfo :: MapState -- ^ Estado do Mapa
                 -> Int -- ^ Número do jogador
                 -> PlayerInfo -- ^ Resultado
getPlayerInfo [] n = (0,(0,0),0,0)
getPlayerInfo ((h0:hs):t) n = if ([h0] == show n)
                                 then auxPlayerState (h0:hs)
                                 else getPlayerInfo t n
                        where auxPlayerState (h0:hs) = if (elem '+' (h0:hs))
                                                          then if (elem '!' (h0:hs))
                                                                  then let [l0,l1,l2,l3] = words (h0:hs)
                                                                       in (read (l0) :: Int,(read (l1) :: Int,read l2 :: Int),countBombs l3,countFlames l3)
                                                                  else let [l0,l1,l2,l3] = words (h0:hs)
                                                                       in (read (l0) :: Int,(read (l1) :: Int,read l2 :: Int),length(l3),0)
                                                          else if (elem '!' (h0:hs))
                                                                  then let [l0,l1,l2,l4] = words (h0:hs)
                                                                       in (read (l0) :: Int,(read (l1) :: Int,read l2 :: Int),0,length(l4))
                                                                  else let [l0,l1,l2] = words (h0:hs)
                                                                       in (read (l0) :: Int,(read (l1) :: Int,read l2 :: Int),0,0)
                              countBombs (h:t) |h=='+'=1+countBombs t
                                               |otherwise=0
                              countFlames [] = 0
                              countFlames (h:t)|h=='!'=1+countFlames t
                                               |otherwise=0+countFlames t
{-|Esta função verifica se a jogada é valida, ou seja, 
Devolve @True@ se:

* O jogador está a tentar andar para um espaço vazio

* O jogador está a tentar colocar uma bomba onde não está já uma e se está dentro dos limites do número de bombas que pode colocar

Devolve @False@ se alguma destas condições não se verificar.

==Funções auxiliares:
*'getPos'
*'validBomb'
*'getBombState'
*'numBombs'

==Variáveis:
*@mapstate@: Estado do Mapa
*@n@: número do jogador
*@(x,y)@: coordenadas do jogador
*@b@: número de bombas adicionais que pode colocar
*@f@: raio das bombas colocadas por este jogador
*@key@: tecla que o jogador premiu
-}
validPlay :: MapState -- ^Todo o mapa e cordenadas
             -> PlayerInfo -- ^Informação do jogador
             -> Char -- ^Tecla premida pelo jogador
             -> Bool -- ^Resultado
validPlay _ (0,(0,0),0,0) _ = False
validPlay mapstate (n,(x,y),b,f) key |key == 'U' = ((getPos mapstate (x,y-1)) /= '#') && ((getPos mapstate (x,y-1)) /= '?')
                                     |key == 'L' = ((getPos mapstate (x-1,y)) /= '#') && ((getPos mapstate (x-1,y)) /= '?')
                                     |key == 'D' = ((getPos mapstate (x,y+1)) /= '#') && ((getPos mapstate (x,y+1)) /= '?')
                                     |key == 'R' = ((getPos mapstate (x+1,y)) /= '#') && ((getPos mapstate (x+1,y)) /= '?')
                                     |key == 'B' = if (b >= (numBombs (getBombState mapstate) n))
                                                      then validBomb (getBombState mapstate) (n,(x,y),b,f)
                                                      else False

{-|Função que ao receber o Estado do Mapa e uma Posição (par de coordenadas) devolve-nos o caracter que se encontra nessa posição. 

==Variáveis:
*@mapstate@: Estado do Mapa 
*@pos@: Caracter da Posição em Questão
-}
getPos :: MapState -- ^ Estado do Mapa
          -> Pos -- ^ Posição do caracter
          -> Char -- ^ Resultado
getPos mapstate pos = y
    where
    (x:xs) = drop (snd pos) mapstate
    (y:ys) = drop (fst pos) x

{-|Esta função recebe o estado de todas as bombas e a informação do jogador que quer colocar outra e devolve @True@ se for possível dentro das regras e @False@ caso contrário.

Por ter muitas variáveis esta função pode ser um bocado dificil de ler. Mas o raciocínio é muito simples.

Primeiro vemos se as coordenadas da primeira bomba da lista são iguais às coordenadas do jogador:

@
(xb,yb) /= (xp,yp)
@

depois invocamos a função recursivamente para o resto da lista.

==Variáveis:
*@(xb,yb)@: coordenadas da primeira bomba da lista
*@pb@: número do jogador que colocou a bomba
*@rb@: raio da bomba
*@tb@: instantes que faltam para a bomba explodir
*@xs@: resto da lista de bombas
*@np@: número do jogador
*@(xp,yp)@: coordenadas do jogador
*@bp@: bombas adicionais que o jogador pode colocar
*@fp@: raio das bombas do jogador
-}
validBomb :: [BombState] -- ^ Estado de todas as bombas
             -> PlayerInfo -- ^ Informação de o jogador em causa
             -> Bool -- ^ Resultado
validBomb [] _ = True
validBomb (((xb,yb),pb,rb,tb):xs) (np,(xp,yp),bp,fp) = ((xb,yb) /= (xp,yp)) && validBomb xs (np,(xp,yp),bp,fp)

{-|Esta função recebe todas a bombas neste momento colocadas e o número de um jogador e devolve quantas bombas esse jogador tem activas.
-}
numBombs :: [BombState] -- ^Estado de todas a Bombas
            -> Int -- ^Número do jogador
            -> Int -- ^Resultado
numBombs [] _ = 0
numBombs (((x,y),p,r,t):xs) n = if p == n
                                   then 1 + numBombs xs n
                                   else 0 + numBombs xs n

{-|Função que ao receber o Estado do Mapa dá informações relativas a todas as bombas ainda por explodir no mapa.

==Variáveis:
*@((h0:hs):t)@: Estado do Mapa 
-}
getBombState :: MapState -- ^ Estado do Mapa
                -> [BombState] -- ^ Resultado
getBombState [] = []
getBombState ((h0:hs):t) = if (h0 == '*') 
                        then auxBombState (h0:hs) : getBombState t
                        else getBombState t
    where auxBombState (h) = let [l0,l1,l2,l3,l4] = (map read (words (tail h)))::[Int]
                             in ((l0,l1),l2,l3,l4)

{-|Esta função executa a jogada em si. Assume que esta é válida e muda as coordenadas do jogador, a quantidade de buffs que este tem ou adiciona uma bomba.

==Funções auxiliares:
*'getBombs'
*'getFlames' 
*'setState'
*'deleteBuff'

==Variáveis:
*@mapstate@: Estado do Mapa
*@n@: número do jogador
*@(x,y)@: coordenadas do jogador
*@b@: número de bombas adicionais que pode colocar
*@f@: raio das bombas colocadas por este jogador
*@key@: tecla que o jogador premiu
-}
execPlay :: MapState -- ^ Estado do Mapa
            -> PlayerInfo -- ^ Estado do jogador
            -> Char -- ^ Tecla que o jogador premiu
            -> MapState -- ^ Resultado
execPlay mapstate (n,(x,y),b,f) key |key == 'U' = if (elem (x,y-1) (getBombs mapstate))
                                                     then deleteBuff (setState 'w' (mapstate) (n,(x,y-1),b+1,f)) (x,y-1)
                                                     else if (elem (x,y-1) (getFlames mapstate))
                                                             then deleteBuff (setState 'w' mapstate (n,(x,y-1),b,f+1)) (x,y-1)
                                                             else setState 'w' mapstate (n,(x,y-1),b,f)
                                    |key == 'L' = if (elem (x-1,y) (getBombs mapstate))
                                                     then deleteBuff (setState 'w' (mapstate) (n,(x-1,y),b+1,f)) (x-1,y)
                                                     else if (elem (x-1,y) (getFlames mapstate))
                                                             then deleteBuff (setState 'w' mapstate (n,(x-1,y),b,f+1)) (x-1,y)
                                                             else setState 'w' mapstate (n,(x-1,y),b,f)
                                    |key == 'R' = if (elem (x+1,y) (getBombs mapstate))
                                                     then deleteBuff (setState 'w' (mapstate)(n,(x+1,y),b+1,f)) (x+1,y)
                                                     else if (elem (x+1,y) (getFlames mapstate))
                                                             then deleteBuff (setState 'w' mapstate (n,(x+1,y),b,f+1)) (x+1,y)
                                                             else setState 'w' mapstate (n,(x+1,y),b,f)
                                    |key == 'D' = if (elem (x,y+1) (getBombs mapstate))
                                                     then deleteBuff (setState 'w' (mapstate) (n,(x,y+1),b+1,f)) (x,y+1)
                                                     else if (elem (x,y+1) (getFlames mapstate))
                                                             then deleteBuff (setState 'w' mapstate (n,(x,y+1),b,f+1)) (x,y+1)
                                                             else setState 'w' mapstate (n,(x,y+1),b,f)
                                    |key == 'B' = setState 'b' mapstate (n,(x,y),b,f)

{-|Função que ao receber o Estado do Mapa dá-nos as coordenadas dos PowerUps Bombs presentes no mapa ainda por apanhar.

==Variáveis:
*@((h0:hs):t)@:Estado do Mapa 
-}
getBombs :: MapState -- ^ Estado do Mapa
            -> [Pos] -- ^ Resultado
getBombs [] = []
getBombs ((h0:hs):t) = if (h0 == '+')
                          then auxBombsState (h0:hs) : getBombs t
                          else getBombs t
                where auxBombsState h = let l0:l1:ls = (map read (words (tail h)))::[Int]
                                        in (l0, l1)

{-|Função que ao receber o Estado do Mapa dá-nos as coordenadas dos PowerUps Flames presentes no mapa ainda por apanhar.

==Variáveis:
*@((h0:hs):t)@: Estado do Mapa
-}
getFlames :: MapState -- ^ Estado do Mapa
             -> [Pos] -- ^ Resultado
getFlames [] = []
getFlames ((h0:hs):t) = if (h0 == '!')
                           then auxFlamesState h0 : getFlames t
                           else getFlames t
                where auxFlamesState (h0) = let (l0:l1:ls) = (map read (words (tail (h0:hs))))::[Int]
                                            in (l0, l1)

{-|Esta função recebe um caracter, o Estado do Mapa, a informação de um jogador e devolve o Estado do Mapa.

O caracter que esta recebe serve para distinguir quando esta função está a colocar uma bomba ou a mover um jogador. 

Decidimos usar esta estratégia para usar apenas uma função em vez de duas pois achamos mais simples de ler.

==Funções auxiliares:
*'clean'

==Variáveis:
*@i@: caracter de distinção
*@((h0:ht):t)@: Estado do Mapa
*@n@: número do jogador
*@(x,y)@: coordenadas do jogador
*@b@: número de bombas adicionais que pode colocar
*@f@: raio das bombas colocadas por este jogador
-}
setState :: Char -- ^ Tecla Utilizada
            -> MapState -- ^ Estado do Mapa
            -> PlayerInfo -- ^ Informação relativa ao jogador
            -> MapState -- ^ Resultado
setState i ((h0:ht):t) (n,(x,y),b,f) |i == 'w' && h0 == (intToDigit n) = clean(show n ++ " " ++ show x ++ " " ++ show y ++ " " ++ replicate b '+' ++ replicate f '!'): t
                                     |i == 'b' && (h0 == '*' || h0 == '0' || h0 == '1' || h0 == '2' || h0 == '3') = (unwords["*",show x,show y,show n,show (f+1),"10"]) : ((h0:ht):t) 
                                     |otherwise = (h0:ht) : setState i t (n,(x,y),b,f)

{-|Esta função serve apenas para limpar espaços a mais que são consequência da função 'setState' para a variável @i@ igual a @'p'@

==Variáveis:
*@[x,y]@:String Original 
-}
clean :: String -- ^String original
         -> String -- ^Resultado
clean [x,y] = if (y == ' ')
                 then if (x == ' ')
                         then []
                         else [x]
                 else [x,y]
clean (x:y:t) = if (x == ' ' && y == ' ')
                   then clean (x:t)
                   else x : clean (y:t)

{-|Função que ao receber o Estado do Mapa e a Posição do PowerUp que será apagado (por ter sido apanhado por um jogador) faz a atualização do Estado do Mapa sem a informação desse PowerUp.

==Variáveis:
*@(h:t)@: Estado do Mapa 
*@(b,c)@: Posição do Buff que será apagado
*@(h0:h1:h2:h3)@: Resultado do words h 
-}
deleteBuff :: MapState -- ^ Estado do Mapa 
              -> Pos -- ^ Posição do Buff que será apagado
              -> MapState -- ^ Resultado
deleteBuff [] (_,_) = []
deleteBuff (h:t) (b,c) = if ((head h) == '!' || (head h) == '+')
                            then if ((read $ h1 :: Int) == b && (read $ h2 :: Int) == c)
                                    then t 
                                    else (h) : deleteBuff (t) (b,c)
                            else h:deleteBuff (t) (b,c)
    where (h0:h1:h2:h3) = words (h)
{-
-- | main
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
             -}
