{-|
Module      : Library
Description : Este modulo é uma copia integral do ficheiro Tarefa 4.
Copyright   : Pedro Mendes a79003;
              Francisco Reinolds a82982

Devido à Tarefa4 necessitar de ter uma função Main e o nome do Modulo também ter de ser do mesmo nome, este ficheiro foi criado para poder ser importado pela Tarefa 5.
-}
module Avanca where

import Data.Char
import Data.List
import System.Environment
import Text.Read
import Data.Maybe
import Move

-- | Elementos correspondendo a: (raio,x,y)
type Bomb  = (Int,Int,Int)
-- | Lista de bombas ('Bomb')
type Bombs = [Bomb]
-- | Posições maracadas para ser destruidas
type Marks = [(Int,Pos)]
-- | Tipo de comportamento a ter quando calcular as coordenadas marcadas pela função 'markToRemove'
data Behavior = Wall | Brick | Kill | Destroy

-- |Função que computa as consequencias da passagem do tempo
avanca :: MapState -- ^ Estado do Mapa
          -> Int -- ^ Número de ticks que faltam
          -> MapState -- ^ Resultado
avanca mapstate tick |tick<=(size-2)^2 = let tw = doTimeWall mapstate tick
                                         in alter tw
                     |otherwise        = alter mapstate
        where
        alter x = concat [removeBricks mapMarks $ gameMap x,map unwords $ removeData marked $ gameData x]
        mapMarks = filter (\(t,_) -> t == 0) marked
        gameData x =getGameData x size
        gameMap x = take size x
        size = getSize mapstate
        b = listBombs mapstate
        firstBombRadius ((r,x,y):t) = r
        marked = markToRemove mapstate b

-- |Função que recebe o Estado do Mapa e devolve os powerups, bombas e jogadores.
getGameData :: MapState -- ^ Estado do Mapa
               -> Int -- ^ Tamanho do mapa
               -> [[String]] -- ^ Resultado
getGameData l size = map words $ drop size l

-- |Função que calcula o tamanho do mapa.
getSize :: MapState -- ^ Estado do Mapa
           -> Int -- ^ Resultado
getSize (h:t) = length h

-- |Função que recebe as coordenadas que tem de ser destruidas e remove os @'?'@ corespondentes
removeBricks :: Marks -- ^ Lista de coordenadas marcadas para remover
                -> MapState -- ^ Estado do Mapa
                -> MapState -- ^ Resultado
removeBricks [] mapstate = mapstate
removeBricks ((i,(x,y)):t) mapstate = replaceBricks (x,y) ' ' (removeBricks t mapstate)

-- |Função auxiliar de 'removeBricks' que recebe coordenadas e substitui @'?'@ por @' '@.
replaceBricks :: Pos -- ^ Coordenada a remover
                    -> Char -- ^ Caracter para substituir
                    -> MapState -- ^ Estado do Mapa
                    -> MapState -- ^ Resultado
replaceBricks (x,y) c mapstate = aboveLines ++ (changedLine : (tail restLines))
                where
                  aboveLines    = take y mapstate
                  changedLine   = prefixLine ++ [c] ++ remainingLine
                  restLines     = drop y mapstate
                  prefixLine    = take x (head restLines)
                  remainingLine = drop (x+1) (head restLines)

{-| Função que recebe as coordenadas que tem de ser removidas e:
* Remove jogadores
* Remove Powerups
* Poe o temporizador de bombas afetadas por explosões a 1
* Subtrai um ao temporizador das outras bombas
-}
removeData :: Marks -- ^ Lista de coordenadas marcadas para remover
              -> [[String]] -- ^ Powerups, bombas e jogadores
              -> [[String]] -- ~ Resultado
removeData _ [] = []
removeData marks mapstate = filter (/=[]) $ map aux mapstate
                     where   
                        aux (h0:h1:h2:ht) 
                                   |h0 == "*" && isExploding ht       = []
                                   |h0 == "*" && elem (1,(x,y)) marks = (h0:h1:h2:setToOne ht)
                                   |h0 == "*"                         = (h0:h1:h2:tickDown ht)
                                   |elem (1,(x,y)) marks              = []
                                   |otherwise                         = (h0:h1:h2:ht)
                                where
                                  x = read h1 ::Int
                                  y = read h2 ::Int
                                  tickDown [h0,h1,h2] = [h0,h1,show((read h2 ::Int)-1)]
                                  setToOne [h0,h1,h2] = [h0,h1,"1"]
                                  isExploding [h0,h1,h2] = h2=="0"

-- |Função que dado o Estado do Mapa devolve a listas das bombas com o temporizador a @0@.
listBombs :: MapState -- ^ Estado do Mapa
             -> Bombs -- ^ Resultado
listBombs [] = []
listBombs mapstate = catMaybes $ map bombStats bombs
            where
              bombFilter = filter (\(h0:hs) -> h0=='*') mapstate
              bombs = map words bombFilter
              bombStats (h0:h1:h2:h3:h4:"0":hs) = Just (read h4 ::Int,read h1 ::Int,read h2 ::Int)
              bombStats _ = Nothing

-- |Função que marca para ser removido os elementos do jogo afetados pelas explosões.
markToRemove :: MapState -- ^ Estado do Mapa
                -> Bombs -- ^ Lista de bombas com o temporisador a @0@
                -> Marks -- ^ Resultado
markToRemove _ [] = []
markToRemove mapstate t = concat $ map markAllDirs t
    where 
      markAllDirs (r, x, y) = concat 
        [ 
          markDir mapstate (pred r) $ \r' -> (         x,y - r + r'), -- up
          markDir mapstate (pred r) $ \r' -> (x + r - r',         y), -- right
          markDir mapstate (pred r) $ \r' -> (         x,y + r - r'), -- down
          markDir mapstate (pred r) $ \r' -> (x - r + r',         y), -- left
          [(1,(x,y))]                                                  -- center
        ]

-- |Função que recebe o raio de uma bomba e uma direção, na forma de uma função, e calcula as coordenadas a marcar.
markDir :: MapState -- ^Estado do Mapa
           -> Int -- ^ raio da bomba
           -> (Int -> Pos) -- ^ função que calcula a proxima posição a analisar
           -> Marks -- ^ Resultado
markDir mapstate r newpos
  | r < 0 = []
  | otherwise = 
    case checkPos mapstate pos of
      Nothing      -> markDir mapstate (r - 1) newpos
      Just Wall    -> []
      Just Destroy -> [(1, pos)]
      Just Brick   -> [(0, pos)]
      Just Kill    -> (1, pos) : markDir mapstate (r - 1) newpos
      where pos = newpos r

{-|Função que devolve o que fazer a uma certa coordenada.

Retorna:
* @Brick@ se for um bloco destruivel
* @Wall@ se for um bloco indestrutivel
* @Kill@ se for um jogador
* @Destroy@ se for powerup ou bomba
-}
checkPos :: MapState -- ^ Estado do Mapa
            -> Pos -- ^ Coordenadas
            -> Maybe Behavior -- ^ Resultado
checkPos mapstate pos |y == '?'=Just Brick
                      |y == '#'=Just Wall
                      |otherwise = checkPosAux gameData pos
        where
         (x:xs) = drop (snd pos) mapstate
         (y:ys) = drop (fst pos) x
         gameData = getGameData mapstate size
         size = getSize mapstate
         checkPosAux mapstate (x,y) |aux == Nothing = Nothing
                                    |isDigit $ head $ head $ fromJust aux = Just Kill
                                    |otherwise = Just Destroy
                where
                  aux = find (\(h0:h1:h2:ht) -> h1 == show x && h2 == show y) mapstate

-- |Função que coloca a parede que cai no fim do jogo
doTimeWall :: MapState -- ^ Estado do Mapa
              -> Int -- ^ Tempo que falta
              -> MapState -- ^ Resultado
doTimeWall (h:t) tick = filter (match pos) (replaceBricks pos '#' (h:t))
                  where
                    pos = getTimeWallPos ((size^2)-tick) 1 1 size size (1,1)
                    size=(length h)-2
                    match x l = matchAux x $ words l
                    matchAux (x,y) (h0:h1:h2:ht) = not(h1==show x && h2==show y)
                    matchAux _ _ = True

-- |Função que determina a posição do proximo @'#'@ dado o tempo que falta para acabar o tempo.
getTimeWallPos :: Int -- ^ Contador de ticks
                  -> Int -- ^ minimo X (começa em @1@)
                  -> Int -- ^ minimo Y (começa em @1@)
                  -> Int -- ^ maximo X (começa igual ao tamanho do mapa)
                  -> Int -- ^ maximo Y (começa igual ao tamanho do mapa)
                  -> Pos -- ^ posição que se esta a verificar (começa em @(1,1)@)
                  -> Pos -- ^ Resultado
getTimeWallPos 0 _ _ _ _ pos = pos
getTimeWallPos i minX minY maxX maxY (x,y) 
    |x==1 && y==1      
                       = getTimeWallPos (i-1) minX      (minY+1) maxX     maxY     (x+1,  y)
    |x>=y && x/=maxX   
                       = getTimeWallPos (i-1) minX      minY     maxX     maxY     (x+1,  y) 
    |x>=y && y/=maxY   
                       = getTimeWallPos (i-1) minX      minY     maxX     maxY     (  x,y+1)
    |x==maxX && y==maxY
                       = getTimeWallPos (i-1) minX      minY     (maxX-1) (maxY-1) (x-1,  y) --reduce both max
    |x<y && x/=minX    
                       = getTimeWallPos (i-1) minX      minY     maxX     maxY     (x-1,  y)
    |x<y && y/=minY    
                       = getTimeWallPos (i-1) minX      minY     maxX     maxY     (  x,y-1)
    |x==minX && y==minY
                       = getTimeWallPos (i-1) (minX+1)  (minY+1) maxX     maxY     (x+1,  y) --increase both min

-- |Função de teste
testFunction :: [String] -- ^ Estado do Mapa
                -> Int -- ^ Quantas vezes quer repetir a função 'avanca'
                -> Int -- ^ Numero de ticks a passar para a primeira iteração da função 'avanca'
                -> [String] -- ^ Resultado
testFunction l 0 t = l
testFunction l n 0 = l
testFunction l n t = avanca (testFunction l (n-1) (t-1)) t

-- |Mapa de teste
tabteste9 = ["#########",
             "#  ?    #",
             "# # # # #",
             "#       #",
             "# # # #?#",
             "#     ? #",
             "# # #?# #",
             "#   ?   #",
             "#########",
             "0 1 1 +",
             "1 6 1",
             "2 5 4"]

-- |Mapa de teste
minitabt = ["#####",
            "#   #",
            "# #?#",
            "# ? #",
            "#####",
            "* 1 1 1 1 10",
            "0 1 3 +",
            "1 3 1"]

-- |Mapa de teste
tabteste11 = ["###########",
              "#   ? ??  #",
              "# # # #?# #",
              "#?  ? ??  #",
              "# # #?#?# #",
              "#??    ? ?#",
              "# # # #?#?#",
              "# ??????  #",
              "# # #?# # #",
              "#      ?  #",
              "###########",
              "+ 7 1",
              "+ 7 6",
              "+ 9 6",
              "! 4 5",
              "! 4 7",
              "* 9 7 2 1 10",
              "* 1 7 3 1 10",
              "0 1 1",
              "1 9 1",
              "2 1 9",
              "3 9 7"]

-- |Mapa de teste
tabteste13 = ["#############",
              "#     ??    #",
              "# #?# # #?# #",
              "# ?  ? ?  ? #",
              "# #?#?#?#?# #",
              "#  ? ????   #",
              "#?# # #?# # #",
              "#         ? #",
              "#?# #?# # #?#",
              "#   ? ?  ?  #",
              "# #?# # # # #",
              "#  ????  ?  #",
              "#############",
              "+ 1 1","+ 2 1","+ 3 1","+ 4 1","+ 5 1","+ 6 1","+ 7 1","+ 8 1","+ 9 1","+ 10 1","+ 11 1","+ 11 2","+ 11 3","+ 11 4","+ 11 5","+ 11 6","+ 11 7","+ 11 8","+ 11 9","+ 11 10","+ 11 11","+ 10 11","+ 9 11","+ 8 11","+ 7 11","+ 6 11","+ 5 11","+ 4 11","+ 3 11","+ 2 11","+ 1 11","+ 1 10","+ 1 9","+ 1 8","+ 1 7","+ 1 6","+ 1 5","+ 1 4","+ 1 3","+ 1 2","+ 2 2","+ 3 2","+ 4 2","+ 5 2","+ 6 2","+ 7 2","+ 8 2","+ 9 2","+ 10 2","+ 10 3","+ 10 4","+ 10 5","+ 10 6","+ 10 7","+ 10 8","+ 10 9","+ 10 10","+ 9 10","+ 8 10","+ 7 10","+ 6 10","+ 5 10","+ 4 10","+ 3 10","+ 2 10","+ 2 9","+ 2 8","+ 2 7","+ 2 6","+ 2 5","+ 2 4","+ 2 3","+ 3 3","+ 4 3","+ 5 3","+ 6 3","+ 7 3","+ 8 3","+ 9 3","+ 9 4","+ 9 5","+ 9 6","+ 9 7","+ 9 8","+ 9 9","+ 8 9","+ 7 9","+ 6 9","+ 5 9","+ 4 9","+ 3 9","+ 3 8","+ 3 7","+ 3 6","+ 3 5","+ 3 4","+ 4 4","+ 5 4","+ 6 4","+ 7 4","+ 8 4","+ 8 5","+ 8 6","+ 8 7","+ 8 8","+ 7 8","+ 6 8","+ 5 8","+ 4 8","+ 4 7","+ 4 6","+ 4 5","+ 5 5","+ 6 5","+ 7 5","+ 7 6","+ 7 7","+ 6 7","+ 5 7","+ 5 6","+ 6 6"]

-- |Mapa de teste
tabteste9_13ticks = ["#########",
                     "#########",
                     "#########",
                     "##    ###",
                     "### # ###",
                     "##    ###",
                     "#########",
                     "#########",
                     "#########",
                     "* 3 4 1 1 10",
                     "0 4 3 +",
                     "2 2 5"]