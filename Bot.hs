{-|
Module      : Tarefa 6
Description : Modulo Haskell contendo funções implementar "inteligencia artificial" no jogo BomberMan
Copyright   : Pedro Mendes a79003;
              Francisco Reinolds a82982

Um módulo contendo definicoes Haskell para o computador tomar decisões quando controla um jogador.
-}
module Bot where

import Data.Maybe
import Data.List
import System.Random
import Move
import Avanca

-- | (direction,x,y) direction(up:0;right:1;down:2;left:3)
type Direction = (Int,Pos)

-- |Função que junta todas as outras funções do modulo para para tomar as decisões.
bot :: MapState -- ^ Estado do Mapa
       -> Int -- ^ Número do jogador que o bot controla
       -> Int -- ^ Número de ticks que faltam para o jogo acabar
       -> Maybe Char -- ^ Resultado
bot mapstate player ticks |currentPosDangerous = escape'
                          |isOnCenter = listToMaybe $ catMaybes 
                                           [
                                            Just 'B' >>= nextToBrickWall' >>= notAllDirectionsDangerous >>= canPlaceBombs,
                                            exploitCenter'>>= mnot nextToBrickWall'
                                           ]
                          |isNotLateGame = listToMaybe $ catMaybes
                                           [
                                             (pathFind powerupPath) >>= canGoToPowerup,
                                             Just 'B' >>= nextToBrickWall' >>= notAllDirectionsDangerous >>= canPlaceBombs,
                                             Just 'B' >>= isOnPlayer >>= canPlaceBombs,
                                             (pathFind playerPath) >>= mnot nextToBrickWall' >>= canGoToPlayer,
                                             goToCenter >>= mnot nextToBrickWall'
                                           ]
                          |otherwise = goToCenter
            where
                --Dados
                playerAndPos                = playerPosAux (getPlayerInfo mapstate player)
                playerPosAux (n,(x,y),_,_)  = (n,(x,y))
                playerPos                   = snd playerAndPos
                bBuffs                      = let (n,(x,y),b,f) = (getPlayerInfo mapstate player) in b
                gameData                    = getGameData mapstate size
                powerupPath                 = goTo mapstate playerAndPos (\(n,(x,y)) -> isPowerUp gameData (x,y)) [playerAndPos] 0 timeWall dangerousBombs
                playerPath                  = goTo mapstate playerAndPos (\(n,(x,y)) -> isPlayer gameData (n,(x,y))) [playerAndPos] 0 timeWall dangerousBombs
                centerPath                  = goTo mapstate playerAndPos (\(n,(x,y)) -> (x,y) == (hSize,hSize)) [playerAndPos] 0 timeWall dangerousBombs
                lCenterPath                 = goTo mapstate playerAndPos (\(n,(x,y)) -> (x,y) == (hSize-1,hSize)) [playerAndPos] 0 timeWall dangerousBombs
                size                        = getSize mapstate
                bPlaced (n,(_,_))           = foldr (+) 0 (filter (==n) (map (\((_,_),n,_,_) -> n) (getBombState mapstate)))
                dangerousBombs              = dangerBombs mapstate
                timeWall                    |isNotLateGame = (-1,-1)
                                            |otherwise     = let s = size-2 in getTimeWallPos ((s^2)-ticks) 1 1 s s (1,1)
                mapHasCenterWall            = mod hSize 2==0
                hSize                       = div size 2
                mid                         = div (pred size) 2
                center                      |mapHasCenterWall = [(mid-1,mid)]--,(mid-1,mid+1),(mid,mid+1)]
                                            |otherwise        = [(mid,mid),(mid,mid-1)]
                aboveCenterSafe             = isJust $ isZoneSafe mapstate (last center) dangerousBombs timeWall
                --Pontos de Decisão
                currentPosDangerous         = isNothing $ isZoneSafe mapstate playerPos dangerousBombs timeWall
                isNotLateGame               = ticks>((length (head mapstate))-2)^2
                isOnCenter                  = elem playerPos center
                notAllDirectionsDangerous x |length (safeRoutes mapstate playerPos timeWall) /= 0 = Just x
                                            |otherwise = Nothing
                nextToBrickWall' x          = nextToBrickWall mapstate playerPos x
                canGoToPowerup x            |snd powerupPath > 99998 = Nothing
                                            |otherwise = Just x
                canPlaceBombs x             |(bBuffs >= bPlaced playerAndPos) && (not $ isBomb gameData playerPos) = Just x
                                            |otherwise = Nothing
                canGoToPlayer x             |snd playerPath > 99998 = Nothing
                                            |otherwise = Just x
                goToCenter                  |mapHasCenterWall = if snd lCenterPath < 99998
                                                                   then pathFind lCenterPath
                                                                   else approachCenter (hSize-1) playerPos (safeRoutes mapstate playerPos timeWall)
                                            |otherwise        = if snd centerPath < 99998
                                                                   then pathFind centerPath
                                                                   else approachCenter hSize playerPos (safeRoutes mapstate playerPos timeWall)
                isOnPlayer x                |isPlayer gameData playerAndPos = Just x
                                            |otherwise = Nothing
                --Other
                escape'                     = escape mapstate playerAndPos timeWall
                exploitCenter'              |mapHasCenterWall = Nothing
                                            |otherwise        = exploitCenter playerPos center timeWall aboveCenterSafe
                pathFind x                  = case (fst x) of 
                                              0 -> Just 'U'
                                              1 -> Just 'R'
                                              2 -> Just 'D'
                                              3 -> Just 'L'

-- |Função que troca um Just para um Nothing e um Nothing para um Just.
mnot :: (Char -> Maybe Char) -- ^ Função que retorna um Maybe Char
        -> Char -- ^ Char para colocar no caso de converter de Nothing para Just
        -> Maybe Char -- ^ Resultado
mnot f x | isJust (f x) = Nothing
         | otherwise = Just x

{-|Função que analisa a circunstancias em que o bot está e com isso escolhe a melhor opção para fugir. Esta função assume que o bot está numa posição perigosa.

==Parametros de decisão:
* Todas as posições imediatamente ao lado são perigosas: 'findFastestWay'
* Pelo menos uma das posições imediatamente ao lado não são perigosas: 'findSafeWay'
-}
escape :: MapState -- ^ Estado do Mapa
          -> (Int,Pos) -- ^ Posição do Jogador
          -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
          -> Maybe Char -- ^ Resultado
escape mapstate (n,pos) timeWall |allDirectionsDangerous = if snd fastestWay > 99998
                                                              then desperationTactic mapstate pos timeWall
                                                              else case fst fastestWay of
                                                                        0 -> Just 'U'
                                                                        1 -> Just 'R'
                                                                        2 -> Just 'D'
                                                                        3 -> Just 'L'
                                 |otherwise = findSafeWay mapstate (reverse (safeRoutes mapstate pos timeWall)) timeWall
                where
                  allDirectionsDangerous = length (safeRoutes mapstate pos timeWall) == 0
                  fastestWay = findFastestWay mapstate (n,pos) (\(n,(x,y)) -> isJust $ isZoneSafe mapstate (x,y) (dangerBombs mapstate) timeWall) [(n,pos)] 0 timeWall

-- |Função chamada apenas quando não existe nenhum caminho seguro para fugir, numa ultima tentativa de sobrevivência.
desperationTactic :: MapState -- ^ Estado do Mapa
                     -> Pos -- ^ Posição do Jogador
                     -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
                     -> Maybe Char -- ^ Resultado
desperationTactic mapstate (x,y) timeWall |isNotWall up    = Just 'U'
                                          |isNotWall right = Just 'R'
                                          |isNotWall left  = Just 'L'
                                          |isNotWall down  = Just 'D'
                                          |otherwise       = Nothing
                        where
                          isNotWall x = x /= '#' && x /= '?'
                          up = getPos mapstate (x,y-1)
                          right = getPos mapstate (x+1,y)
                          left = getPos mapstate (x-1,y)
                          down = getPos mapstate (x,y+1)

{-|Função que recebe um par de coordenadas e encontra o melhor caminho para chegar a essas coordenadas

Esta função retorna um par de inteiros em que o primeiro @Int@ é a direção escolhida (0 para cima;1 para a direita;3 para baixo;4 para a esquerda) e o segundo é o número de passos.

Esta função é praticamente igual à função 'goTo', mudando apenas a primeira guarda.
-}
findFastestWay :: MapState -- ^ Estado do Mapa
        -> (Int,Pos) -- ^ Posição do Bot
        -> ((Int,Pos) -> Bool) -- ^ Posição alvo
        -> [(Int,Pos)] -- ^ Coordenadas onde o algoritemo já esteve (evita ciclos infinitos)
        -> Int -- ^ Contador de passos
        -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
        -> (Int,Int) -- ^ Resultado
findFastestWay _ _ _ _ 7 _ = (0,99999)
findFastestWay mapstate (n,(x,y)) target stepedOn stepCounter timeWall
    | currentPos == '#' || currentPos == '?' = ( 0, 99999)
    | target (n,(x,y))                       = ( 0, stepCounter)
    | otherwise                              = ( 3-(fromJust $ getBest adjPos), stepCounter + getBestsCounter adjPos )
  where
    currentPos = getPos mapstate (x,y)
    u'      = (n,(x,y-1))
    r'      = (n,(x+1,y))
    d'      = (n,(x,y+1))
    l'      = (n,(x-1,y))
    adjPos  = [left,down,right,up]
    
    step'   = stepCounter + 1
    up      = if elem u' stepedOn
                then (0,99999) 
                else findFastestWay mapstate u' target (u':stepedOn) step' timeWall
    right   = if elem r' stepedOn 
                then (0,99999) 
                else findFastestWay mapstate r' target (r':stepedOn) step' timeWall
    down    = if elem d' stepedOn 
                then (0,99999)
                else findFastestWay mapstate d' target (d':stepedOn) step' timeWall
    left    = if elem l' stepedOn 
                then (0,99999)
                else findFastestWay mapstate l' target (l':stepedOn) step' timeWall
                
    getBest         l = let l' = map snd l
                        in elemIndex (minimum l') l'
    getBestsCounter l = minimum $ map snd l

-- |Função que escolhe entre as direções seguras a melhor a tomar.
findSafeWay :: MapState -- ^ Estado do Mapa
               -> [Direction] -- ^ Lista de direções possiveis
               -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
               -> Maybe Char -- ^ Resultado
findSafeWay _ [(d,(x,y))] _ = case d of 
                              0 -> Just 'U'
                              1 -> Just 'R'
                              2 -> Just 'D'
                              3 -> Just 'L'
findSafeWay mapstate ((d,(x,y)):t) timeWall |d==0 && safe (x,y-1) = Just 'U'
                                            |d==1 && safe (x+1,y) = Just 'R'
                                            |d==2 && safe (x,y+1) = Just 'D'
                                            |d==3 && safe (x-1,y) = Just 'L'
                                            |otherwise = findSafeWay mapstate t timeWall
                      where
                        safe pos = (length (safeRoutes mapstate pos timeWall) /= 0)

-- |Função que verifica se o jogador está encostado a uma parede.
nextToBrickWall :: MapState-- ^ Estado do Mapa
                   -> Pos -- ^ Posição
                   -> Char
                   -> Maybe Char -- ^ Resultado
nextToBrickWall mapstate (x,y) b |(getPos mapstate (x+1,y))=='?'= Just b
                                 |(getPos mapstate (x-1,y))=='?'= Just b
                                 |(getPos mapstate (x,y+1))=='?'= Just b
                                 |(getPos mapstate (x,y-1))=='?'= Just b
                                 |otherwise=Nothing

-- |Função que verifica se uma certa posição tem um powerup.
isPowerUp :: [[String]] -- ^ Estado dos powerups, bombas e jogadores
             -> Pos -- ^ Coordenadas
             -> Bool -- ^ Resultado
isPowerUp [[]] _ = False
isPowerUp ((h0:h1:h2:ht):t) (x,y) |(h0=="!" || h0=="+") && (show x)==h1 && (show y)==h2 = True
                                  |otherwise=isPowerUp t (x,y)
isPowerUp _ _ = False

-- |Função que verifica se uma certa posição é um jogador.
isPlayer :: [[String]] -- ^ Estado dos powerups, bombas e jogadores
            -> (Int,Pos) -- ^ Número e posição do Jogador
            -> Bool -- ^ Resultado
isPlayer [[]] _ = False
isPlayer ((h0:h1:h2:ht):t) (n,(x,y)) |h0/="!" && h0/="+" && h0/=(show n) && (show x)==h1 && (show y)==h2 = True
                                     |otherwise=isPlayer t (n,(x,y))
isPlayer _ _ = False

-- |Função que verifica se uma certa posição tem uma bomba.
isBomb :: [[String]] -- ^ Estado dos powerups, bombas e jogadores
          -> Pos -- ^ Coordenadas
          -> Bool -- ^ Resultado
isBomb [[]] _ = False
isBomb ((h0:h1:h2:ht):t) (x,y) |h0=="*" && (show x)==h1 && (show y)==h2 = True
                               |otherwise=isBomb t (x,y)
isBomb _ _ = False

-- |Função que determina das quatro direções que o bot pode seguir quais são seguras.
safeRoutes :: MapState -- ^ Estado do Mapa
              -> Pos -- ^ Posição em questão
              -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
              -> [Direction] -- ^ Resultado
safeRoutes mapstate (x,y) timeWall = catMaybes $ isz 0 (x,y-1):
                                                 isz 1 (x+1,y):
                                                 isz 2 (x,y+1):
                                                 isz 3 (x-1,y):
                                                 []
            where
              bombs = dangerBombs mapstate
              isz n (x,y) = fmap (\a -> (n,(x,y))) $ isZoneSafe mapstate (x,y) bombs timeWall

-- |Função que recebe o Estado do Mapa e devolve uma lista das bombas consideradas perigosas (Tempo até explosão < 6)
dangerBombs :: MapState -- ^ Estado do Mapa
             -> Bombs -- ^ Resultado
dangerBombs [] = []
dangerBombs mapstate = catMaybes $ map bombStats bombs
            where
              bombFilter = filter (\(h0:hs) -> h0=='*') mapstate
              bombs = map words bombFilter
              bombStats (h0:h1:h2:h3:h4:hs) = Just (read h4 ::Int,read h1 ::Int,read h2 ::Int)

-- |Função que determina se uma certa posição é segura, ou seja, não está em alacance de uma bomba, não é onde a parede do fim do jogo vai estar, não é uma parede.
isZoneSafe :: MapState -- ^ Estado do Mapa
              -> Pos -- ^ Posição do bot
              -> Bombs -- ^ Bombas que apresentam perigo para o bot
              -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
              -> Maybe Pos -- ^ Resultado
isZoneSafe mapstate (xp,yp) [] timeWall |(xp,yp) == timeWall = Nothing
                                        |pos=='#'||pos=='?'= Nothing
                                        |otherwise = Just (xp,yp)
                  where
                        pos = getPos mapstate (xp,yp)
isZoneSafe mapstate (xp,yp) ((r,xb,yb):t) timeWall |xp==xb && (abs(yb-yp)<=r) = Nothing
                                                   |yp==yb && (abs(xb-xp)<=r) = Nothing
                                                   |otherwise=isZoneSafe mapstate (xp,yp) t timeWall

-- |Função que permite ao bot se ir aproximando ao centro do mapa, até a função 'goTo' conseguir encontrar um caminho direto.
approachCenter :: Int -- ^ Metado do tamanho do mapa
                  -> Pos -- ^ Posição do bot
                  -> [Direction] -- ^ Direções seguras para seguir
                  -> Maybe Char -- ^ Resultado
approachCenter _ _ [] = Nothing
approachCenter hSize (x,y) ((d,(_,_)):t) 
    |y>hSize && d==0 = Just 'U'
    |x<hSize && d==1 = Just 'R'
    |y<hSize && d==2 = Just 'D'
    |x>hSize && d==3 = Just 'L'
    |otherwise = approachCenter hSize (x,y) t

-- |Função que efetua a estrategia a praticar quando no centro do mapa
exploitCenter :: Pos -- ^ Posição do bot
                 -> [Pos] -- ^ Lista com as posições do centro e acima do centro
                 -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
                 -> Bool -- ^ Segurança do centro
                 -> Maybe Char -- ^ Resultado
exploitCenter pos [center,abovecenter] timeWall safe |pos==center && abovecenter/=timeWall && safe = Just 'U'
                                                     |pos==abovecenter = Just 'B'
                                                     |otherwise = Nothing


{-|Função que recebe um par de coordenadas e encontra o melhor caminho para chegar a essas coordenadas

Esta função retorna um par de inteiros em que o primeiro @Int@ é a direção escolhida e o segundo é o número de passos.

==Direção:
* 0 para cima
* 1 para a direita
* 2 para baixo
* 3 para a esquerda
-}
goTo :: MapState -- ^ Estado do Mapa
        -> (Int,Pos) -- ^ Posição do Bot
        -> ((Int,Pos) -> Bool) -- ^ Posição alvo
        -> [(Int,Pos)] -- ^ Coordenadas onde o algoritemo já esteve (evita ciclos infinitos)
        -> Int -- ^ Contador de passos
        -> Pos -- ^ Posições onde a parede que cai no fim de jogo vai estar no proximo tick
        -> Bombs -- ^ Lista de bombas a ter em conta
        -> (Int,Int) -- ^ Resultado
goTo _ _ _ _ 7 _ _ = (0,99999)
goTo mapstate (n,(x,y)) target stepedOn stepCounter timeWall bombs
    | isNothing $ isZoneSafe mapstate (x,y) bombs timeWall = ( 0, 99999)
    | target (n,(x,y))                                                      = ( 0, stepCounter)
    | otherwise                                                             = ( 3-(fromJust $ getBest adjPos), stepCounter + getBestsCounter adjPos )
  where
    currentPos = getPos mapstate (x,y)
    u'      = (n,(x,y-1))
    r'      = (n,(x+1,y))
    d'      = (n,(x,y+1))
    l'      = (n,(x-1,y))
    adjPos  = [left,down,right,up]
    
    step'   = stepCounter + 1
    up      = if elem u' stepedOn
                then (0,99999) 
                else goTo mapstate u' target (u':stepedOn) step' timeWall bombs
    right   = if elem r' stepedOn 
                then (0,99999) 
                else goTo mapstate r' target (r':stepedOn) step' timeWall bombs
    down    = if elem d' stepedOn 
                then (0,99999)
                else goTo mapstate d' target (d':stepedOn) step' timeWall bombs
    left    = if elem l' stepedOn 
                then (0,99999)
                else goTo mapstate l' target (l':stepedOn) step' timeWall bombs
                
    getBest         l = let l' = map snd l
                        in elemIndex (minimum l') l'
    getBestsCounter l = minimum $ map snd l