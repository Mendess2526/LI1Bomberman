{-|
Module      : Main
Description : Modulo Haskell contendo funções para desenhar o jogo em Gloss.
Copyright   : Pedro Mendes a79003;
              Francisco Reinolds a82982

Um módulo contendo definicoes Haskell, usando o Gloss, concretizar o jogo em si, utilizando todos os outros modulos desenvolvidos até agora.
-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Environment
import Data.Maybe
import Data.Char
import System.Random
import Mapa
import Move
import Avanca
import Bot (bot)

framerate = 3

-- |Uma representação do Estado do Mapa. (MapState,seed ou tamanho do mapa (dependendo do jogo ter começado ou não),ticks que faltam para acabar o jogo,Lista de Bitmaps,0 se o jogo não tiver acabado e 1 se o jogo acabou)
type Estado = (MapState,Int,Int,[Picture],Int,MenuData,Int,Int)
-- |Dados do menu ((Coordenadas da opcção selecionada) e posição,Valor)
type MenuData = (Pos,Int)

-- |Função principal que invoca o jogo.
main :: IO ()
main = do imgList <- loadIMG
          gen <- newStdGen
          play dm                        -- display mode
               black                     -- côr do fundo da janela
               framerate                 -- frame rate
               (estadoInicial (fst (randomR (0,10) gen)) imgList) -- estado inicial
               desenhaEstado             -- desenha o Estado do Mapa
               reageEvento               -- reage a um evento
               reageTempo                -- reage ao passar do tempo

-- |Função que carrega todas as imagens necessarias para o jogo.
loadIMG :: IO [Picture] -- ^ Resultado
loadIMG = do imgGround <- loadJuicy "bitmaps/Ground.bmp"                    --0
             imgWall <- loadJuicy "bitmaps/Wall.bmp"                        --1
             imgBrick <- loadJuicy "bitmaps/Brick.bmp"                      --2
             imgPowerUpFlames <- loadJuicy "bitmaps/PowerUpFlames.bmp"      --3
             imgPowerUpBombs <- loadJuicy "bitmaps/PowerUpBombs.bmp"        --4
             imgPlayer0 <- loadJuicy "bitmaps/Player0.png"                  --5
             imgPlayer1 <- loadJuicy "bitmaps/Player1.png"                  --6
             imgPlayer2 <- loadJuicy "bitmaps/Player2.png"                  --7
             imgPlayer3 <- loadJuicy "bitmaps/Player3.png"                  --8
             imgBomb <- loadJuicy "bitmaps/Bomb.png"                        --9
             imgBombTimer1 <- loadJuicy "bitmaps/BombTimer1.png"            --10
             imgCentralExplosion <- loadJuicy "bitmaps/CentralExplosion.png"--11
             imgPowerUpExploding <- loadJuicy "bitmaps/PowerUpExploding.bmp"--12
             imgBrickExploding <- loadJuicy "bitmaps/BrickExploding.bmp"    --13
             imgExplosionTip <- loadJuicy "bitmaps/ExplosionTip.png"        --14
             imgExplosionMid <- loadJuicy "bitmaps/ExplosionMid.png"        --15
             imgSideBar <- loadJuicy "bitmaps/sidebar.bmp"                  --16
             imgMainMenuPlrNum <- loadJuicy "bitmaps/MainMenuPlrNum.bmp"    --17
             imgMainMenuBot1pl <- loadJuicy "bitmaps/MainMenuBotNum1plr.bmp"--18
             imgMainMenuBot2pl <- loadJuicy "bitmaps/MainMenuBotNum2plr.bmp"--19
             img0 <- loadJuicy "bitmaps/0.png"                              --20
             img1 <- loadJuicy "bitmaps/1.png"                              --21
             img2 <- loadJuicy "bitmaps/2.png"                              --22
             img3 <- loadJuicy "bitmaps/3.png"                              --23
             img4 <- loadJuicy "bitmaps/4.png"                              --24
             img5 <- loadJuicy "bitmaps/5.png"                              --25
             img6 <- loadJuicy "bitmaps/6.png"                              --26
             img7 <- loadJuicy "bitmaps/7.png"                              --27
             img8 <- loadJuicy "bitmaps/8.png"                              --28
             img9 <- loadJuicy "bitmaps/9.png"                              --29
             imgMainMenu <- loadJuicy "bitmaps/MainMenu.bmp"                --30
             return (map fromJust [imgGround,imgWall,imgBrick,imgPowerUpFlames,imgPowerUpBombs,imgPlayer0,imgPlayer1,imgPlayer2,imgPlayer3,imgBomb,imgBombTimer1,imgCentralExplosion,imgPowerUpExploding,imgBrickExploding,imgExplosionTip,imgExplosionMid,imgSideBar,imgMainMenuPlrNum,imgMainMenuBot1pl,imgMainMenuBot2pl,img0,img1,img2,img3,img4,img5,img6,img7,img8,img9,imgMainMenu])

-- |O estado inicial do jogo.
estadoInicial :: Int -- ^ Seed para gerar o mapa aleatorio
              -> [Picture] -- ^ bitmaps para incluir no estado
              -> Estado -- ^ Resultado
estadoInicial seed bitmaps = ([[]],seed,0,bitmaps,-3,((0,0),1),0,0)

-- |Função que desenha o jogo.
desenhaEstado :: Estado -- ^ Estado do Mapa
                 -> Picture -- ^ Resultado
                 -- Menu (number of players)
desenhaEstado (mapstate,seed,ticks,bitmaps,-3,menuData,np,nb) = Pictures [Translate (-100) 0 (bitmaps!!17),menuSelectorPlr menuData]
                 -- Menu (number of bots (1plr))
desenhaEstado (mapstate,seed,ticks,bitmaps,-2,menuData,1,nb) = Pictures [Translate (-100) 0 (bitmaps!!18),menuSelectorBots menuData]
                 -- Menu (number of bots (2plr))
desenhaEstado (mapstate,seed,ticks,bitmaps,-2,menuData,2,nb) = Pictures [Translate (-100) 0 (bitmaps!!19),menuSelectorBots menuData]
                 -- Menu (size)
desenhaEstado (mapstate,seed,ticks,bitmaps,-1,menuData,np,nb) = Pictures [Translate (-100) 0 (last bitmaps),menuSelector menuData]                 
                 -- Game is playing
desenhaEstado (mapstate,size,ticks,bitmaps,0,menuData,np,nb) = Pictures [Translate (-450) 350 (Scale f f (fieldBuilder  mapstate size bitmaps)),
                                                                         Pictures ((Translate 353 0 (bitmaps!!16)) : (drawSideBar (deadPlayers [0,1,2,3] mapstate))),
                                                                         Translate 255 (-300) $ drawClock bitmaps ticks]
                      where
                        f = (700.0/((fromIntegral size)*20.0))
                 -- Game is over
desenhaEstado (mapstate,size,ticks,bitmaps,1,menuData,np,nb) = Pictures [Translate (-450) 350 (Scale f f (fieldBuilder mapstate size bitmaps)),
                                                                         gameOverScreen mapstate,
                                                                         Pictures ((Translate 353 0 (bitmaps!!16)) : (drawSideBar (deadPlayers [0,1,2,3] mapstate))),
                                                                         Translate 255 (-300) $ drawClock bitmaps ticks]
                      where
                        f = (700.0/((fromIntegral size)*20.0))
                 -- Game is paused
desenhaEstado (mapstate,size,ticks,bitmaps,2,menuData,np,nb) = Pictures [Translate (-450) 350 (Scale f f (fieldBuilder mapstate size bitmaps)),
                                                                         pauseScreen,
                                                                         Pictures ((Translate 353 0 (bitmaps!!16)) : (drawSideBar (deadPlayers [0,1,2,3] mapstate))),
                                                                         Translate 255 (-300) $ drawClock bitmaps ticks]
                      where
                        f = (700.0/((fromIntegral size)*20.0))

-- |Função que altera o Estado do Mapa quando acontece um evento.
reageEvento :: Event -- ^ Tecla premida
               -> Estado -- ^ Estado do Mapa
               -> Estado -- ^ Resultado
               -- Menu (plrNum)
reageEvento (EventKey(SpecialKey KeyRight) Down _ _) (g,s,t,b,-3,menuData,np,nb) = (g,s,t,b,-3,setMenuDataPlr menuData 'r',np,nb)
reageEvento (EventKey(SpecialKey KeyLeft) Down _ _) (g,s,t,b,-3,menuData,np,nb)  = (g,s,t,b,-3,setMenuDataPlr menuData 'l',np,nb)
reageEvento (EventKey(SpecialKey KeyEnter) Down _ _) (g,s,t,b,-3,(_,n),np,nb)    = (g,s,t,b,-2,((0,0),0),n,nb)
               -- Menu (botNum)
reageEvento (EventKey(SpecialKey KeyRight) Down _ _) (g,s,t,b,-2,menuData,np,nb) = (g,s,t,b,-2,setMenuDataBot menuData 'r',np,nb)
reageEvento (EventKey(SpecialKey KeyLeft) Down _ _) (g,s,t,b,-2,menuData,np,nb)  = (g,s,t,b,-2,setMenuDataBot menuData 'l',np,nb)
reageEvento (EventKey(SpecialKey KeyEnter) Down _ _) (g,s,t,b,-2,(_,n),np,nb)    = (g,s,t,b,-1,((0,3),5),np,n)
               -- Menu (size)
reageEvento (EventKey(SpecialKey KeyUp) Down _ _) (g,s,t,b,-1,menuData,np,nb)           = (g,s,t,b,-1,setMenuData menuData 'u',np,nb)
reageEvento (EventKey(SpecialKey KeyRight) Down _ _) (g,s,t,b,-1,menuData,np,nb)        = (g,s,t,b,-1,setMenuData menuData 'r',np,nb)
reageEvento (EventKey(SpecialKey KeyDown) Down _ _) (g,s,t,b,-1,menuData,np,nb)         = (g,s,t,b,-1,setMenuData menuData 'd',np,nb)
reageEvento (EventKey(SpecialKey KeyLeft) Down _ _) (g,s,t,b,-1,menuData,np,nb)         = (g,s,t,b,-1,setMenuData menuData 'l',np,nb)
reageEvento (EventKey(SpecialKey KeyEnter) Down _ _) (g,seed,t,b,-1,(index,size),np,nb) = ((mapa size seed)++initializePlayers (size-2) np nb,size,((size-2)^2)*4,b,0,(index,size),np,nb)
               -- Game is playing
reageEvento (EventKey(Char 'w') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 0 'U'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(Char 'd') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 0 'R'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(Char 's') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 0 'D'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(Char 'a') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 0 'L'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(Char 'v') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 0 'B'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(SpecialKey KeyUp) Down _ _) (g,s,t,b,0,menuData,2,nb)     = ((move g 1 'U'),s,t,b,0,menuData,2,nb)
reageEvento (EventKey(SpecialKey KeyRight) Down _ _) (g,s,t,b,0,menuData,2,nb)  = ((move g 1 'R'),s,t,b,0,menuData,2,nb)
reageEvento (EventKey(SpecialKey KeyDown) Down _ _) (g,s,t,b,0,menuData,2,nb)   = ((move g 1 'D'),s,t,b,0,menuData,2,nb)
reageEvento (EventKey(SpecialKey KeyLeft) Down _ _) (g,s,t,b,0,menuData,2,nb)   = ((move g 1 'L'),s,t,b,0,menuData,2,nb)
reageEvento (EventKey(Char '-') Down _ _) (g,s,t,b,0,menuData,np,nb)            = ((move g 1 'B'),s,t,b,0,menuData,np,nb)
reageEvento (EventKey(Char 'p') Down _ _) (g,s,t,b,0,menuData,np,nb)            = (g,s,t,b,2,menuData,np,nb)
               -- Game is paused
reageEvento (EventKey(Char 'p') Down _ _) (g,s,t,b,2,menuData,np,nb) = (g,s,t,b,0,menuData,np,nb)
               -- Game is over
reageEvento _ (g,s,t,b,1,menuData,np,nb) = (g,s,t,b,1,menuData,np,nb)
reageEvento _ s = s

-- |Função que altera o Estado do Mapa quando o tempo avança @n@ segundos.
reageTempo :: Float -- ^ Numero de segundos a avançar (não utilizado 
              -> Estado -- ^ Estado espandido do jogo
              -> Estado -- ^ Resultado
reageTempo f (mapstate,size,ticks,bitmaps,0,menuData,np,nb) |numPlayers mapstate size > 1 = (avanca rBots ticks,size,ticks-1,bitmaps,0,menuData,np,nb)
                                                            |otherwise                    = (mapstate,size,ticks,bitmaps,1,menuData,np,nb)
                where
                  rBots = runBots mapstate np ticks
reageTempo f (mapstate,size,ticks,bitmaps,mode,menuData,np,nb) = (mapstate,size,ticks,bitmaps,mode,menuData,np,nb)

runBots :: MapState -> Int -> Int -> MapState
runBots mapstate np ticks |np == 1   = callBot0
                          |otherwise = callBot1
          where
              callBot0 |isJust bot1 = move callBot1 1 (fromJust bot1)
                       |otherwise   = callBot1
              callBot1 |isJust bot2 = move callBot2 2 (fromJust bot2)
                       |otherwise   = mapstate
              callBot2 |isJust bot3 = move mapstate 3 (fromJust bot3)
                       |otherwise   = mapstate
              bot1 = (bot mapstate 1 ticks)
              bot2 = (bot mapstate 2 ticks)
              bot3 = (bot mapstate 3 ticks)

-- |Função que recebe o Estado do Mapa e o tamanho e devolve o número de jogadores
numPlayers :: MapState -- ^ Estado do Mapa
              -> Int -- ^ Tamanho do mapa
              -> Int -- ^ Resultado
numPlayers [] _ = 0
numPlayers ((h0:hs):t) size |h0=='#' = numPlayers (drop size ((h0:hs):t)) size
                            |h0=='0' ||h0=='1' ||h0=='2' ||h0=='3' = 1 + numPlayers t size
                            |otherwise = numPlayers t size

-- |Função que recebe o Estado do Mapa e chama a função 'gameOverScreenAux' para imprimir no ecra o jogador que ganhou
gameOverScreen :: MapState -- ^ Estado do Mapa
                  -> Picture -- ^ Resultado
gameOverScreen [] = gameOverScreenAux 'a'
gameOverScreen ((h0:hs):t) |isDigit h0 = gameOverScreenAux h0
                           |otherwise = gameOverScreen t

-- |Função que imprime no ecra o jogador que ganhou, ou empate no caso do jogo empatar.
gameOverScreenAux :: Char -- ^ O Jogador que ganhou ou 'a' para um empate
                     -> Picture -- ^ Resultado
gameOverScreenAux '0' = Translate (-355) (-16) (Scale (0.5) (0.5) (Pictures [Color red (Polygon [(-47,122),(1067,122),(1067,-47),(-47,-47)]),
                                                                             Color white (Polygon [(-40,115),(1060,115),(1060,-40),(-40,-40)])
                                                                             ,(text "Red player wins")]))
gameOverScreenAux '1' = Translate (-365) (-16) (Scale (0.5) (0.5) (Pictures [Color blue (Polygon [(-47,122),(1097,122),(1097,-47),(-47,-47)]),
                                                                             Color white (Polygon [(-40,115),(1090,115),(1090,-40),(-40,-40)]),
                                                                             (text "Blue player wins")]))
gameOverScreenAux '2' = Translate (-380) (-16) (Scale (0.5) (0.5) (Pictures [Color green (Polygon [(-47,122),(1177,122),(1177,-47),(-47,-47)]),
                                                                             Color white (Polygon [(-40,115),(1170,115),(1170,-40),(-40,-40)]),
                                                                             (text "Green player wins")]))
gameOverScreenAux '3' = Translate (-390) (-16) (Scale (0.5) (0.5) (Pictures [Color violet (Polygon [(-47,122),(1207,122),(1207,-47),(-47,-47)]),
                                                                             Color white (Polygon [(-40,115),(1200,115),(1200,-40),(-40,-40)]),
                                                                             (text "Purple player wins")]))
gameOverScreenAux 'a' = Translate (-240) (-16) (Scale (0.5) (0.5) (Pictures [Color black (Polygon [(-47,122),(607,122),(607,-47),(-47,-47)]),
                                                                             Color white (Polygon [(-40,115),(600,115),(600,-40),(-40,-40)]),
                                                                             (text "It's a tie")]))

-- | Função que imprime "Pause"
pauseScreen :: Picture -- ^ Resultado
pauseScreen = Translate (-189) (-16) (Scale (0.5) (0.5) (Pictures [Color black (Polygon [(-47,122),(402,122),(402,-47),(-47,-47)]),
                                                                   Color white (Polygon [(-40,115),(395,115),(395,-40),(-40,-40)]),
                                                                   (text "Pause")]))

-- |Função que determina o resultado de cada tecla premida no menu.
setMenuDataPlr :: MenuData -- ^ Coordenadas e valor selecionado no menu
               -> Char -- ^ Tecla pressionada
               -> MenuData -- ^ Resultado
setMenuDataPlr ((x,y),value) 'r' = if (x==1) then ((x,y),value) else ((x+1,y),value+1)
setMenuDataPlr ((x,y),value) 'l' = if (x==0) then ((x,y),value) else ((x-1,y),value-1)

-- |Função que determina o resultado de cada tecla premida no menu.
setMenuDataBot :: MenuData -- ^ Coordenadas e valor selecionado no menu
               -> Char -- ^ Tecla pressionada
               -> MenuData -- ^ Resultado
setMenuDataBot ((x,y),value) 'r' = if (x==2) then ((x,y),value) else ((x+1,y),value+1)
setMenuDataBot ((x,y),value) 'l' = if (x==0) then ((x,y),value) else ((x-1,y),value-1)

-- |Função que determina o resultado de cada tecla premida no menu.
setMenuData :: MenuData -- ^ Coordenadas e valor selecionado no menu
               -> Char -- ^ Tecla pressionada
               -> MenuData -- ^ Resultado
setMenuData ((x,y),value) 'u' = if (y==3) then ((x,y),value) else ((x,y+1),value-2) 
setMenuData ((x,y),value) 'r' = if (x==3) then ((x,y),value) else ((x+1,y),value+8)
setMenuData ((x,y),value) 'd' = if (y==0) then ((x,y),value) else ((x,y-1),value+2)
setMenuData ((x,y),value) 'l' = if (x==0) then ((x,y),value) else ((x-1,y),value-8)

-- |Função que desenha um quadrado a volta do tamanho selecionado.
menuSelector :: MenuData -- ^ Coordenadas e valor selecionado no menu
                -> Picture -- ^ Resultado
menuSelector ((x,y),_) = Translate (fromIntegral (x*98)) (fromIntegral (y*51)) outline
                  where
                    outline = Pictures [
                                        Color blue $ Polygon [(-314.9,-228),(-289,-245),(-314.9,-260)],
                                        Color white $ Line [(-314.9,-229),(-289,-246)],
                                        Color violet $ Line [(-314.9,-230),(-290,-246)],
                                        Color violet $ Line [(-314.9,-231),(-291,-246)],
                                                   lineLoop [(-314.9,-228),(-289,-245),(-314.9,-260)],
                                                   lineLoop [(-315.9,-227),(-288,-245),(-315.9,-261)]
                                       ]

menuSelectorPlr :: MenuData -- ^ Coordenadas e valor selecionado no menu
                   -> Picture -- ^ Resultado
menuSelectorPlr ((x,y),_) = Translate (fromIntegral (x*175)) 0 outline
                  where
                    outline = Pictures [
                                        Color blue $ Polygon [(-260.9,-150),(-235,-167),(-260.9,-182)],
                                        Color white $ Line   [(-260.9,-151),(-235,-168)],
                                        Color violet $ Line  [(-260.9,-152),(-236,-168)],
                                        Color violet $ Line  [(-260.9,-153),(-237,-168)],
                                                   lineLoop  [(-260.9,-150),(-235,-167),(-260.9,-182)],
                                                   lineLoop  [(-261.9,-149),(-234,-167),(-261.9,-183)]
                                       ]

menuSelectorBots :: MenuData -- ^ Coordenadas e valor selecionado no menu
                   -> Picture -- ^ Resultado
menuSelectorBots ((x,y),_) = Translate (fromIntegral (x*127)) 0 outline
                  where
                    outline = Pictures [
                                        Color blue $ Polygon [(-294.9,-150),(-269,-167),(-294.9,-182)],
                                        Color white $ Line   [(-294.9,-151),(-269,-168)],
                                        Color violet $ Line  [(-294.9,-152),(-270,-168)],
                                        Color violet $ Line  [(-294.9,-153),(-271,-168)],
                                                   lineLoop  [(-294.9,-150),(-269,-167),(-294.9,-182)],
                                                   lineLoop  [(-295.9,-149),(-268,-167),(-295.9,-183)]
                                       ]

-- |Função que desenha @X@'s sobre os jogadores que já perderam.orange
drawSideBar :: [Int] -- ^ Lista do jogadore que já perderam
               -> [Picture] -- ^ Resultado
drawSideBar [] = []
drawSideBar (h:t) = case h of
                    0 -> (Translate 333 188 cross) : drawSideBar t
                    1 -> (Translate 333 100 cross) : drawSideBar t
                    2 -> (Translate 333 8 cross) : drawSideBar t
                    3 -> (Translate 333 (-88) cross) : drawSideBar t
            where
              cross = Scale 20 20 (Pictures [
                                             Color black (Line [(-0.1,0),(2,2.1)]),          --upper black /
                                             Color black (Line [(-0.1,-0.05),(2.05,2.1)]),   --upper middle black /
                                             Color black (Line [(-0.1,-0.1),(2.1,2.1)]),     --middle middle black /
                                             Color black (Line [(-0.05,-0.1),(2.1,2.05)]),   --lower middle black /
                                             Color black (Line [(0,-0.1),(2.1,2)]),          --lower black /
                                             Color black (Line [(0,2.1),(2.1,0)]),           --upper black \
                                             Color black (Line [(-0.05,2.1),(2.1,-0.05)]),   --upper middle black \
                                             Color black (Line [(-0.1,2.1),(2.1,-0.1)]),     --middle middle black \
                                             Color black (Line [(-0.1,2.05),(2.05,-0.1)]),   --lower middle black \                                             
                                             Color black (Line [(-0.1,2),(2,-0.1)]),         --lower black \
                                             Color red   (Line [(-0.05,0),(2,2.05)]),        --upper red /
                                             Color red   (Line [(-0.05,-0.05),(2.05,2.05)]), --center red /
                                             Color red   (Line [(0,-0.05),(2.05,2)]),        --lower red /
                                             Color red   (Line [(0,2.05),(2.05,0)]),         --upper red \
                                             Color red   (Line [(-0.05,2),(2,-0.05)]),       --center red \
                                             Color red   (Line [(-0.05,2.05),(2.05,-0.05)])  --lower red \
                                            ])

-- |Função que retorna uma lista de jogadores que já perderam.
deadPlayers :: [Int] -- ^ Lista de Jogadores
               -> MapState -- ^ Estado do mapa
               -> [Int] -- ^ Resultado
deadPlayers [] _ = []
deadPlayers (h:t) mapstate = if ((getPlayerInfo mapstate h) == (0,(0,0),0,0))
                                then h : deadPlayers t mapstate
                                else deadPlayers t mapstate

drawClock :: [Picture] -> Int -> Picture
drawClock bitmaps ticks = Pictures $ Color black (Polygon [(0,0),(192,0),(192,80),(0,80)]) : drawSeconds (show $ div ticks framerate) bitmaps 0 ++ [Color black (Line [(0,9),(192,9)])]

drawSeconds :: String -> [Picture] -> Int -> [Picture]
drawSeconds [] _ _ = []
drawSeconds (h:t) bitmaps acc = Translate (fromIntegral(30+(45*acc))) 40 (bitmaps!!n) : drawSeconds t bitmaps (acc+1)
          where n = (digitToInt h)+20

-- |Display mode
dm :: Display -- ^ Resultado
dm = InWindow "BomberTron" (900,700) (0,0)

-- |Função que verifica se uma string tem apenas um número
isDigitString :: String -- ^ String a verificar
                 -> Bool -- ^ Resultado
isDigitString [] = True
isDigitString (h:t) = if (isDigit h)
                        then isDigitString t
                        else False

-- |Função que inicializa os jogadores. Neste momento inicializa apenas 2, mas pode ser alterada para inicializar até 4.
initializePlayers :: Int -- ^ Tamanho do mapa
                     -> Int -- ^ Numero de Jogadores
                     -> Int -- ^ Numero de Bots
                     -> [String] -- ^ Resultado
initializePlayers size 1 0 = ["0 1 1","1 "++ show size ++ " " ++ show size]
initializePlayers size 1 1 = ["0 1 1","2 1 "++show size,"3 "++ show size ++ " " ++ show size]
initializePlayers size 1 2 = ["0 1 1","1 "++show size++" 1","2 1 "++show size,"3 "++ show size ++ " " ++ show size]
initializePlayers size 2 0 = ["0 1 1","1 "++ show size ++ " " ++ show size]
initializePlayers size 2 1 = ["0 1 1","1 "++show size++" 1","2 1 "++show size]
initializePlayers size 2 2 = ["0 1 1","1 "++show size++" 1","2 1 "++show size,"3 "++ show size ++ " " ++ show size]

-- |Função que recebe o nome de uma imagem e retorna o bitmap correspondente.
getIMG :: String -- ^ Nome da imagem
          -> [Picture] -- ^ Lista de todas as imagens
          -> Picture -- ^ Resultado
getIMG request bitmaps = case request of
                         "imgGround" -> bitmaps !! 0
                         "imgWall" -> bitmaps !! 1
                         "imgBrick" -> bitmaps !! 2
                         "imgPowerUpFlames" -> bitmaps !! 3
                         "imgPowerUpBombs" -> bitmaps !! 4
                         "imgPlayer0" -> bitmaps !! 5
                         "imgPlayer1" -> bitmaps !! 6
                         "imgPlayer2" -> bitmaps !! 7
                         "imgPlayer3" -> bitmaps !! 8
                         "imgBomb" -> bitmaps !! 9
                         "imgBombTimer1" -> bitmaps !! 10
                         "imgCentralExplosion" -> bitmaps !! 11
                         "imgPowerUpExploding" -> bitmaps !! 12
                         "imgBrickExploding" -> bitmaps !! 13
                         "imgExplosionTipUp" -> bitmaps !! 14
                         "imgExplosionTipDown" -> rotate 180 (bitmaps !! 14)
                         "imgExplosionTipLeft" -> rotate (-90) (bitmaps !! 14)
                         "imgExplosionTipRight" -> rotate 90 (bitmaps !! 14)
                         "imgExplosionMidUp" -> bitmaps !! 15
                         "imgExplosionMidDown" -> bitmaps !! 15
                         "imgExplosionMidLeft" -> rotate 90 (bitmaps !! 15)
                         "imgExplosionMidRight" -> rotate 90 (bitmaps !! 15)

{- |Função que evoca as funções que desenham os tres "Layers" do jogo.

==Layers
* Layer1: Chão e paredes do mapa, powerups, bombas e jogadores (ver: 'fieldBuilderLayer1')
* Layer2: Blocos destruiveis (ver: 'fieldBuilderLayer2')
* Layer3: Explosões (ver: 'fieldBuilderLayer3')
-}
fieldBuilder :: MapState -- ^ Estado do Mapa
                -> Int -- ^ Tamanho do Mapa
                -> [Picture] -- ^ Lista de todas as imagens
                -> Picture -- ^ Resultado
fieldBuilder mapstate size bitmaps = Pictures ((fieldBuilderLayer1 mapstate size bitmaps 0 0)++
                                               (fieldBuilderLayer2 mapstate size bitmaps 0 0)++
                                               (fieldBuilderLayer3 gameData mapstate bitmaps size))
                    where
                        gameData = getGameData mapstate size

-- |Função que desenha o chão e paredes do mapa e e evoca a função 'fieldBuilderLayer1Aux' que desenha os powerups, bombas e jogadores.
fieldBuilderLayer1 :: MapState -- ^ Estado do mapa
                      -> Int -- ^ Tamanho do mapa
                      -> [Picture] -- ^ Lista de todas as imagens
                      -> Int -- ^ Coordenada @x@
                      -> Int -- ^ Coordenada @y@
                      -> [Picture] -- ^ Resultado
fieldBuilderLayer1 [] _ _ _ _ = []
fieldBuilderLayer1 ([]:t) size bitmaps x y = fieldBuilderLayer1 t size bitmaps x y
fieldBuilderLayer1 ((h0:hs):t) size bitmaps x y |(y == (size-1)) && (x == size) = fieldBuilderLayer1Aux (map words ((h0:hs):t)) bitmaps size
                                                |(x == size) = fieldBuilderLayer1 ((h0:hs):t) size bitmaps 0 (y+1)
                                                |(h0 == '#') = (Translate xFloat yFloat (getIMG "imgWall" bitmaps)): fieldBuilderLayer1 (hs:t) size bitmaps (x+1) y
                                                |(h0 == ' ') = (Translate xFloat yFloat (getIMG "imgGround" bitmaps)): fieldBuilderLayer1 (hs:t) size bitmaps (x+1) y
                                                |(h0 == '?') = (Translate xFloat yFloat (getIMG "imgGround" bitmaps)): fieldBuilderLayer1 (hs:t) size bitmaps (x+1) y
                                  where xFloat = fromIntegral ((x*20)+10)
                                        yFloat = fromIntegral ((y*(-20))-10)

-- |Função que desenha os powerups, bombas e jogadores.
fieldBuilderLayer1Aux :: [[String]] -- ^ Estado dos powerups, bombas e jogadores
                         -> [Picture] -- ^ Lista de todas as imagens
                         -> Int -- ^ Tamanho do mapa
                         -> [Picture] -- ^ Resultado
fieldBuilderLayer1Aux [[]] _ _ = []
fieldBuilderLayer1Aux ((h0:h1:h2:hs):t) bitmaps size |h0 == "!" = (Translate x y (getIMG "imgPowerUpFlames" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "+" = (Translate x y (getIMG "imgPowerUpBombs" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "*" && timerIs1 hs = (Translate x y (getIMG "imgBombTimer1" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "*" = (Translate x y (getIMG "imgBomb" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "0" = (Translate x y (getIMG "imgPlayer0" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "1" = (Translate x y (getIMG "imgPlayer1" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "2" = (Translate x y (getIMG "imgPlayer2" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                                     |h0 == "3" = (Translate x y (getIMG "imgPlayer3" bitmaps)): fieldBuilderLayer1Aux t bitmaps size
                                    where x = fromIntegral (((read h1 :: Int)*20)+10)
                                          y = fromIntegral (((read h2 :: Int)*(-20))-10)
                                          timerIs1 [h0,h1,h2] = (h2 == "1" || h2 == "0")
fieldBuilderLayer1Aux _ _ _ = []

-- |Função que desenha os blocos destrutiveis.
fieldBuilderLayer2 :: MapState -- ^ Estado do mapa
                      -> Int -- ^ Tamanho do mapa
                      -> [Picture] -- ^ Lista de todas as imagens
                      -> Int -- ^ Coordenada @x@
                      -> Int -- ^ Coordenada @y@
                      -> [Picture] -- ^ Resultado
fieldBuilderLayer2 ([]:t) size bitmaps x y = fieldBuilderLayer2 t size bitmaps x y
fieldBuilderLayer2 ((h0:hs):t) size bitmaps x y |(x == size) && (y == size) = []
                                                |(x == size) = fieldBuilderLayer2 ((h0:hs):t) size bitmaps 0 (y+1)
                                                |h0 == '?' = (Translate xFloat yFloat (getIMG "imgBrick" bitmaps)): fieldBuilderLayer2 (hs:t) size bitmaps (x+1) y
                                                |otherwise = fieldBuilderLayer2 (hs:t) size bitmaps (x+1) y
                                     where xFloat = fromIntegral ((x*20)+10)
                                           yFloat = fromIntegral ((y*(-20))-10)
fieldBuilderLayer2 _ _ _ _ _ = []

-- |Função que desenha as esplosões evocando a função 'explosionDrawer' para cada direção.
fieldBuilderLayer3 :: [[String]] -- ^ Estado do powerups, bombas e jogadores
                      -> MapState -- ^ Estado do mapa
                      -> [Picture] -- ^ lista de todas as imagens
                      -> Int -- ^ Tamanho do mapa
                      -> [Picture] -- ^ Resultado
fieldBuilderLayer3 [[]] _ _ _ = []
fieldBuilderLayer3 ((h0:h1:h2:h3:h4:h5:hs):t) mapstate bitmaps size |h0 == "*" && h5 == "0" = ((Translate xFloat yFloat (getIMG "imgCentralExplosion" bitmaps)):
                                                                                               ((explosionDrawer mapstate bitmaps (x,(y-1)) radius 0 size) ++
                                                                                                (explosionDrawer mapstate bitmaps ((x+1),y) radius 1 size) ++
                                                                                                (explosionDrawer mapstate bitmaps (x,(y+1)) radius 2 size) ++
                                                                                                (explosionDrawer mapstate bitmaps ((x-1),y) radius 3 size))) ++
                                                                                              fieldBuilderLayer3 t mapstate bitmaps size
                                                                    |otherwise = fieldBuilderLayer3 t mapstate bitmaps size
                                                where xFloat = fromIntegral (((read h1 :: Int)*20)+10)
                                                      yFloat = fromIntegral (((read h2 :: Int)*(-20))-10)
                                                      x = read h1 :: Int
                                                      y = read h2 :: Int
                                                      radius = read h4 :: Int
fieldBuilderLayer3 (h:t) mapstate bitmaps size = fieldBuilderLayer3 t mapstate bitmaps size
fieldBuilderLayer3 _ _ _ _ = []

{-|Função que desenha as exlplosões em direções diferentes conforme o valor que receber na variavel @direction@.

* 0 para cima

* 1 para a direita

* 2 para baixo

* 3 para a esquerda

-}
explosionDrawer :: MapState -- ^ Estado do mapa
                   -> [Picture] -- ^ Lista de todas as imagens
                   -> Pos -- ^ Posição onde vai desenhar
                   -> Int -- ^ Raio da bomba
                   -> Int -- ^ Direção para onde esta a desenhar (0 para cima,1 para a direita, 2 para baixo, 3 para a esquerda)
                   -> Int -- ^ Tamanho do mapa
                   -> [Picture] -- ^ Resultado
explosionDrawer mapstate bitmaps (x,y) radius direction size |getPos mapstate (x,y)=='#'             = []
                                                             |getPos mapstate (x,y)=='?'             = [Translate xFloat yFloat $ getIMG "imgBrickExploding" bitmaps]
                                                             |elem (x,y) powerUpList                 = [Translate xFloat yFloat $ getIMG "imgPowerUpExploding" bitmaps]
                                                             |elem (x,y) bombList                    = []
                                                             |elem (x,y) bombList                    = []
                                                             |getPos mapstate (x,y)==' ' && radius==1= [Translate xFloat yFloat finalExplosion]
                                                             |getPos mapstate (x,y)==' ' && radius/=1= (Translate xFloat yFloat midExplosion):explosionDrawer mapstate bitmaps nextPos (radius-1) direction size
              where
                powerUpList = (getBombs mapstate) ++ (getFlames mapstate)
                bombList = map coordinates (getBombState mapstate)
                coordinates ((x,y),_,_,_) = (x,y)
                xFloat = fromIntegral ((x*20)+10)
                yFloat = fromIntegral ((y*(-20))-10)
                finalExplosion = case direction of
                    0 -> (getIMG "imgExplosionTipUp" bitmaps)
                    1 -> (getIMG "imgExplosionTipRight" bitmaps)
                    2 -> (getIMG "imgExplosionTipDown" bitmaps)
                    3 -> (getIMG "imgExplosionTipLeft" bitmaps)
                midExplosion = case direction of
                    0 -> (getIMG "imgExplosionMidUp" bitmaps)
                    1 -> (getIMG "imgExplosionMidRight" bitmaps)
                    2 -> (getIMG "imgExplosionMidDown" bitmaps)
                    3 -> (getIMG "imgExplosionMidLeft" bitmaps)
                nextPos = case direction of
                      0 -> (x,y-1)
                      1 -> (x+1,y)
                      2 -> (x,y+1)
                      3 -> (x-1,y)