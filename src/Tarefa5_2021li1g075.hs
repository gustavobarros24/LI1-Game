{- |
Module      : Tarefa5_2021li1gXXX
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main (
  -- * Functions
  -- ** Main
  main,
  window,
  background,
  fr,
  estadoInicial,
  draw,
  moveJogadorTeclas,
  reageTempoGloss,
  desenhaEstadoGloss,
  -- ** Secondaries
  desenhaMenu,
  desenhaJogador,
  desenhaMapa,
  -- * Conclusion
  conclusao
)where

import LI12122
import DataGloss
import Tarefa2_2021li1g075
import Tarefa3_2021li1g075
import Tarefa4_2021li1g075
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import Maps
import Data.Maybe
import System.Exit

type Textures = [(Peca, (Picture))]
type EstadoGloss = (Estado, Textures, PlayerTextures, MenuTextures)
type PlayerTextures = [(Direcao, (Picture))]
type MenuTextures = [(String, (Picture))]

{- | A função window serve para definir as dimensões da janela em que correrá o jogo. 
-}
window :: Display 
window = FullScreen

centerofthemape :: Mapa -> (Int, Int)
centerofthemape (x:xs) = ((length x) `div` 2, (length (x:xs)) `div` 2 )
centerofthemape [] = (0,0) 

comprimento :: Mapa -> Int
comprimento mapa = fst (centerofthemape mapa)

altura :: Mapa -> Int
altura mapa = snd (centerofthemape mapa)

{- | A função background dá a cor ao fundo do jogo.
-}
background :: Color
background = greyN 0.8

{- | A função fr dá os frame rates ao jogo.
-}
fr :: Int
fr = 60

{- | A função estadoInicial é o estado inicial do jogo. 
-}
estadoInicial :: Textures -> PlayerTextures -> MenuTextures -> EstadoGloss
estadoInicial textures playertextures menutextures = (estadoMapainicial, textures, playertextures, menutextures)

{-| A função desenhaMenu é utilizada para representar o menu na window.
-}
desenhaMenu :: EstadoGloss -> Picture
desenhaMenu (Estado (Menu Start) jogo, textures, playertexures, menutextures) = (fromJust . lookup "Start") menutextures 
desenhaMenu (Estado (Menu Quit) jogo, textures, playertexures, menutextures) = (fromJust . lookup "Quit") menutextures 







posicJogadorPorta :: Jogo -> Bool
posicJogadorPorta (Jogo mapa (Jogador (x,y) m l)) = elem (Porta, (x,y)) (desconstroiMapa mapa)

{-| A função reageTempoGloss serve aplicar alterações ao jogo ao longo do tempo, contudo não foi utilizada, pois não foi utilizada uma variável de tempo.
-}
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss _ s = return s

{-| A função moveJogadorTeclas é a que define as teclas e as funções correspondentes a estas que podem ser feitas nos diferentes Estados.
-}
moveJogadorTeclas :: Event -> EstadoGloss -> IO EstadoGloss
moveJogadorTeclas (EventKey (SpecialKey KeyUp) Down _ _) ((Estado (Game lvl) jogo ),textures, playertextures, menutextures) = return ((Estado (Game lvl) (moveJogador jogo Trepar)),textures, playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeyDown) Down _ _) ((Estado (Game lvl) jogo),textures, playertextures, menutextures) = return ((Estado (Game lvl) (moveJogador jogo InterageCaixa)),textures, playertextures, menutextures) 
moveJogadorTeclas (EventKey (SpecialKey KeyLeft) Down _ _) ((Estado (Game lvl) jogo),textures, playertextures, menutextures) = return ((Estado (Game lvl) (moveJogador jogo AndarEsquerda)),textures, playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeyRight) Down _ _) ((Estado (Game lvl) jogo),textures, playertextures, menutextures) = return ((Estado (Game lvl) (moveJogador jogo AndarDireita)),textures, playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeySpace) Down _ _) (Estado (Game Lvl1) jogo, textures, playertextures, menutextures) | posicJogadorPorta jogo = return (Estado (Game Lvl2) m2e1,textures,playertextures, menutextures) 
                                                                                                                              | otherwise = return (Estado (Game Lvl1) jogo,textures,playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeySpace) Down _ _) (Estado (Game Lvl2) jogo, textures, playertextures, menutextures) | posicJogadorPorta jogo = return (Estado (Menu Start) jogo, textures, playertextures, menutextures) 
                                                                                                                              | otherwise = return (Estado (Game Lvl2) jogo,textures,playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeyDown) Down _ _) ((Estado (Menu Start)jogo), textures, playertextures, menutextures) = return ((Estado (Menu Quit) jogo, textures, playertextures, menutextures))
moveJogadorTeclas (EventKey (SpecialKey KeyUp) Down _ _) ((Estado (Menu Quit)jogo), textures, playertextures, menutextures) = return ((Estado (Menu Start) jogo, textures, playertextures, menutextures))
moveJogadorTeclas (EventKey (SpecialKey KeySpace) Down _ _) ((Estado (Menu Start)jogo),textures, playertextures, menutextures) = return ((Estado (Game Lvl1)) jogo, textures, playertextures, menutextures)
moveJogadorTeclas (EventKey (SpecialKey KeySpace) Down _ _) ((Estado (Menu Quit)jogo),textures, playertextures, menutextures) = exitSuccess
moveJogadorTeclas (EventKey (SpecialKey KeyEsc) Down _ _) ((Estado (Game lvl) jogo),textures, playertextures, menutextures) = exitSuccess
moveJogadorTeclas _ s = return s

vazio :: Picture
vazio = Blank

{-| A função desenhaEstadoGloss serve para aplicar e representar Texturas ao jogo.
-}
desenhaEstadoGloss :: EstadoGloss -> Picture 
desenhaEstadoGloss (estado, textures, playertextures, menutextures) = Translate (fromIntegral(comprimento mapa)*(-50)) (fromIntegral(altura mapa)*50) $ Pictures (desenho textures (jogador estado) playertextures)
        where constroiGlMapa textures = (desenhaMapa (altura mapa) (comprimento mapa) mapa textures)
              desenho textures jogador playertextures = constroiGlMapa textures ++ [desenhoJogador jogador playertextures]
              mapa = escolheMapa estado
              desenhoJogador jogador playertextures = posicJogador (desenhaJogador jogador playertextures) (escolheCoordenadas jogador)
              jogador estado = escolheJogador estado

{-| A função desenhaJogador é utilizada para representar o Jogador na window.
-}
desenhaJogador :: Jogador -> PlayerTextures -> Picture
desenhaJogador (Jogador (_,_) direcao _) playertextures = (fromJust . lookup direcao) playertextures

posicJogador :: Picture -> Coordenadas -> Picture
posicJogador jogador (x,y)  = (Translate (fromIntegral(x*50)) (fromIntegral(y*(-50))+10) $ jogador)



{-| A função desenhaMapa é utilizada para representar o Mapa na window, esta usa ainda funções auxiliares que divide o processo em desenhar linhas e colunas, Peca a Peca.
-}
desenhaMapa :: Int -> Int -> Mapa -> Textures -> [Picture]
desenhaMapa x y (m:ms) textures = linha ++ resto
        where linha = desenhaMapalinha x y m textures
              resto = desenhaMapa x (y-50) ms textures
desenhaMapa _ _ _ _ = []

desenhaMapalinha :: Int -> Int -> [Peca] -> Textures -> [Picture]
desenhaMapalinha x y (m:ms) textures = peca : resto
        where peca = desenhaTexturePeca x y m textures
              resto = desenhaMapalinha (x+50) y ms textures
desenhaMapalinha _ _ _ _ = []

desenhaTexturePeca :: Int -> Int -> Peca -> Textures -> Picture
desenhaTexturePeca x y peca textures = Translate (fromIntegral x) (fromIntegral y) texture
        where escolheTexture = (fromJust . lookup peca) textures
              texture = escolheTexture


{-| A função draw serve para desenhar o Estado em que o jogo se encontra.
-}
draw :: EstadoGloss -> IO Picture
draw estado@(Estado (Menu _ ) jogo,textures, playertextures, menutextures) = return $ desenhaMenu estado
draw estado@(Estado (Game lvl) jogo,textures, playertextures,menutextures) = return $ desenhaEstadoGloss estado

{-| As estratégias adotadas surgiram de vários exemplos vistos e fruto de pesquisas ao longo do tempo, uma das maiores dificuldades foi a pesquisa de informação, pois esta é quase escassa. Mas com o tempo e a dedicação necessária foi possível criar um jogo funcional.
    Webgrafia: Vídeos do cesium https://www.youtube.com/playlist?list=PLadvWyx_6w6XiJ95A4MqSfmIaRVbXWFGS
               https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1
               https://blog.jayway.com/2020/11/01/making-a-small-game-with-gloss/
               https://www.youtube.com/watch?v=Ccue0ayHw2Y
               https://andrew.gibiansky.com/blog/haskell/haskell-gloss/
               https://leanpub.com/gameinhaskell
-}
conclusao = undefined


{-| A função principal que produz o jogo.
-}
main :: IO()
main = do 
        Just block <- loadJuicy "textures/Block.png"
        Just door <- loadJuicy "textures/Door.png"
        Just box <- loadJuicy "textures/Box.png"
        Just playerskin2 <- loadJuicy "textures/Player2.png"
        Just playerskin <-loadJuicy "textures/Player2Esq.png"
        Just menu1 <- loadJuicy "textures/start.png"
        Just menu2 <- loadJuicy "textures/quit.png"
        playIO window 
               background
               fr
               (estadoInicial
                  [
                    (Vazio, (vazio)),
                    (Bloco, (block)),
                    (Porta, (door)),
                    (Caixa, (box))
                  ]
                  [
                    (Este, (playerskin)),
                    (Oeste, (playerskin2))
                  ]
                  [
                    ("Start", (menu1)),
                    ("Quit", (menu2))
                  ]
               )
               draw
               moveJogadorTeclas
               reageTempoGloss