module DataGloss where

import LI12122


data Estado = 
    Estado
        EstadoMenu
        Jogo



data EstadoMenu = 
    Menu Opcao | Game Lvl


data Lvl = 
    Lvl1 | Lvl2



data Opcao =
    Start | Quit









escolheMapa (Estado lvl (Jogo mapa jogador) ) = mapa
escolheJogador (Estado lvl (Jogo mapa jogador) ) = jogador
escolheCoordenadas (Jogador coords direc c) = coords