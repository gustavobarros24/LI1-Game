{- |
Module      : Tarefa3_2021li1g075
Description : Representação textual do jogo
Copyright   : Gustavo Manuel Marinho Barros <a100656@alunos.uminho.pt>;
            : Pedro Emanuel Organista Silva <a100745@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g075 (
  convertertexto

)where



import LI12122

instance Show Jogo where
  show = convertertexto


textolinhas :: [Peca] -> Int -> Int -> Coordenadas -> Direcao -> String
textolinhas [] _ _ _ _ = []
textolinhas (x:xs) l currX coord dir | (currX,l) == coord = if dir == Oeste then "<" ++ textolinhas xs l (currX + 1) coord dir else ">" ++ textolinhas xs l (currX + 1) coord dir
                                     | x == Bloco = "X" ++ textolinhas xs l (currX + 1) coord dir
                                     | x == Porta = "P" ++ textolinhas xs l (currX + 1) coord dir
                                     | x == Caixa = "C" ++ textolinhas xs l (currX + 1) coord dir
                                     | x == Vazio = " " ++ textolinhas xs l (currX + 1) coord dir

textocolunas :: Mapa -> Int -> Coordenadas -> Direcao -> String
textocolunas [a] currY coord dir = textolinhas a currY 0 coord dir
textocolunas (x:xs) currY coord dir = textolinhas x currY 0 coord dir ++ "\n" ++ textocolunas xs (currY + 1) coord dir

{-| A função convertertexto serve transformar um mapa, e demonstrar a posição do jogador no mapa, em algo mais gráfico, transformando as peças e o jogador em símbolos.

Esta função utiliza duas funções auxiliares, que transformam o mapa e o jogador nesses simbolos, e os organizam em linhas e colunas, em forma de string.

== Exemplo:
>>>convertertexto (Jogo [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Bloco,Bloco,Bloco]] (Jogador (1,1) Este False))
"   
 > 
XXX"
-}
convertertexto :: Jogo -> String
convertertexto (Jogo mapa (Jogador coord dir _)) = textocolunas mapa 0 coord dir
