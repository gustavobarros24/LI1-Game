{- |
Module      : Tarefa2_2021li1g075
Description : Construção/Desconstrução do mapa
Copyright   : Gustavo Manuel Marinho Barros <a100656@alunos.uminho.pt>;
            : Pedro Emanuel Organista Silva <a100745@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g075 (
    constroiMapalinhas,
    constroiMapacolunas,
    constroiMapa,
    desconstroiMapa,
    desconstroiMapacolunas,
    desconstroiMapalinhas

)where 

import LI12122
import Tarefa1_2021li1g075

maiorX :: [(Peca, Coordenadas)] -> Int -> Int
maiorX [] a = a
maiorX ((_, (x, _)) : xs) a =
  if x > a
    then maiorX xs x
    else maiorX xs a

maiorY :: [(Peca, Coordenadas)] -> Int -> Int
maiorY [] a = a
maiorY ((_, (_, y)) : xs) a =
  if y > a
    then maiorY xs y
    else maiorY xs a

{-| A função `constroiMapalinhas` recebe uma lista de peças com as suas coordenadas, e três inteiros (numero da linha, x inicial, e o numero de x a utilizar) na qual retribui uma lista de peças.

Esta função é utilizada para construir as linhas do mapa.

== Exemplo: 
>>> constroiMapalinhas [(Bloco, (0, 4)), (Bloco, (1, 4)), (Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4))] 4 0 5
[Bloco,Bloco,Bloco,Bloco,Bloco]
-}
constroiMapalinhas :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> [Peca]
constroiMapalinhas _ _ _ 0 = []
constroiMapalinhas pc l currX maiorx
  | (Bloco, (currX, l)) `elem` pc = Bloco : constroiMapalinhas pc l (currX + 1) (maiorx -1)
  | (Porta, (currX, l)) `elem` pc = Porta : constroiMapalinhas pc l (currX + 1) (maiorx -1)
  | (Caixa, (currX, l)) `elem` pc = Caixa : constroiMapalinhas pc l (currX + 1) (maiorx -1)
  | (Vazio, (currX, l)) `elem` pc = Vazio : constroiMapalinhas pc l (currX + 1) (maiorx -1)
  | otherwise = Vazio : constroiMapalinhas pc l (currX + 1) (maiorx -1)



{-| A função `constroiMapacolunas`recebe uma lista de peças com as suas coordenadas, e três inteiros (y inicial, e o numero de x e de y a utilizar) na qual retribui uma lista de peças.

Este função é utilizada para construir as colunas do mapa

== Exemplo:
>>> constroiMapacolunas [(Porta, (1, 0)), (Bloco, (2, 0)), (Bloco, (3, 0)), (Bloco, (4, 0))] 0 6 1
[[Vazio,Porta,Bloco,Bloco,Bloco,Vazio]]
-}
constroiMapacolunas :: [(Peca, Coordenadas)] -> Int -> Int -> Int -> Mapa
constroiMapacolunas _ _ _ 0 = []
constroiMapacolunas pc currY maiorx maiory = constroiMapalinhas pc currY 0 maiorx : constroiMapacolunas pc (currY + 1) maiorx (maiory -1)

{-| A função `constroiMapa` recebe uma listas de peças com coordenadas e fornece um mapa.

A lista de peças antes de ser fornecida terá de ser confirmada pela função `validaPotencialMapa` para ser considerado como válido e só assim ser construído.

Esta função é apenas a aplicação das funções `constroiMapacolunas` e `constroiMapalinhas` em conjunto, que irão formar o mapa.

== Exemplo:
>>> constroiMapa [(Porta,(2,1)) ,(Bloco, (1,1)), (Bloco, (2,0))]
[[Vazio,Vazio,Vazio],[Vazio,Bloco,Porta],[Vazio,Vazio,Bloco]]
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa pc =
  if validaPotencialMapa pc
    then constroiMapacolunas pc 0 x y
    else []
  where
    (x, y) = ((maiorX pc 0) + 1, (maiorY pc 0) + 1)

{-| A função `desconstroiMapa` recebe um mapa e fornece uma lista de peças.

Esta função é a aplicação inversa da função "constroiMapa"

Esta função é apenas a aplicação das funções `desconstroiMapacolunas` e `desconstroiMapalinhas` em conjunto, que irão formar a lista de peças.

== Exemplo:
>>> desconstroiMapa [[Vazio,Vazio,Vazio],[Vazio,Bloco,Porta],[Vazio,Vazio,Bloco]]
[(Porta,(2,1)) ,(Bloco, (1,1)), (Bloco, (2,0))]
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapacolunas mapa 0

{-| A função `desconstroiMapacolunas`recebe um mapa e um inteiros (y da primeira lista) na qual retribui uma lista de peças e coordenadas.

Este função é utilizada para desconstruir as colunas do mapa

== Exemplo:
>>> desconstroiMapacolunas [[Vazio,Porta,Bloco,Bloco,Bloco,Vazio]] 0
[(Porta,(1,0)),(Bloco,(2,0)),(Bloco,(3,0)),(Bloco,(4,0))]
-}
desconstroiMapacolunas :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiMapacolunas [] _ = []
desconstroiMapacolunas [a] currY = desconstroiMapalinhas a currY 0
desconstroiMapacolunas (x : xs) currY = desconstroiMapalinhas x currY 0 ++ desconstroiMapacolunas xs (currY + 1)

{-| A função `desconstroiMapalinhas` recebe um mapa, e dois inteiros (y das peças e o x da primeira peça) na qual retribui uma lista de peças e coordenadas.

Esta função é utilizada para desconstruir as linhas do mapa.

== Exemplo:
>>> desconstroiMapalinhas [Bloco,Bloco,Bloco,Bloco,Bloco] 4 0
[(Bloco,(0,4)),(Bloco,(1,4)),(Bloco,(2,4)),(Bloco,(3,4)),(Bloco,(4,4))]
-}
desconstroiMapalinhas :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiMapalinhas [] _ _ = []
desconstroiMapalinhas (x : xs) currY currX
  | x == Bloco = (Bloco, (currX, currY)) : desconstroiMapalinhas xs currY (currX + 1)
  | x == Caixa = (Caixa, (currX, currY)) : desconstroiMapalinhas xs currY (currX + 1)
  | x == Porta = (Porta, (currX, currY)) : desconstroiMapalinhas xs currY (currX + 1)
  | x == Vazio = desconstroiMapalinhas xs currY (currX + 1)