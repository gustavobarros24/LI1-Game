{- |
Module      : Tarefa1_2021li1g075
Description : Validação de um potencial mapa
Copyright   : Gustavo Manuel Marinho Barros <a100656@alunos.uminho.pt>;
            : Pedro Emanuel Organista Silva <a100745@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g075 (
    validaPotencialMapa,
    validaEspaco,
    validaPorta,
    validasobrep,
    validaCaixa,
    validaChao,
    validaChao'

)where

import LI12122
{-| A função `validaPotencialMapa` verifica uma lista de peças e indica se as mesmas podem ser usadas para construir um mapa.

    Esta função irá utilizar outras 5 funções, nas quais cada uma terá de retribuir o caso apresentado como válido. 

== Exemplo:
>>> validaPotencialMapa [(Bloco, (0,0))]
True
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa ((p,(x,y)):xs) | validasobrep ((p,(x,y)):xs) && validaPorta ((p,(x,y)):xs) && validaEspaco ((p,(x,y)):xs) && validaCaixa ((p,(x,y)):xs) && validaChao' ((p,(x,y)):xs) = True
                                   | otherwise = False

{-| A função `validaEspaco` verifica se a lista de peças proposta não irá construir um mapa completamente cheio, ou seja, com toda a sua área ocupada.

== Exemplo:
>>> validaEspaco [(Bloco, (1,1)), (Bloco, (1,2)), (Bloco,(2,1)]
True
-}
validaEspaco :: [(Peca, Coordenadas)] -> Bool
validaEspaco [] = True
validaEspaco ((p,(x,y)):xs) = if length ((p,(x,y)):xs) < areamapa ((p,(x,y)):xs) then True
                              else False

{-| A função `validaPorta` verifica se a lista de peças proposta terá uma e exatamente uma porta.

== Exemplo:
>>> validaPorta [(Porta, (1,1)), (Bloco, (1,0)]
True
>>> validaPorta [(Porta, (1,1)), (Porta, (2,1)), (Bloco,(3,1))]
False
-}
validaPorta :: [(Peca, Coordenadas)] -> Bool
validaPorta [] = False
validaPorta ((p,(x,y)):xs) | p == Porta = notElem Porta (juntar xs)
                           | p /= Porta = validaPorta xs
                           | otherwise = False

{-| A função `validasobrep` verifica se nenhuma peça de uma lista de peças que será o objeto de estudo, está a sobrepor outra.

== Exemplo:
>>> validasobrep [(Bloco, (1,1)), (Bloco, (1,2))]
True
>>> validasobrep [(Bloco, (1,1)), (Bloco, (1,1))]
False
-}
validasobrep :: [(Peca, Coordenadas)] -> Bool
validasobrep [] = True
validasobrep ((p,(x,y)):xs) | notElem (x,y) (juntar' xs) = validasobrep xs
                            | otherwise = False

{-| A função `validaCaixa` verifica se as caixas na lista de peças, não estão a flutuar, ou seja, se estão em cima de uma caixa ou de um bloco.

== Exemplo:
>>> validaCaixa [(Caixa, (1,1)), (Bloco, (1,0))]
True
>>> validaCaixa [(Caixa, (1,2)), (Bloco, (1,0))]
False
-}
validaCaixa :: [(Peca, Coordenadas)] -> Bool
validaCaixa l = validaCaixa' (juntarcaixas l) (juntarblocoscaixas l) 
    where
        validaCaixa' :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> Bool
        validaCaixa' [] l = True
        validaCaixa' ((p,(x,y)):xs) l
            | elem (Bloco, (x, y+1)) l || elem (Caixa, (x, y+1)) l = validaCaixa' xs l
            | otherwise = False

{-| A função `validaChao` verifica se na lista de peças apresentadas, o chão constituído por "Blocos", está completamente unido, ou seja, sem nenhum espaço pelo qual o `Jogador` possa sair.

== Exemplo:
>>> validaChao [(Bloco, (1,0)), (Bloco, (2,0))]
True
>>> validaChao [(Bloco, (1,0)), (Bloco,(3,0))]
False
-}
validaChao :: [(Peca, Coordenadas)] -> Bool
validaChao [] = False
validaChao ((p,(x,y)):xs) = if verificaCima ((p,(x,y)):xs) == False && verificaLado ((p,(x,y)):xs) == False && verificaBaixo ((p,(x,y)):xs) == False && verificaBlocoEmCima ((p,(x,y)):xs) == False && verificaBlocoEmBaixo ((p,(x,y)):xs) == False  then False 
                            else True

{-| A função `validaChao'` apenas apresenta recursividade à função `validaChao`
-}                            
validaChao' :: [(Peca, Coordenadas)] -> Bool
validaChao' [] = True
validaChao' ((p,(x,y)):xs) = if validaChao ((p,(x,y)):xs) == True then validaChao' xs
                             else False

juntary [] = []
juntary ((p,(x,y)):xs) = (y): juntary xs
juntarx [] = []
juntarx ((p,(x,y)):xs) = (x): juntarx xs
juntarxy [] = []
juntarxy ((p,(x,y)):xs) = (x,y) : juntarxy xs
juntar' [] = []
juntar' ((p,(x,y)):xs) = (x,y):juntar' xs
juntar [] = []
juntar ((p,(x,y)):xs) = (p):juntar xs
juntarblocos [] = []
juntarblocos ((p,(x,y)):xs) = if p == Bloco then (x,y): juntarblocos xs
                              else juntarblocos xs

juntarblocoscaixas [] = []
juntarblocoscaixas ((p,(x,y)):xs) = if p == Bloco || p == Caixa then (p,(x,y)): juntarblocoscaixas xs
                                    else juntarblocoscaixas xs

juntarcaixas [] = []
juntarcaixas ((p,(x,y)):xs) = if p == Caixa then (p,(x,y)): juntarcaixas xs
                                    else juntarcaixas xs

verificaCima [x] = True
verificaCima ((p,(x,y)):xs) = if elem (x+1,y+1) (juntarblocos xs) then True
                              else False
verificaLado [x] = True
verificaLado ((p,(x,y)):xs) = if elem (x+1,y) (juntarblocos xs) then True
                              else False
verificaBaixo [x] = True
verificaBaixo ((p,(x,y)):xs) = if elem (x+1,y-1) (juntarblocos xs) then True
                               else False
verificaBlocoEmCima [x] = True
verificaBlocoEmCima ((p,(x,y)):xs) = if elem (x,y+1) (juntarblocos xs) then True
                                     else False
verificaBlocoEmBaixo [x] = True
verificaBlocoEmBaixo ((p,(x,y)):xs) = if elem (x,y-1) (juntarblocos xs) then True
                                     else False


maximox l = maximum (juntarx (tail l))
maximoy l = maximum (juntary (tail l))
areamapa ((p,(x,y)):xs) = maximoy ((p,(x,y)):xs) * maximox ((p,(x,y)):xs)