-- |
-- Module      : Tarefa6_2021li1gXXX
-- Description : Resolução de um puzzle
--
-- Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
module Tarefa6_2021li1g075 where
import LI12122
import Tarefa4_2021li1g075
import Data.List

{-| `resolveJogo`: Função que detemrina todas as combinações de Jogos em que o jogador chegou à porta. -}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = let jogosquepassam = filter fimPorta (verificacaoJogos i jogo) in
    if null jogosquepassam
    then Nothing
    else Just (converteJogos (head (sortOn length jogosquepassam)))

{-| `verificacaoJogos`: Função que devolve apenas os jogos em que o jogador chegou á porta dentro do limite de movimentos. -}
verificacaoJogos :: Int -> Jogo -> [[Jogo]]
verificacaoJogos i jogo
    | i <= 0 = [[jogo]]
    | otherwise = [jogo] : map (jogo :) (concatMap (verificacaoJogos (i - 1)) (jogosUteis jogo))

{-| `jogosUteis` Função que vai aplicar todos os movimentos possíveis que o jogador pdoe efetuar, através da função auxiliar que efetua cada movimento e verifica se ele é possível -}
jogosUteis :: Jogo -> [Jogo]
jogosUteis = jogosUteisaux [AndarEsquerda, AndarDireita, Trepar, InterageCaixa]

jogosUteisaux :: [Movimento] -> Jogo -> [Jogo]
jogosUteisaux [] _ = []
jogosUteisaux (x:xs) jogo = let novoJogo = moveJogador jogo x in
        if novoJogo /= jogo
        then novoJogo : jogosUteisaux xs jogo
        else jogosUteisaux xs jogo

{-| A função `fimPorta` junta os jogos em que o jogador acaba na porta, descartando os outros.
-}
fimPorta :: [Jogo] -> Bool
fimPorta jogos = (mapa !! y) !! x == Porta where
    Jogo mapa (Jogador (x,y) _ _) = last jogos

{-| `converteJogos`: Função que transforma os jogos válidos nos movimentos respetivos, utilizando a função auxiliar que determina o movimento efetuado entre cada jogo.
-}
converteJogos :: [Jogo] -> [Movimento]
converteJogos [] = []
converteJogos [_] = []
converteJogos (x:y:t) = converteJogosaux x y : converteJogos (y:t)

converteJogosaux :: Jogo -> Jogo -> Movimento
converteJogosaux (Jogo _ (Jogador (_,y1) _ caixa1)) (Jogo _ (Jogador (_,y2) dir2 caixa2))
        | caixa1 /= caixa2 = InterageCaixa
        | y1 > y2 = Trepar
        | dir2 == Este = AndarDireita
        | otherwise = AndarEsquerda