module Tarefa6_2021li1g075_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g075
import Fixtures



m8 :: Jogo
m8 = (Jogo m1g (Jogador (5,3) Este False))



testsT6 =
  test
    [ "Tarefa 6 - Teste Achar Caminho com Caixa" ~: Just [AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda] ~=? resolveJogo 4 m8 ]
    