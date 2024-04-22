module Tarefa3_2021li1g075_Spec where

import Test.HUnit
import Tarefa3_2021li1g075
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo com Jogador com Caixa" ~: "   \n < \nXXX" ~=? show m2e2
    ]