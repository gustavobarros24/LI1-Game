module Tarefa1_2021li1g075_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g075
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com sobreposição" ~: validaPotencialMapa [(Bloco, (1,1)), (Bloco, (1,1)), (Porta,(1,2))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa cheio" ~: validaPotencialMapa [(Bloco, (0,0)), (Bloco, (1,1))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem portas" ~: validaPotencialMapa [(Porta, (1,2)), (Porta, (2,2))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com buraco" ~: validaPotencialMapa [(Bloco, (1,2)), (Porta,(1,1)), (Bloco, (3,1))] ~=? False
    ]
