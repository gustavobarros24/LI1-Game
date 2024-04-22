module Tarefa4_2021li1g075
  ( moveJogador,
    correrMovimentos,
    pegarCaixa,
    largarCaixa,
    moveJogadorEsq,
    moveJogadorDir,
    moveJogadorDirCaixa,
    moveJogadorEsqCaixa,
    meterCaixa,
    moveJogadorCair,
    moveJogadorTrepar,
    moveJogadorTreparCaixa,
    tirarCaixa,
  )
where

import LI12122

-- | A função `moveJogador` recebe um `Jogo` e um `Movimento` retribuindo assim o jogo atualizado, ou seja depois do `Jogador` se ter movido.
--
-- Esta função utiliza outras funções auxiliares que tanto aplicam a mudança no mapa, como aplicam o movimento ao `Jogador`.
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa (Jogador (x, y) dir caixa)) movimento = case movimento of
  AndarEsquerda ->
    if caixa
      then moveJogadorEsqCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else moveJogadorEsq (Jogo mapa (Jogador (x, y) dir caixa))
  AndarDireita ->
    if caixa
      then moveJogadorDirCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else moveJogadorDir (Jogo mapa (Jogador (x, y) dir caixa))
  Trepar ->
    if caixa
      then moveJogadorTreparCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else moveJogadorTrepar (Jogo mapa (Jogador (x, y) dir caixa))
  InterageCaixa ->
    if caixa
      then largarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else pegarCaixa (Jogo mapa (Jogador (x, y) dir caixa))

-- | A função `pegarCaixa` recebe um `Jogo` e retribui um `Jogo` sem a `Caixa` no mapa mas sim com o `Jogador`.
--
-- Esta função utiliza outras funções auxiliares que removem a `Caixa` do mapa e alteram o `Bool` do `Jogador`, desta forma colocando a `Caixa` no `Jogador`
pegarCaixa :: Jogo -> Jogo
pegarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este =
    if mapa !! y !! (x + 1) == Caixa && mapa !! (y -1) !! x /= Bloco
      then tirarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Oeste =
    if mapa !! y !! (x -1) == Caixa && mapa !! (y -1) !! x /= Bloco
      then tirarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else (Jogo mapa (Jogador (x, y) dir caixa))

-- | A função `largar` recebe um `Jogo` e retribui um `Jogo` com a `Caixa` no mapa e não com o `Jogador`.
--
-- Esta função utiliza outras funções auxiliares que colocam a `Caixa` no mapa e alteram o `Bool` do jogador, desta forma retirando a `Caixa` do `Jogador`.
largarCaixa :: Jogo -> Jogo
largarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este =
    if mapa !! y !! (x + 1) == Vazio
      then meterCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Oeste =
    if mapa !! y !! (x -1) == Vazio
      then meterCaixa (Jogo mapa (Jogador (x, y) dir caixa))
      else (Jogo mapa (Jogador (x, y) dir caixa))

-- | A função `tirarCaixa` é a função auxiliar utilizada em outras funções para tirar a `Caixa` do `Jogo` e ainda mudar o Bool do `Jogador`.
--
-- Esta função usa ainda funções auxiliares para retirar a `Caixa` do `Mapa`.
tirarCaixa :: Jogo -> Jogo
tirarCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este = (Jogo (tirarCaixacoluna mapa (x + 1, y)) (Jogador (x, y) dir True))
  | dir == Oeste = (Jogo (tirarCaixacoluna mapa (x - 1, y)) (Jogador (x, y) dir True))

tirarCaixacoluna :: Mapa -> Coordenadas -> Mapa
tirarCaixacoluna [] _ = []
tirarCaixacoluna (h : t) (x, y) =
  if y == 0
    then tiraCaixalinha h x : t
    else h : tirarCaixacoluna t (x, y -1)

tiraCaixalinha :: [Peca] -> Int -> [Peca]
tiraCaixalinha [] _ = []
tiraCaixalinha (x : xs) n
  | n == 0 = Vazio : xs
  | otherwise = x : tiraCaixalinha xs (n -1)

-- | A função `meterCaixa` é a função auxiliar utilizada em outras funções para colocar a `Caixa` no `Jogo` e ainda mudar o Bool do `Jogador`.
--
-- Esta função usa ainda funções auxiliares para colocar a `Caixa` do `Mapa`.
meterCaixa :: Jogo -> Jogo
meterCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este = (Jogo (meterCaixacoluna mapa (caixaCair mapa (x+1,y))) (Jogador (x, y) dir False))
  | dir == Oeste = (Jogo (meterCaixacoluna mapa (caixaCair mapa (x-1,y))) (Jogador (x, y) dir False))

meterCaixacoluna :: Mapa -> Coordenadas -> Mapa
meterCaixacoluna [] _ = []
meterCaixacoluna (h : t) (x, y) =
  if y == 0
    then meterCaixalinha h x : t
    else h : meterCaixacoluna t (x, y -1)

meterCaixalinha :: [Peca] -> Int -> [Peca]
meterCaixalinha [] _ = []
meterCaixalinha (x : xs) n
  | n == 0 = Caixa : xs
  | otherwise = x : meterCaixalinha xs (n -1)

-- | A função `moveJogadorTrepar` aplica o movimento de subida ao `Jogador` através da alteração da posição do `Jogador` no `Mapa`.
moveJogadorTrepar :: Jogo -> Jogo
moveJogadorTrepar (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este =
    if mapa !! y !! (x + 1) == Bloco && mapa !! (y - 1) !! (x + 1) /= Caixa && mapa !! (y - 1) !! (x + 1) /= Bloco 
      then Jogo mapa (Jogador ((x + 1), (y -1)) dir caixa)
      else
        if mapa !! y !! (x + 1) == Caixa && mapa !! (y - 1) !! (x + 1) /= Caixa && mapa !! (y - 1) !! (x + 1) /= Bloco 
          then Jogo mapa (Jogador ((x + 1), (y -1)) dir caixa)
          else Jogo mapa (Jogador (x, y) dir caixa)
  | dir == Oeste =
    if mapa !! y !! (x -1) == Bloco && mapa !! (y - 1) !! (x - 1) /= Caixa && mapa !! (y - 1) !! (x - 1) /= Bloco 
      then Jogo mapa (Jogador ((x -1), (y -1)) dir caixa)
      else
        if mapa !! y !! (x -1) == Caixa && mapa !! (y - 1) !! (x - 1) /= Caixa && mapa !! (y - 1) !! (x - 1) /= Bloco 
          then Jogo mapa (Jogador ((x -1), (y -1)) dir caixa)
          else Jogo mapa (Jogador (x, y) dir caixa)

-- | A função `moveJogadorTreparCaixa` tem o mesmo objetivo da função `moveJogadorTrepar` mas é usada quando o `Jogador` carrega uma `Caixa`.
moveJogadorTreparCaixa :: Jogo -> Jogo
moveJogadorTreparCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | dir == Este =
    if mapa !! (y -2) !! (x + 1) == Bloco || mapa !! (y - 1) !! (x + 1) == Caixa || mapa !! (y - 1) !! (x + 1) == Bloco 
      then Jogo mapa (Jogador (x, y) dir caixa)
      else
        if mapa !! y !! (x + 1) == Bloco
          then Jogo mapa (Jogador ((x + 1), (y -1)) dir caixa)
          else
            if mapa !! y !! (x + 1) == Caixa
              then Jogo mapa (Jogador ((x + 1), (y -1)) dir caixa)
              else Jogo mapa (Jogador (x, y) dir caixa)
  | dir == Oeste =
    if mapa !! (y -2) !! (x -1) == Bloco || mapa !! (y - 1) !! (x - 1) == Caixa || mapa !! (y - 1) !! (x - 1) == Bloco 
      then Jogo mapa (Jogador (x, y) dir caixa)
      else
        if mapa !! y !! (x -1) == Bloco
          then Jogo mapa (Jogador ((x -1), (y -1)) dir caixa)
          else
            if mapa !! y !! (x -1) == Caixa
              then Jogo mapa (Jogador ((x -1), (y -1)) dir caixa)
              else Jogo mapa (Jogador (x, y) dir caixa)

-- | A função `moveJogadorEsq` recebe um `Jogo` e retribui um `Jogo` com o `Jogador` numa outra posição mais à direita.
--
-- Esta função utiliza outras funções auxiliares que movem o jogador para a direita, alterando o seu x.
moveJogadorDir :: Jogo -> Jogo
moveJogadorDir (Jogo mapa (Jogador (x, y) dir caixa))
  | x + 1 > (length (mapa !! y) - 1) = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Bloco = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Caixa = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Porta = Jogo mapa (Jogador (x, y) Este caixa)
  | otherwise = moveJogadorCair (Jogo mapa (Jogador (x + 1, y) Este caixa))

-- | A função `moveJogadorDirCaixa` tem o mesmo objetivo da função `moveJogadorDir`, mas é utilizada quando esta tem a `Caixa`.
moveJogadorDirCaixa :: Jogo -> Jogo
moveJogadorDirCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | x + 1 > (length (mapa !! y) - 1) = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! (y -1) !! (x + 1) == Bloco = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Bloco = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Caixa = Jogo mapa (Jogador (x, y) Este caixa)
  | mapa !! y !! (x + 1) == Porta = Jogo mapa (Jogador (x, y) Este caixa)
  | otherwise = moveJogadorCair (Jogo mapa (Jogador (x + 1, y) Este caixa))

-- | A função `moveJogadorEsq` recebe um `Jogo` e retribui um `Jogo` com o `Jogador` numa outra posição mais à esquerda.
--
-- Esta função utiliza outras funções auxiliares que movem o jogador para a esquerda, alterando o seu x.
moveJogadorEsq :: Jogo -> Jogo
moveJogadorEsq (Jogo mapa (Jogador (x, y) dir caixa))
  | x -1 < 0 = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Bloco = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Caixa = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Porta = Jogo mapa (Jogador (x, y) Oeste caixa)
  | otherwise = moveJogadorCair (Jogo mapa (Jogador (x -1, y) Oeste caixa))

-- | A função `moveJogadorEsqCaixa` tem o mesmo objetivo da função `moveJogadorEsq`, mas é utilizada quando esta tem a `Caixa`.
moveJogadorEsqCaixa :: Jogo -> Jogo
moveJogadorEsqCaixa (Jogo mapa (Jogador (x, y) dir caixa))
  | x -1 < 0 = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! (y -1) !! (x -1) == Bloco = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Bloco = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Caixa = Jogo mapa (Jogador (x, y) Oeste caixa)
  | mapa !! y !! (x -1) == Porta = Jogo mapa (Jogador (x, y) Oeste caixa)
  | otherwise = moveJogadorCair (Jogo mapa (Jogador (x -1, y) Oeste caixa))

-- | A função `moveJogadorCair` é utilizada para aplicar o momento de queda ou descida do jogador, este movimento é aplicado mudando a posição do jogador no `Mapa`.
moveJogadorCair :: Jogo -> Jogo
moveJogadorCair (Jogo mapa (Jogador (x, y) dir caixa))
  | mapa !! (y + 1) !! x == Bloco = Jogo mapa (Jogador (x, y) dir caixa)
  | mapa !! (y + 1) !! x == Caixa = Jogo mapa (Jogador (x, y) dir caixa)
  | otherwise = moveJogadorCair (Jogo mapa (Jogador (x, y + 1) dir caixa))

caixaCair :: Mapa -> Coordenadas -> Coordenadas
caixaCair mapa (x,y)
  | mapa !! (y + 1) !! x == Bloco = (x,y)
  | mapa !! (y + 1) !! x == Caixa = (x,y)
  | otherwise = caixaCair mapa (x,y+1)

-- | A função `correMovimentos` aplica a função `moveJogador` a todos os elementos de uma lista ordenada de `Movimento`.
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos (Jogo mapa (Jogador coord dir caixa)) [] = Jogo mapa (Jogador coord dir caixa)
correrMovimentos (Jogo mapa (Jogador coord dir caixa)) (x : xs) = correrMovimentos (moveJogador (Jogo mapa (Jogador coord dir caixa)) x) xs