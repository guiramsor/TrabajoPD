module Patrones 
  ( Patron(..)
  , gliderPatron
  , lwssPatron
  , pulsarPatron
  , patrones
  ) where

import Tablero (Pos)

-- Tipo de dato para un patrón predefinido
data Patron = Patron
  { nombre     :: String
  , posiciones :: [Pos]
  , ancho      :: Int
  , alto       :: Int
  } deriving (Show, Eq)

-- Algunos patrones clásicos
gliderPatron :: Patron
gliderPatron = Patron "Glider" [(0,1),(1,2),(2,0),(2,1),(2,2)] 10 10

lwssPatron :: Patron
lwssPatron = Patron "LWSS" [(1,1),(1,4),(2,5),(3,1),(3,5),(4,2),(4,3),(4,4),(4,5)] 8 20

pulsarPatron :: Patron
pulsarPatron = Patron "Pulsar" 
  [ (2, 4), (2, 5), (2, 6), (2, 10), (2, 11), (2, 12),
    (4, 2), (4, 7), (4, 9), (4, 14), (5, 2), (5, 7), (5, 9), (5, 14),
    (6, 2), (6, 7), (6, 9), (6, 14), (7, 4), (7, 5), (7, 6), (7, 10), (7, 11), (7, 12),
    (9, 4), (9, 5), (9, 6), (9, 10), (9, 11), (9, 12), (10, 2), (10, 7), (10, 9), (10, 14),
    (11, 2), (11, 7), (11, 9), (11, 14), (12, 2), (12, 7), (12, 9), (12, 14),
    (14, 4), (14, 5), (14, 6), (14, 10), (14, 11), (14, 12)
  ] 17 17

patrones :: [Patron]
patrones = [gliderPatron, lwssPatron, pulsarPatron]
