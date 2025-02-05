module Tablero 
  ( Celula(..)
  , Tablero
  , Pos
  , initTablero
  , insertarPatron
  , mostrarTablero
  , contarVecinos
  , siguienteGen
  , moverCursor
  , actualizarCelula
  ) where

import Data.Array
import Data.List (intercalate)

-- Tipo de datos para las células
data Celula = Viva | Muerta deriving (Show, Eq)

-- Representación del tablero y posiciones
type Tablero = Array (Int, Int) Celula
type Pos = (Int, Int)

-- Inicializa un tablero vacío (todas las células Muertas)
initTablero :: Int -> Int -> Tablero
initTablero w h = array ((0,0),(w-1,h-1))
  [((x,y), Muerta) | x <- [0..w-1], y <- [0..h-1]]

-- Inserta un patrón en el tablero (pone Viva en las posiciones indicadas)
insertarPatron :: Tablero -> [Pos] -> Tablero
insertarPatron tab posList =
  tab // [ (pos, Viva) | pos <- posList, inRange (bounds tab) pos ]

-- Convierte una célula en un carácter para imprimirla
mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

-- Muestra el tablero en pantalla; si la posición coincide con el cursor, muestra '@'
mostrarTablero :: Tablero -> Pos -> IO ()
mostrarTablero tab cursor = do
  putStr "\ESC[2J"  -- Borra pantalla
  let (_, (w, h)) = bounds tab
  putStrLn $ intercalate "\n" [ [ if (x,y)==cursor then '@'
                                   else mostrarCelula (tab ! (x,y))
                              | y <- [0..h] ]
                            | x <- [0..w] ]

-- Cuenta los vecinos vivos de una posición usando 'filter'
contarVecinos :: Tablero -> Pos -> Int
contarVecinos tab (x,y) = length $ filter esViva vecinos
  where
    vecinos = [(x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    esViva pos = inRange (bounds tab) pos && tab ! pos == Viva

-- Calcula la siguiente generación según las reglas de Conway
siguienteGen :: Tablero -> Tablero
siguienteGen tab = array (bounds tab)
  [ ((x,y), nuevaEstado (tab ! (x,y)) (contarVecinos tab (x,y)))
  | (x,y) <- indices tab ]
  where
    nuevaEstado Viva n | n == 2 || n == 3 = Viva
    nuevaEstado Muerta n | n == 3 = Viva
    nuevaEstado _ _ = Muerta

-- Mueve el cursor según la tecla ('w','a','s','d')
moverCursor :: Tablero -> Pos -> Char -> Pos
moverCursor tab (x,y) 'w' = (max 0 (x-1), y)
moverCursor tab (x,y) 's' = (min (fst (snd (bounds tab))) (x+1), y)
moverCursor tab (x,y) 'a' = (x, max 0 (y-1))
moverCursor tab (x,y) 'd' = (x, min (snd (snd (bounds tab))) (y+1))
moverCursor _ pos _ = pos

-- Actualiza una celda; encapsula el operador (//)
actualizarCelula :: Tablero -> Pos -> Celula -> Tablero
actualizarCelula tab pos est = tab // [(pos, est)]
