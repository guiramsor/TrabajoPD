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
  -- , analizaVecinos
  ) where

import Data.Array
import Data.List (intercalate)

-- Tipo de datos para las células
data Celula = Viva | Muerta deriving (Show, Eq)

type Tablero = Array (Int, Int) Celula
type Pos = (Int, Int)

-- Inicializa un tablero vacio
initTablero :: Int -> Int -> Tablero
initTablero numFilas numCol = array ((0, 0), (numFilas - 1, numCol - 1))
  [ ((fila, col), Muerta) | fila <- [0 .. numFilas - 1], col <- [0 .. numCol - 1] ]

-- Inserta un patrón en el tablero 
insertarPatron :: Tablero -> [Pos] -> Tablero
insertarPatron tablero posList =
  tablero // [ (pos, Viva) | pos <- posList, inRange (bounds tablero) pos ]

-- carácter de la celula
mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

-- Muestra el tablero en pantalla
mostrarTablero :: Tablero -> Pos -> IO ()
mostrarTablero tablero cursor = do
  putStr "\ESC[2J"
  let (_, (numFilas, numCol)) = bounds tablero
  putStrLn $ intercalate "\n"
    [ [ if (fila, col) == cursor then '@'
         else mostrarCelula (tablero ! (fila, col))
      | col <- [0 .. numCol - 1] ]
    | fila <- [0 .. numFilas - 1] ]

-- Cuenta los vecinos vivos de una posición
contarVecinos :: Tablero -> Pos -> Int
contarVecinos tablero (x, y) = length $ filter esViva vecinos
  where
    vecinos :: [Pos]
    vecinos = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
    esViva pos = inRange (bounds tablero) pos && tablero ! pos == Viva

-- Calcula la siguiente generación del tablero según las reglas de Conway
siguienteGen :: Tablero -> Tablero
siguienteGen tablero = array (bounds tablero)
  [ ((fila, col), nuevaEstado (tablero ! (fila, col)) (contarVecinos tablero (fila, col)))
  | (fila, col) <- indices tablero ]
  where
    nuevaEstado Viva n | n == 2 || n == 3 = Viva
    nuevaEstado Muerta n | n == 3 = Viva
    nuevaEstado _ _ = Muerta

-- Mueve el cursor según la tecla
moverCursor :: Tablero -> Pos -> Char -> Pos
moverCursor tablero (x, y) 'w' = (max 0 (x - 1), y)
moverCursor tablero (x, y) 's' = (min (fst (snd (bounds tablero))) (x + 1), y)
moverCursor tablero (x, y) 'a' = (x, max 0 (y - 1))
moverCursor tablero (x, y) 'd' = (x, min (snd (snd (bounds tablero))) (y + 1))
moverCursor _ pos _ = pos

-- Actualiza una celda del tablero
actualizarCelula :: Tablero -> Pos -> Celula -> Tablero
actualizarCelula tablero pos est = tablero // [(pos, est)]

-- analizaVecinos :: Int -> String
-- analizaVecinos n
--   | n < 2     = "Célula aislada"       
--   | n == 2    = "Vive en equilibrio"   
--   | n == 3    = "Perfectamente viva"   
--   | n > 3     = "Sobrepoblada"         

