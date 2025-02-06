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
  , reglaConway
  ) where

import Data.Array
import Data.List (intercalate, nub)

data Celula = Viva | Muerta deriving (Show, Eq)

type Tablero = Array (Int, Int) Celula
type Pos = (Int, Int)

-- Inicializa un tablero vacio
initTablero :: Int -> Int -> Tablero
initTablero numFilas numCol = array ((0, 0), (numFilas - 1, numCol - 1))
  [ ((fila, col), Muerta) | fila <- [0 .. numFilas - 1], col <- [0 .. numCol - 1] ]

-- Inserta un patrón en el tablero (se eliminan duplicados con nub)
insertarPatron :: Tablero -> [Pos] -> Tablero
insertarPatron tablero posList =
  tablero // [ (pos, Viva) | pos <- nub posList, inRange (bounds tablero) pos ]

-- Convierte una célula en un carácter
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
  [ ((fila, col), reglaConway (tablero ! (fila, col)) (contarVecinos tablero (fila, col))) 
  | (fila, col) <- indices tablero ]

-- Función de reglas de Conway, definida completamente con guardas
reglaConway :: Celula -> Int -> Celula
reglaConway c n
  | c == Viva   && (n == 2 || n == 3) = Viva
  | c == Muerta && n == 3             = Viva
  | otherwise                         = Muerta

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
