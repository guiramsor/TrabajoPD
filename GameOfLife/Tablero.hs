-- ---------------------------------------------------------------------
-- Este módulo define la representación del tablero (usando arrays),
-- las funciones para inicializar y actualizar el tablero, así como
-- la aplicación de las reglas del Juego de la Vida de Conway.
-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------
-- Tipos de datos
-- ---------------------------------------------------------------------

data Celula = Viva | Muerta deriving (Show, Eq)

type Tablero = Array (Int, Int) Celula
type Pos = (Int, Int)

-- ---------------------------------------------------------------------
-- Inicialización y actualización del tablero

-- Con "initTablero" inicializamos el tablero vacio y con "insertarPatron"
-- añadimos patron al tablero.
-- ---------------------------------------------------------------------

initTablero :: Int -> Int -> Tablero
initTablero numFilas numCol = array ((0, 0), (numFilas - 1, numCol - 1))
  [ ((fila, col), Muerta) | fila <- [0 .. numFilas - 1], col <- [0 .. numCol - 1] ]

insertarPatron :: Tablero -> [Pos] -> Tablero
insertarPatron tablero posList =
  tablero // [ (pos, Viva) | pos <- nub posList, inRange (bounds tablero) pos ]

-- ---------------------------------------------------------------------
-- Visualización del tablero
-- Con "mostrarTablero" mostramos el tablero en pantalla. El cursor se indica con '@'
-- ---------------------------------------------------------------------

mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

mostrarTablero :: Tablero -> Pos -> IO ()
mostrarTablero tablero cursor = do
  putStr "\ESC[2J"
  let (_, (numFilas, numCol)) = bounds tablero
  putStrLn $ intercalate "\n"
    [ [ if (fila, col) == cursor then '@'
         else mostrarCelula (tablero ! (fila, col))
      | col <- [0 .. numCol - 1] ]
    | fila <- [0 .. numFilas - 1] ]

-- ---------------------------------------------------------------------
-- Cálculo de la siguiente generación
-- ---------------------------------------------------------------------

contarVecinos :: Tablero -> Pos -> Int
contarVecinos tablero (x, y) = length $ filter esViva vecinos
  where
    vecinos :: [Pos]
    vecinos = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
    esViva pos = inRange (bounds tablero) pos && tablero ! pos == Viva

reglaConway :: Celula -> Int -> Celula
reglaConway c n
  | c == Viva   && (n == 2 || n == 3) = Viva    -- Supervivencia
  | c == Muerta && n == 3             = Viva    -- Nacimiento
  | otherwise                         = Muerta  -- Muerte (soledad o sobrepoblación)

-- Calcula la siguiente generación aplicando la regla a cada celda
siguienteGen :: Tablero -> Tablero
siguienteGen tablero = array (bounds tablero)
  [ ((fila, col), reglaConway (tablero ! (fila, col)) (contarVecinos tablero (fila, col))) 
  | (fila, col) <- indices tablero ]

-- ---------------------------------------------------------------------
-- Movimiento y actualización de celdas
-- ---------------------------------------------------------------------

moverCursor :: Tablero -> Pos -> Char -> Pos
moverCursor tablero (x, y) 'w' = (max 0 (x - 1), y)
moverCursor tablero (x, y) 's' = (min (fst (snd (bounds tablero))) (x + 1), y)
moverCursor tablero (x, y) 'a' = (x, max 0 (y - 1))
moverCursor tablero (x, y) 'd' = (x, min (snd (snd (bounds tablero))) (y + 1))
moverCursor _ pos _ = pos

actualizarCelula :: Tablero -> Pos -> Celula -> Tablero
actualizarCelula tablero pos est = tablero // [(pos, est)]
