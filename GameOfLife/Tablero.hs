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

-- Tipo para representar el tablero y la posición
type Tablero = Array (Int, Int) Celula
type Pos = (Int, Int)

-- Inicializar un tablero vacío (todas las células Muertas)
initTablero :: Int -> Int -> Tablero
initTablero w h = array ((0, 0), (w - 1, h - 1)) [((x, y), Muerta) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Insertar un patrón en el tablero (coloca células Viva en las posiciones indicadas)
insertarPatron :: Tablero -> [Pos] -> Tablero
insertarPatron tablero posList = tablero // [ (pos, Viva) | pos <- posList, inRange (bounds tablero) pos ]

-- Función para convertir una célula en un carácter
mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

-- Mostrar el tablero en pantalla, marcando la posición del cursor con '@'
mostrarTablero :: Tablero -> Pos -> IO ()
mostrarTablero tablero cursor = do
  putStrLn "\ESC[2J" -- Limpia la pantalla
  let (_, (w, h)) = bounds tablero
  putStrLn $ intercalate "\n" 
           [ [ if (x, y) == cursor 
                  then '@' 
                  else mostrarCelula (tablero ! (x, y))
             | y <- [0 .. h] ]
           | x <- [0 .. w] ]

-- Contar los vecinos vivos de una posición (usando 'filter', función de orden superior)
contarVecinos :: Tablero -> Pos -> Int
contarVecinos tablero (x, y) = length $ filter esViva vecinos
  where
    vecinos :: [Pos]
    vecinos = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
    esViva pos = inRange (bounds tablero) pos && tablero ! pos == Viva

-- Calcular la siguiente generación del tablero según las reglas del juego
siguienteGen :: Tablero -> Tablero
siguienteGen tablero = array (bounds tablero)
  [ ((x, y), nuevaEstado (tablero ! (x, y)) (contarVecinos tablero (x, y)))
  | (x, y) <- indices tablero ]
  where
    nuevaEstado :: Celula -> Int -> Celula
    nuevaEstado Viva nVecinos
      | nVecinos == 2 || nVecinos == 3 = Viva
      | otherwise                    = Muerta
    nuevaEstado Muerta nVecinos
      | nVecinos == 3 = Viva
      | otherwise     = Muerta

-- Mover el cursor en el tablero según la tecla presionada (definición por patrones)
moverCursor :: Tablero -> Pos -> Char -> Pos
moverCursor tablero (x, y) 'w' = (max 0 (x - 1), y)
moverCursor tablero (x, y) 's' = (min (fst (snd (bounds tablero))) (x + 1), y)
moverCursor tablero (x, y) 'a' = (x, max 0 (y - 1))
moverCursor tablero (x, y) 'd' = (x, min (snd (snd (bounds tablero))) (y + 1))
moverCursor _ pos _          = pos

-- Función auxiliar para actualizar una celda del tablero (encapsula el operador '//')
actualizarCelula :: Tablero -> Pos -> Celula -> Tablero
actualizarCelula tablero pos estado = tablero // [(pos, estado)]
