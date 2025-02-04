module Board 
  ( Celula(..)
  , Board
  , Pos
  , initBoard
  , insertarPatron
  , mostrarBoard
  , contarVecinos
  , siguienteGen
  , moverCursor
  ) where

import Data.Array
import Data.List (intercalate)

-- Tipo de datos para las células
data Celula = Viva | Muerta deriving (Show, Eq)

-- Tipos para representar el tablero y la posición
type Board = Array (Int, Int) Celula
type Pos   = (Int, Int)

-- Inicializar un tablero vacío (todas las células Muertas)
initBoard :: Int -> Int -> Board
initBoard w h = array ((0, 0), (w - 1, h - 1)) [((x, y), Muerta) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Insertar un patrón en el tablero (coloca células Viva en las posiciones indicadas)
insertarPatron :: Board -> [Pos] -> Board
insertarPatron board posList = board // [ (pos, Viva) | pos <- posList, inRange (bounds board) pos ]

-- Función para convertir una célula en un carácter
mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

-- Mostrar el tablero en pantalla, marcando la posición del cursor con '@'
mostrarBoard :: Board -> Pos -> IO ()
mostrarBoard board cursor = do
  putStrLn "\ESC[2J" -- Limpia la pantalla
  let (_, (w, h)) = bounds board
  putStrLn $ intercalate "\n" 
           [ [ if (x, y) == cursor 
                  then '@' 
                  else mostrarCelula (board ! (x, y))
             | y <- [0 .. h] ]
           | x <- [0 .. w] ]

-- Contar los vecinos vivos de una posición (usando 'filter', función de orden superior)
contarVecinos :: Board -> Pos -> Int
contarVecinos board (x, y) = length $ filter esViva vecinos
  where
    vecinos :: [Pos]
    vecinos = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
    esViva pos = inRange (bounds board) pos && board ! pos == Viva

-- Calcular la siguiente generación del tablero según las reglas del juego
siguienteGen :: Board -> Board
siguienteGen board = array (bounds board)
  [ ((x, y), nuevaEstado (board ! (x, y)) (contarVecinos board (x, y)))
  | (x, y) <- indices board ]
  where
    nuevaEstado :: Celula -> Int -> Celula
    nuevaEstado Viva nVecinos
      | nVecinos == 2 || nVecinos == 3 = Viva
      | otherwise                    = Muerta
    nuevaEstado Muerta nVecinos
      | nVecinos == 3 = Viva
      | otherwise     = Muerta

-- Mover el cursor en el tablero según la tecla presionada (usando patrones)
moverCursor :: Board -> Pos -> Char -> Pos
moverCursor board (x, y) 'w' = (max 0 (x - 1), y)
moverCursor board (x, y) 's' = (min (fst (snd (bounds board))) (x + 1), y)
moverCursor board (x, y) 'a' = (x, max 0 (y - 1))
moverCursor board (x, y) 'd' = (x, min (snd (snd (bounds board))) (y + 1))
moverCursor _ pos _        = pos
