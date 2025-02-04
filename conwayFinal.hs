module GameOfLife where

import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hFlush)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Array

-- Tipos de datos algebraicos
data Celula = Viva | Muerta deriving (Show, Eq)

data Patron = Patron 
  { nombre     :: String
  , posiciones :: [Pos]
  , ancho      :: Int
  , alto       :: Int 
  } deriving (Show, Eq)

-- Tipo para representar el tablero y posiciones
type Board = Array (Int, Int) Celula
type Pos   = (Int, Int)

-- Dimensiones por defecto (para personalización)
anchoDef, altoDef :: Int
anchoDef = 20
altoDef = 20

-- Inicializar un tablero vacío con dimensiones específicas (todas las células Muertas)
initBoard :: Int -> Int -> Board
initBoard w h = array ((0, 0), (w - 1, h - 1)) [((x, y), Muerta) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Función definida por patrones para mostrar una célula
mostrarCelula :: Celula -> Char
mostrarCelula Viva   = '*'
mostrarCelula Muerta = '.'

-- Patrones predefinidos
gliderPatron :: Patron
gliderPatron = Patron "Glider" [(0,1), (1,2), (2,0), (2,1), (2,2)] 10 10

lwssPatron :: Patron
lwssPatron = Patron "LWSS" [(1,1), (1,4), (2,5), (3,1), (3,5), (4,2), (4,3), (4,4), (4,5)] 22 10

pulsarPatron :: Patron
pulsarPatron = Patron "Pulsar" 
  [
    (2, 4), (2, 5), (2, 6), (2, 10), (2, 11), (2, 12),
    (4, 2), (4, 7), (4, 9), (4, 14), (5, 2), (5, 7), (5, 9), (5, 14),
    (6, 2), (6, 7), (6, 9), (6, 14), (7, 4), (7, 5), (7, 6), (7, 10), (7, 11), (7, 12),
    (9, 4), (9, 5), (9, 6), (9, 10), (9, 11), (9, 12), (10, 2), (10, 7), (10, 9), (10, 14),
    (11, 2), (11, 7), (11, 9), (11, 14), (12, 2), (12, 7), (12, 9), (12, 14),
    (14, 4), (14, 5), (14, 6), (14, 10), (14, 11), (14, 12)
  ]
  17 17

patrones :: [Patron]
patrones = [gliderPatron, lwssPatron, pulsarPatron]

-- Insertar un patrón en el tablero (coloca células Viva en las posiciones indicadas)
insertarPatron :: Board -> [Pos] -> Board
insertarPatron board posList = board // [ (pos, Viva) | pos <- posList, inRange (bounds board) pos ]

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

-- Contar los vecinos vivos (células en estado Viva) de una posición dada
-- Aquí se utiliza 'filter', una función de orden superior, para filtrar las posiciones vecinas que son válidas y están vivas.
contarVecinos :: Board -> Pos -> Int
contarVecinos board (x, y) = length $ filter esViva vecinos
  where
    vecinos :: [Pos]
    vecinos = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
    esViva pos = inRange (bounds board) pos && board ! pos == Viva

-- Función con guardas para determinar si una célula está viva
esCelulaViva :: Celula -> Bool
esCelulaViva cell
  | cell == Viva = True
  | otherwise    = False

-- Avanzar una generación, actualizando el estado de cada célula según las reglas
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

-- Loop de ejecución automática: muestra el tablero y avanza en generaciones
autoLoop :: Board -> IO ()
autoLoop board = do
  mostrarBoard board (-1, -1)
  threadDelay 400000
  autoLoop (siguienteGen board)

-- Mover el cursor utilizando patrones para cada tecla ('w', 'a', 's', 'd')
moverCursor :: Board -> Pos -> Char -> Pos
moverCursor board (x, y) 'w' = (max 0 (x - 1), y)
moverCursor board (x, y) 's' = (min (fst (snd (bounds board))) (x + 1), y)
moverCursor board (x, y) 'a' = (x, max 0 (y - 1))
moverCursor board (x, y) 'd' = (x, min (snd (snd (bounds board))) (y + 1))
moverCursor _ pos _        = pos

-- Editar el tablero interactivamente: mover cursor y activar células al presionar ENTER
configurarBoard :: Board -> Pos -> IO Board
configurarBoard board cursor = do
  mostrarBoard board cursor
  putStrLn "Usa WASD para mover el cursor, ENTER para colocar células vivas, I para iniciar:"
  hFlush stdout
  key <- getChar
  let nuevoCursor = moverCursor board cursor key
  case key of
    '\n' -> configurarBoard (board // [(cursor, Viva)]) cursor
    c | toLower c == 'i' -> return board
    _    -> configurarBoard board nuevoCursor

-- Seleccionar un patrón predefinido o personalizar el tablero
seleccionarPatron :: IO Board
seleccionarPatron = do
  putStrLn "Elige un patrón o personaliza el tablero:"
  mapM_ (\(i, patron) -> putStrLn (show i ++ ". " ++ nombre patron)) (zip [1..] patrones)
  putStrLn "0. Personalizar"
  putStr "Tu elección: "
  hFlush stdout
  -- Activamos echo para que el usuario vea lo que escribe
  hSetEcho stdin True
  choice <- readLn
  hSetEcho stdin False
  if choice >= 1 && choice <= length patrones
    then let patronSeleccionado = patrones !! (choice - 1)
         in return $ insertarPatron 
                      (initBoard (ancho patronSeleccionado) (alto patronSeleccionado))
                      (posiciones patronSeleccionado)
    else do
      putStr "Introduce ancho del tablero: "
      hFlush stdout
      hSetEcho stdin True
      w <- readLn
      hSetEcho stdin False
      putStr "Introduce alto del tablero: "
      hFlush stdout
      hSetEcho stdin True
      h <- readLn
      hSetEcho stdin False
      configurarBoard (initBoard w h) (0, 0)

-- Función principal
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  boardInicial <- seleccionarPatron
  autoLoop boardInicial
