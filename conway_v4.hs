import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hReady, hFlush)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Array

-- Tipo para representar el tablero
type Board = Array (Int, Int) Int
type Pos = (Int, Int)

-- Dimensiones del tablero (modificables en personalización)
anchoDef, altoDef :: Int
anchoDef = 20
altoDef = 20

-- Inicializar un tablero vacío con dimensiones específicas
initBoard :: Int -> Int -> Board
initBoard w h = array ((0, 0), (w - 1, h - 1)) [((x, y), 0) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

-- Patrones
patronGlider :: [Pos]
patronGlider = [(0,1), (1,2), (2,0), (2,1), (2,2)]

patronLWSS :: [Pos]
patronLWSS = [(1,1), (1,4), (2,5), (3,1), (3,5), (4,2), (4,3), (4,4), (4,5)]

patronPulsar :: [Pos]
patronPulsar = [(2,4), (2,5), (2,6), (2,10), (2,11), (2,12),
                (4,2), (4,7), (4,9), (4,14), (5,2), (5,7), (5,9), (5,14),
                (6,2), (6,7), (6,9), (6,14), (7,4), (7,5), (7,6), (7,10), (7,11), (7,12)]

patrones :: [(String, [Pos], Int, Int)]
patrones = [("Glider", patronGlider, 10, 10), ("LWSS", patronLWSS, 12, 12), ("Pulsar", patronPulsar, 20, 20)]

-- Insertar un patrón en el tablero
insertarPatron :: Board -> [Pos] -> Board
insertarPatron board posiciones = board // [(pos, 1) | pos <- posiciones, inRange (bounds board) pos]

-- Mostrar tablero con cursor
mostrarBoard :: Board -> Pos -> IO ()
mostrarBoard board cursor = do
  putStrLn "\ESC[2J" -- Limpia pantalla
  let (_, (w, h)) = bounds board
  putStrLn $ intercalate "\n" [[if (x, y) == cursor then '@' else if board ! (x, y) == 1 then '*' else '.' | y <- [0 .. h]] | x <- [0 .. w]]

-- Contar vecinos vivos
contarVecinos :: Board -> Pos -> Int
contarVecinos board (x, y) = length [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0), vivo (x + dx, y + dy)]
  where
    vivo (nx, ny) = inRange (bounds board) (nx, ny) && board ! (nx, ny) == 1

-- Avanzar una generación
siguienteGen :: Board -> Board
siguienteGen board = array (bounds board) [((x, y), actualizarCelda (x, y)) | (x, y) <- indices board]
  where
    actualizarCelda pos
      | board ! pos == 1 && (nVecinos == 2 || nVecinos == 3) = 1
      | board ! pos == 0 && nVecinos == 3 = 1
      | otherwise = 0
      where
        nVecinos = contarVecinos board pos

-- Loop de ejecución automática
autoLoop :: Board -> IO ()
autoLoop board = do
  mostrarBoard board (-1, -1)
  threadDelay 400000
  autoLoop (siguienteGen board)

-- Mover cursor
moverCursor :: Board -> Pos -> Char -> Pos
moverCursor board (x, y) 'w' = (max 0 (x - 1), y)
moverCursor board (x, y) 's' = (min (fst (snd (bounds board))) (x + 1), y)
moverCursor board (x, y) 'a' = (x, max 0 (y - 1))
moverCursor board (x, y) 'd' = (x, min (snd (snd (bounds board))) (y + 1))
moverCursor _ pos _ = pos

-- Editar tablero interactivamente
configurarBoard :: Board -> Pos -> IO Board
configurarBoard board cursor = do
  mostrarBoard board cursor
  putStrLn "Usa WASD para mover el cursor, V para colocar células vivas, I para iniciar:"
  hFlush stdout
  key <- getChar
  let nuevoCursor = moverCursor board cursor key
  case toLower key of
    'v' -> configurarBoard (board // [(nuevoCursor, 1)]) nuevoCursor
    'i' -> return board
    _   -> configurarBoard board nuevoCursor

-- Seleccionar patrón o personalizar tamaño
seleccionarPatron :: IO Board
seleccionarPatron = do
  putStrLn "Elige un patrón o personaliza el tablero:"
  mapM_ (\(i, (nombre, _, _, _)) -> putStrLn (show i ++ ". " ++ nombre)) (zip [1..] patrones)
  putStrLn "0. Personalizar"
  putStr "Tu elección: "
  hFlush stdout
  choice <- hSetEcho stdin True >> readLn <* hSetEcho stdin False
  if choice >= 1 && choice <= length patrones
    then let (_, posiciones, w, h) = patrones !! (choice - 1)
         in return $ insertarPatron (initBoard w h) posiciones
    else do
      putStr "Introduce ancho del tablero: "
      hFlush stdout
      w <- readLn
      putStr "Introduce alto del tablero: "
      hFlush stdout
      h <- readLn
      configurarBoard (initBoard w h) (0, 0)

-- Main
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  boardInicial <- seleccionarPatron
  autoLoop boardInicial
