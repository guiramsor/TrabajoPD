import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hReady, hFlush)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Char (toLower, isDigit)
import Data.List (intercalate)
import Data.Array

-- Tipo para representar el tablero
type Board = Array (Int, Int) Int
type Pos = (Int, Int)

-- Dimensiones del tablero (modificables en personalización)
anchoDef, altoDef :: Int
anchoDef = 20
altoDef = 20

-- Función para leer números con soporte de Backspace
leerNumeroSeguro :: IO Int
leerNumeroSeguro = do
  hSetEcho stdin False
  let loop acc = do
        c <- getChar
        case c of
          '\n' -> do
            putStrLn ""
            return (read acc)
          '\DEL' -> if null acc
                     then loop acc
                     else do
                       putStr "\b \b"
                       hFlush stdout
                       loop (init acc)
          _ | isDigit c -> do
              putChar c
              hFlush stdout
              loop (acc ++ [c])
            | otherwise -> loop acc
  loop ""

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

-- Seleccionar patrón o personalizar tamaño
seleccionarPatron :: IO Board
seleccionarPatron = do
  putStrLn "Elige un patrón o personaliza el tablero:"
  mapM_ (\(i, (nombre, _, _, _)) -> putStrLn (show i ++ ". " ++ nombre)) (zip [1..] patrones)
  putStrLn "0. Personalizar"
  putStr "Tu elección: "
  hFlush stdout
  choice <- leerNumeroSeguro
  if choice >= 1 && choice <= length patrones
    then let (_, posiciones, w, h) = patrones !! (choice - 1)
         in return $ insertarPatron (initBoard w h) posiciones
    else do
      putStr "Introduce ancho del tablero: "
      hFlush stdout
      w <- leerNumeroSeguro
      putStr "Introduce alto del tablero: "
      hFlush stdout
      h <- leerNumeroSeguro
      return (initBoard w h)

-- Main
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  boardInicial <- seleccionarPatron
  autoLoop boardInicial
