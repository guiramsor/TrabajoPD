module Main where

import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hFlush)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import Tablero
import Patrones
import qualified Data.Map as M

-- Inspirado en P9.3: funciones sencillas para mover cursor y borrar pantalla
mueveCursor :: Int -> Int -> IO ()
mueveCursor x y = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

borraPantalla :: IO ()
borraPantalla = putStr "\ESC[2J"

-- Selección del patrón. Se usa Data.Map para asociar opciones.
seleccionarPatron :: IO Tablero
seleccionarPatron = do
  putStrLn "Elige un patrón (o 0 para personalizar):"
  let mapaPatrones = M.fromList $ zip [1..] patrones
  mapM_ (\(i, p) -> putStrLn $ show i ++ ". " ++ nombre p) (M.toList mapaPatrones)
  putStr "Opción: "
  hFlush stdout
  hSetEcho stdin True
  opcion <- readLn
  hSetEcho stdin False
  if opcion >= 1 && opcion <= M.size mapaPatrones
    then case M.lookup opcion mapaPatrones of
           Just pat -> return $ insertarPatron (initTablero (ancho pat) (alto pat))
                                                (posiciones pat)
           Nothing  -> error "Opción incorrecta"
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
      return $ initTablero w h

-- Modo interactivo para editar el tablero: se mueve el cursor y se activa una celda al pulsar ENTER.
configurarTablero :: Tablero -> Pos -> IO Tablero
configurarTablero tab cursor = do
  mostrarTablero tab cursor
  putStrLn "Usa WASD para mover el cursor, ENTER para activar, I para iniciar:"
  hFlush stdout
  tecla <- getChar
  let nuevoCursor = moverCursor tab cursor tecla
  case tecla of
    '\n' -> configurarTablero (actualizarCelula tab cursor Viva) cursor
    c | toLower c == 'i' -> return tab
    _ -> configurarTablero tab nuevoCursor

-- Bucle que avanza en generaciones, con una pausa breve.
autoLoop :: Tablero -> IO ()
autoLoop tab = do
  mostrarTablero tab (-1, -1)
  threadDelay 300000
  autoLoop (siguienteGen tab)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  borraPantalla
  putStrLn "Bienvenido al juego de la vida de John Conway."
  putStrLn "Pulsa ENTER para continuar..."
  _ <- getChar
  tabInicial <- seleccionarPatron
  tabFinal <- configurarTablero tabInicial (0,0)
  autoLoop tabFinal
