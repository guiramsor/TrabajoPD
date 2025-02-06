-- ---------------------------------------------------------------------
-- § Juego de la Vida de John Conway

-- Mini-Proyecto Realizado por:
-- * Jatin Dulani - (BLD2172)
-- * Guillermo Ramón Soria - (guiramsor)

-- Este módulo gestiona la interacción con el usuario, la selección
-- del patrón o la personalización del tablero, y el bucle principal
-- de ejecución.
  
-- Se usan secuencias de escape para limpiar la pantalla y mover el cursor,
-- siguiendo el estilo de las prácticas.
-- ---------------------------------------------------------------------

module Main where

import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hFlush)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import Tablero
import Patrones
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- Secuencias de escape para control del terminal
-- ---------------------------------------------------------------------
mueveCursor :: Int -> Int -> IO ()
mueveCursor x y = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

borraPantalla :: IO ()
borraPantalla = putStr "\ESC[2J"

-- ---------------------------------------------------------------------
-- Se muestra un menú con los patrones predefinidos. (0 para personalizar)
-- ---------------------------------------------------------------------
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
    then let Just pat = M.lookup opcion mapaPatrones
         in return $ insertarPatron (initTablero (numFilas pat) (numCol pat))
                                      (posiciones pat)
    else do
      putStr "Introduce número de filas: "
      hFlush stdout
      hSetEcho stdin True
      nf <- readLn
      hSetEcho stdin False
      putStr "Introduce número de columnas: "
      hFlush stdout
      hSetEcho stdin True
      nc <- readLn
      hSetEcho stdin False
      return $ initTablero nf nc

-- ---------------------------------------------------------------------
-- Configuración interactiva del tablero
-- Permite al usuario editar el tablero moviendo el cursor y activando
-- celdas (con ENTER). Se termina con la tecla 'I'.
-- ---------------------------------------------------------------------
configurarTablero :: Tablero -> Pos -> IO Tablero
configurarTablero tab cursor = do
  mostrarTablero tab cursor
  putStrLn "Usa WASD para mover el cursor, ENTER para activar una celda, I para iniciar:"
  hFlush stdout
  tecla <- getChar
  let nuevoCursor = moverCursor tab cursor tecla
  case tecla of
    '\n' -> configurarTablero (actualizarCelula tab cursor Viva) cursor
    c | toLower c == 'i' -> return tab
    _ -> configurarTablero tab nuevoCursor

-- ---------------------------------------------------------------------
-- Bucle principal: avanza en generaciones con una breve pausa.
-- ---------------------------------------------------------------------
autoLoop :: Tablero -> IO ()
autoLoop tab = do
  mostrarTablero tab (-1, -1)
  threadDelay 300000
  autoLoop (siguienteGen tab)

-- ---------------------------------------------------------------------
-- Función principal.
-- Selecciona o personaliza el tablero y arranca el juego.
-- ---------------------------------------------------------------------
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  borraPantalla
  putStrLn "Bienvenido al Juego de la Vida de John Conway."
  putStrLn "Pulsa ENTER para continuar..."
  _ <- getChar
  tabInicial <- seleccionarPatron
  tabFinal <- configurarTablero tabInicial (0,0)
  autoLoop tabFinal
