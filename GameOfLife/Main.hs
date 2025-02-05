module Main where

import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hFlush)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)

import Tablero
import Patrones

-- Editar el tablero interactivamente: mover cursor y activar células al presionar ENTER
configurarTablero :: Tablero -> Pos -> IO Tablero
configurarTablero tablero cursor = do
  mostrarTablero tablero cursor
  putStrLn "Usa WASD para mover el cursor, ENTER para colocar células vivas, I para iniciar:"
  hFlush stdout
  key <- getChar
  let nuevoCursor = moverCursor tablero cursor key
  case key of
    '\n' -> configurarTablero (actualizarCelula tablero cursor Viva) cursor
    c | toLower c == 'i' -> return tablero
    _    -> configurarTablero tablero nuevoCursor

-- Loop de ejecución automática: muestra el tablero y avanza en generaciones
autoLoop :: Tablero -> IO ()
autoLoop tablero = do
  mostrarTablero tablero (-1, -1)
  threadDelay 300000
  autoLoop (siguienteGen tablero)

-- Seleccionar un patrón predefinido o personalizar el tablero
seleccionarPatron :: IO Tablero
seleccionarPatron = do
  putStrLn "Elige un patrón o personaliza el tablero:"
  mapM_ (\(i, patron) -> putStrLn (show i ++ ". " ++ nombre patron)) (zip [1..] patrones)
  putStrLn "0. Personalizar"
  putStr "Tu elección: "
  hFlush stdout
  hSetEcho stdin True
  choice <- readLn
  hSetEcho stdin False
  if choice >= 1 && choice <= length patrones
    then let patronSeleccionado = patrones !! (choice - 1)
         in return $ insertarPatron 
                      (initTablero (ancho patronSeleccionado) (alto patronSeleccionado))
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
      configurarTablero (initTablero w h) (0, 0)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  boardInicial <- seleccionarPatron
  autoLoop boardInicial
