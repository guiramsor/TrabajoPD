module Main where

import System.IO (hSetBuffering, hSetEcho, BufferMode(NoBuffering), stdin, stdout, hFlush)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import Board
import Patrones
import Data.Array


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

-- Loop de ejecución automática: muestra el tablero y avanza en generaciones
autoLoop :: Board -> IO ()
autoLoop board = do
  mostrarBoard board (-1, -1)
  threadDelay 400000
  autoLoop (siguienteGen board)

-- Seleccionar un patrón predefinido o personalizar el tablero
seleccionarPatron :: IO Board
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

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  boardInicial <- seleccionarPatron
  autoLoop boardInicial
