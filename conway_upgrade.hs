import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Exit (exitSuccess)
import Data.Char (toLower)



-- Tablero y patrones

type Board = [[Int]]

glider :: [(Int, Int)]
glider = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (replicate cols 0)

initializeBoard :: Board -> [(Int, Int)] -> Board
initializeBoard board positions =
    foldl (\b (x, y) -> updateCell b x y 1) board positions
  where
    updateCell b x y value =
        take x b ++ [take y (b !! x) ++ [value] ++ drop (y + 1) (b !! x)] ++ drop (x + 1) b

-- Muestra el tablero con un cursor
printBoardWithCursor :: Board -> (Int, Int) -> IO ()
printBoardWithCursor board (cx, cy) = do
    clearScreen
    setCursorPosition 0 0
    mapM_ putStrLn $ zipWith showRow [0 ..] board
  where
    showRow x row = map (\(y, cell) -> if (x, y) == (cx, cy) then 'X' else if cell == 1 then '*' else '.') (zip [0 ..] row)

-- Movimiento del cursor
moveCursor :: (Int, Int) -> Char -> (Int, Int) -> (Int, Int)
moveCursor (rows, cols) dir (x, y) =
    case toLower dir of
        'w' -> ((x - 1) `mod` rows, y)
        's' -> ((x + 1) `mod` rows, y)
        'a' -> (x, (y - 1) `mod` cols)
        'd' -> (x, (y + 1) `mod` cols)
        _   -> (x, y)

-- Inserta un glider en la posición del cursor
placePattern :: Board -> (Int, Int) -> [(Int, Int)] -> Board
placePattern board (cx, cy) pattern =
    initializeBoard board adjustedPattern
  where
    rows = length board
    cols = length (head board)
    adjustedPattern = [(wrap (cx + dx) rows, wrap (cy + dy) cols) | (dx, dy) <- pattern]
    wrap coord limit = (coord + limit) `mod` limit

-- Bucle interactivo para el modo editor
editorLoop :: Board -> (Int, Int) -> IO ()
editorLoop board cursor = do
    printBoardWithCursor board cursor
    putStrLn "Usa WSAD para mover el cursor, ESPACIO para colocar un glider, Q para salir."
    hFlush stdout
    dir <- getChar
    case toLower dir of
        'q' -> exitSuccess
        ' ' -> editorLoop (placePattern board cursor glider) cursor
        _   -> editorLoop board (moveCursor (length board, length (head board)) dir cursor)

-- Programa principal
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn "Bienvenido al modo editor del Juego de la Vida de Conway"
    putStrLn "Introduce el tamaño del tablero (filas y columnas):"
    putStr "Filas: "
    hFlush stdout
    rows <- readLn
    putStr "Columnas: "
    hFlush stdout
    cols <- readLn
    let board = emptyBoard rows cols
    let initialCursor = (0, 0)
    editorLoop board initialCursor
