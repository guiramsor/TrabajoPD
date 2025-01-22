import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

type Board = [[Int]]

-- Patrones predefinidos
glider :: [(Int, Int)]
glider = [(0, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

lwss :: [(Int, Int)]
lwss = [
    (1, 1), (1, 4), (2, 5), (3, 1), (3, 5), (4, 2), (4, 3), (4, 4), (4, 5)
    ]

pulsar :: [(Int, Int)]
pulsar = [
    (2, 4), (2, 5), (2, 6), (2, 10), (2, 11), (2, 12),
    (4, 2), (4, 7), (4, 9), (4, 14), (5, 2), (5, 7), (5, 9), (5, 14),
    (6, 2), (6, 7), (6, 9), (6, 14), (7, 4), (7, 5), (7, 6), (7, 10), (7, 11), (7, 12),
    (9, 4), (9, 5), (9, 6), (9, 10), (9, 11), (9, 12), (10, 2), (10, 7), (10, 9), (10, 14),
    (11, 2), (11, 7), (11, 9), (11, 14), (12, 2), (12, 7), (12, 9), (12, 14),
    (14, 4), (14, 5), (14, 6), (14, 10), (14, 11), (14, 12)
    ]

patterns :: [(String, (Int, Int, [(Int, Int)]))]
patterns = [
    ("glider", (10, 10, glider)),
    ("lwss", (8, 20, lwss)),
    ("pulsar", (17, 20, pulsar))
    ]

-- Imprime el tablero en la consola
printBoard :: Board -> IO ()
printBoard board = do
    mapM_ (putStrLn . map (\x -> if x == 1 then '*' else '.')) board
    putStrLn ""

-- Calcula los vecinos vivos de una célula con bordes infinitos
liveNeighbors :: Board -> Int -> Int -> Int
liveNeighbors board x y = sum [board !! wrappedRow (x + dx) !! wrappedCol (y + dy) | (dx, dy) <- deltas]
  where
    rows = length board
    cols = length (head board)
    deltas = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]
    wrappedRow nx = (nx + rows) `mod` rows
    wrappedCol ny = (ny + cols) `mod` cols


-- Avanza una generación
nextGeneration :: Board -> Board
nextGeneration board =
    [[ nextState (board !! x !! y) (liveNeighbors board x y)
     | y <- [0..cols-1]]
     | x <- [0..rows-1]]
  where
    rows = length board
    cols = length (head board)
    nextState cell neighbors
        | cell == 1 && (neighbors == 2 || neighbors == 3) = 1
        | cell == 0 && neighbors == 3 = 1
        | otherwise = 0

-- Animación automática
autoGameLoop :: Board -> IO ()
autoGameLoop board = do
    printBoard board
    threadDelay 400000  -- Pausa de 500 ms (ajustable para velocidad)
    let nextBoard = nextGeneration board
    autoGameLoop nextBoard

-- Inicializa un tablero vacío
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (replicate cols 0)

-- Configura células vivas iniciales
initializeBoard :: Board -> [(Int, Int)] -> Board
initializeBoard board positions =
    foldl (\b (x, y) -> updateCell b x y 1) board positions
  where
    updateCell b x y value =
        take x b ++ [take y (b !! x) ++ [value] ++ drop (y+1) (b !! x)] ++ drop (x+1) b

-- Programa principal
main :: IO ()
main = do
    putStrLn "Bienvenido al Juego de la Vida de Conway"
    putStrLn "Introduce el nombre de un patrón (glider, lwss, pulsar), o deja vacío para personalizar:"
    patternName <- getLine
    case lookup patternName patterns of
        Just (rows, cols, positions) -> do
            let board = initializeBoard (emptyBoard rows cols) positions
            autoGameLoop board
        Nothing -> do
            putStrLn "Introduce el tamaño del tablero (filas y columnas):"
            putStr "Filas: "
            hFlush stdout
            rows <- readLn
            putStr "Columnas: "
            hFlush stdout
            cols <- readLn
            let board = emptyBoard rows cols
            putStrLn "Introduce las coordenadas de las células vivas iniciales (formato: x y), termina con una línea vacía:"
            let getPositions = do
                    input <- getLine
                    if null input
                        then return []
                        else do
                            let [x, y] = map read (words input)
                            rest <- getPositions
                            return ((x, y) : rest)
            positions <- getPositions
            let initialBoard = initializeBoard board positions
            autoGameLoop initialBoard
