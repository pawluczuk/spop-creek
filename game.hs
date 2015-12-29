-- BoardSize: (szerokosc, wysokosc) rozmiar planszy
type BoardSize = (Int, Int)

-- Coordinates: współrzędna na planszy, np. (1,1)
type Coordinate = (Int, Int)

-- CoordValue: współrzędna na planszy plus wartość z łamigłówki ile pól ma być zaciemnonych
-- np. (1,1), 3
type CoordValue = (Coordinate, Int)

-- CoordColoured: współrzędna na planszy plus oznaczenie czy powinna być pokolorowana
type CoordColoured = (Coordinate, Bool)

-- Creek: plansza gry, tzn. jej rozmiar i pola do zaciemnienia
data Creek = Creek BoardSize [CoordValue]
    deriving Show

-- SolvedBoard: rozwiązana plansza
type SolvedBoard = [CoordColoured]

--sample = Creek (4, 4) [((1, 0), 1), ((1, 2), 2), ((2, 1), 4), ((2, 3), 1), ((3, 3), 1), ((4, 1), 0)]
sampel = Creek (4,4) []

-- validate the board
-- TODO
validateBoard :: Creek -> Bool
validateBoard (Creek _ []) = True
validateBoard (Creek _ _) = False

-- get size of the board
boardSize :: Creek -> BoardSize
boardSize (Creek size _) = size

-- get x size of the board
xBoardSize :: Creek -> Int
xBoardSize (Creek (x, _) _) = x

-- get y size of the board
yBoardSize :: Creek -> Int
yBoardSize (Creek (_, y) _) = y

-- get y size of solvedBoard
-- TODO
ySolveBoardSize :: SolvedBoard -> Int
ySolveBoardSize [] = 0
ySolveBoardSize board = snd (fst (last board))

-- empty solve board
emptySolveBoard :: Creek -> SolvedBoard
emptySolveBoard (Creek (0, _) _) = []
emptySolveBoard (Creek (_, 0) _) = []
emptySolveBoard (Creek (x, y) _) = [ ((a, b), False) | a <- [0..x], b <- [0..y]]

-- prepare empty board for results and solve game
-- TODO
solveBoard :: Creek -> SolvedBoard -> SolvedBoard
solveBoard (Creek (0, _) _) _ = []
solveBoard (Creek (_, 0) _) _ = []
solveBoard (creek ( emptySolveBoard creek )) = []

-- convert solution board single row to list of Strings
-- FIX (written badly)
boardToString :: SolvedBoard -> Int -> String
boardToString [] _ = ""
boardToString board row = 
    if (row <= ySolveBoardSize board)
        then ( concat list ) ++ "\n" ++ ( boardToString board (row + 1) )
        else ""
    where list = [ mapSolveValue (snd a) ++ " | " | a <- board, snd (fst a) == row ]

-- map True with "x" - mark on the board and False with space
mapSolveValue :: Bool -> String
mapSolveValue True = "x"
mapSolveValue False = " "

-- print solution on screen
printBoard::SolvedBoard -> IO()
printBoard [] = putStrLn "Couldn't find solution."
printBoard board = putStrLn (boardToString board 0)

