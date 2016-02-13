module PrintBoard where
import CreekType

-- RYSOWANIE PLANSZY
-- rysuj planszę (od i-tego wiersza)
boardToString :: [CoordColoured] -> BoardSize -> Int -> String
boardToString [] _ _            = ""
boardToString _ (0, 0) _        = []
boardToString board (m, n) i 
    | i <= m    = (rowToString row (m,n) 1) ++ (boardToString board (m,n) (i+1))
    | otherwise = ""
    where row = [ a | a <- board, fst (fst a) == i ]

-- rysuj wiersz (od i-tej kolumny w wierszu)
rowToString :: [CoordColoured] -> BoardSize -> Int -> String
rowToString [] _ _ = []
rowToString row (m, n) i
    | i <= n    = printElement [ a | a <- row, snd (fst a) == i ] 
                    ++ " | " 
                    ++ rowToString row (m, n) (i+1)
    | otherwise = "\n" 

-- rysuj element 
printElement :: [CoordColoured] -> String
printElement [] = "⬜️"
printElement [x]
    | snd x == True     = "⬛️"
    | otherwise         = "⬜️"

-- wydrukuj rozwiazanie
printBoard :: [CoordColoured] -> BoardSize -> IO()
printBoard [] _         = putStrLn "Couldn't find solution."
printBoard board size   = putStrLn (boardToString board size 1)