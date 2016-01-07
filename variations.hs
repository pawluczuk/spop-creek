-- BoardSize: (szerokosc, wysokosc) rozmiar planszy
type BoardSize = (Int, Int)

-- Coordinates: współrzędna na planszy, np. (1,1)
type Coordinate = (Int, Int)

-- CoordValue: współrzędna na planszy plus wartość z łamigłówki ile pól ma być zaciemnonych
-- np. (1,1), 3
type CoordValue = (Coordinate, Int)

-- Creek: plansza gry, tzn. jej rozmiar i pola do zaciemnienia
data Creek = Creek BoardSize [CoordValue]
    deriving Show

-- CoordColoured: współrzędna na planszy plus oznaczenie czy powinna być pokolorowana
type CoordColoured = (Coordinate, Bool)

-- SolvedBoard: rozwiązana plansza
type SolvedBoard = [CoordColoured]

-- get coordinates from which we can variate
possibleCoord :: Coordinate -> BoardSize -> [Coordinate]
possibleCoord (x, y) (m, n) = 
    [ (a, b) | a <- [x, x + 1], b <- [y, y + 1], a > 0, b > 0, a <= m, b <= n ]

-- get list of possible variations
-- k pol zaciemnionych (z wartoscia True)
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- get possible combinations for requirement
possibleCombinations :: CoordValue -> BoardSize -> [[Coordinate]]
possibleCombinations ( coord, k ) size = combinations k (possibleCoord coord size)

-- get possible combinations for requirement with non coloured values
possibleCombinations' :: [[Coordinate]] -> Coordinate -> BoardSize -> [[CoordColoured]]
possibleCombinations' [] _ _ = []
possibleCombinations' arr coord size = map (colourValues coord size) arr
-- possibleCombinations' x:xs coord size =
--    colourValues x coord size : possibleCombinations' xs coord size

colourValues :: Coordinate -> BoardSize -> [Coordinate] -> [CoordColoured]
colourValues coord size arr = [ (x, (x `elem` arr)) | x <- (possibleCoord coord size)]

-- combine all requirements
-- combineReqs :: [[Coordinate]] -> [[Coordinate]] -> [[Coordinate]]
-- combineReqs x y = [ combine2Req a b | a <- x, b <- y]

size = (2,2)
aReq = ((1,1), 3)
bReq = ((1,2), 1)
a = possibleCombinations aReq size
b = possibleCombinations bReq size

possibleCombinations' a (fst aReq) size

