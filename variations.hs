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

-- wyznacz pola z planszy ktore sa brane pod uwage przy malowaniu
-- dla przeciecia w miejscu (x,y) dla planszy o rozmiarach (m, n)
-- np. possibleCoord (0,1) (2,2) => [(1,1),(1,2)]
possibleCoord :: Coordinate -> BoardSize -> [Coordinate]
possibleCoord (x, y) (m, n) = 
    [ (a, b) | a <- [x, x + 1], b <- [y, y + 1], a > 0, b > 0, a <= m, b <= n ]

-- wyznacz wszystkie mozliwosci n elementów z podanej listy, np.
-- combinations 2 [1,2,3] => [[1,2],[1,3],[2,3]]
-- combinations 3 [(1,1), (1,2), (2,1), (2,2)] 
-- => [[(1,1),(1,2),(2,1)], [(1,1),(1,2),(2,2)], [(1,1),(2,1),(2,2)], [(1,2),(2,1),(2,2)]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

-- wyznacz wszystkie możliwe kombinacje pol, ktore moga byc pomalowane
-- jezeli nalezy zamalowac k pol wokol przeciecia w miejscu coord 
-- dla planszy o rozmiarze size, np.
-- possibleCombinations ((1,2), 1) (2,2) => [[(1,2)],[(2,2)]]
possibleCombinations :: CoordValue -> BoardSize -> [[Coordinate]]
possibleCombinations ( coord, k ) size = combinations k (possibleCoord coord size)

-- uzupelnij mozliwe kombinacje zamalowanych pol o informacje,
-- ktore pola maja zostac niepomalowane, np.
-- possibleCombinations' [[(1,2)],[(2,2)]] (1,2) (2,2) => 
-- [ [((1,2), True), ((2,2), False)], [((1,2), False), ((2,2), True)] ]
possibleCombinations' :: [[Coordinate]] -> Coordinate -> BoardSize -> [[CoordColoured]]
possibleCombinations' [] _ _ = []
possibleCombinations' arr coord size = map (colourValues coord size) arr

-- dla jednej kombinacji pol uzupelnij informacje, 
-- ktore pola pozostaja niepomalowane, np.
-- colourValues [(1,1),(1,2),(2,1)] (1,1) (2,2) => 
-- [ ((1,1), True), ((1,2), True),((2,1), True),((2,2), False)]
colourValues :: Coordinate -> BoardSize -> [Coordinate] -> [CoordColoured]
colourValues coord size arr = [ (x, (x `elem` arr)) | x <- (possibleCoord coord size)]

-- sprawdz czy zamalowanie pola nie koliduje z innymi zamalowaniami z listy
-- jesli w liscie bedzie element o tych samych wspolrzednych, ale inne zamalowanie
-- zwroc False. True jesli element mozna dolaczyc do listy wymagan
combineEl :: [CoordColoured] -> CoordColoured -> Bool
combineEl []  _ = True
combineEl (y:ys) x      | x `elem` (y:ys)                               = True
                        | ((fst x) == (fst y)) &&  ((snd x) /= (snd y)) = False 
                        | otherwise                                     = combineEl ys x

-- z dwóch list kombinacji pól utwórz jedną
combineArr :: [CoordColoured] -> [CoordColoured] -> [CoordColoured]
combineArr [] x = x
combineArr x [] = x
combineArr x y
    | False `elem` res      = []
    | otherwise             =  removeDups (x ++ y)
    where   res = map (combineEl longer) shorter
            longer = getLonger x y
            shorter = getShorter x y

-- Dla dwoch wymagan wyznacz wszystkie mozliwe kombinacje
combineReqs :: [[CoordColoured]] -> [[CoordColoured]] -> [[CoordColoured]]
combineReqs x y = removeEmpty [ combineArr a b | a <- x, b <- y]


-- Wyznacz mozliwe kombinacje dla kazdego z wymagan
readReqs :: Creek -> [[[CoordColoured]]]
readReqs (Creek (0, _) _)   = []
readReqs (Creek (_, 0) _)   = []
readReqs (Creek (_, _) [])  = []
readReqs (Creek size (x:xs)) =
    [ possibleCombinations' combinations coord size ] ++ readReqs (Creek size xs)
    where   combinations = possibleCombinations x size
            coord = fst x

-- Połącz wszystkie mozliwe kombinacje wszystkich wymagan
combineAllReq :: [[[CoordColoured]]] -> [[[CoordColoured]]]
combineAllReq []    = []
combineAllReq (x:[])  = []
combineAllReq (x:y:ys) = [combineReqs x y] ++ combineAllReq (y:ys)

-- FUNKCJE POMOCNICZE

-- zwroc dluzsza liste sposrod dwoch
getLonger :: [a] -> [a] -> [a]
getLonger x y   | length x > length y = x
                | otherwise           = y

 -- zwroc krotsza liste sposrod dwoch
getShorter :: [a] -> [a] -> [a]
getShorter x y  | length x <= length y  = x
                | otherwise             = y

-- usuniecie duplikatow z listy
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : (removeDups [ a | a <- xs, a /= x])

-- usuniecie pustych list z listy
removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty [] = []
removeEmpty x = [ a | a <- x, a /= []]

size = (4,4)
reqs = [((1, 0), 1), ((1,2),2), ((2,1), 4), ((2,3), 1), ((3,3), 1), ((4,1), 0)]
allReqs = readReqs (Creek size reqs)


