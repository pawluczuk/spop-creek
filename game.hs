module Game where
import CreekType

-- WYZNACZANIE KOMIBNACJI ZAMALOWANIA POL WEDLUG CYFR NA PRZECIECIACH

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
combinations 0 _    = [[]]
combinations _ []   = []
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
    where   res     = map (combineEl longer) shorter
            longer  = getLonger x y
            shorter = getShorter x y

-- Dla dwoch wymagan wyznacz wszystkie mozliwe kombinacje
combineReqs :: [[CoordColoured]] -> [[CoordColoured]] -> [[CoordColoured]]
combineReqs x y = removeEmpty [ combineArr a b | a <- x, b <- y]

-- Polacz wszystkie mozliwe kombinacje wszystkich wymagan
combineAllReq :: [[[CoordColoured]]] -> [[CoordColoured]]
combineAllReq []        = []
combineAllReq (x:[])    = []
combineAllReq (x:xs)    = foldl (combineReqs) x xs

-- Uzupelnij mozliwe kombinacje wymagan o pola nieuwzglednione w wymaganiach
boardCombinations :: [[CoordColoured]] -> BoardSize -> [[CoordColoured]]
boardCombinations [] _          = []
boardCombinations boards size   = concat ( map (fillBoard size) boards)

-- Uzupelnij pojedyncza plansze o nieuwzglednione w wymaganiach pola
fillBoard :: BoardSize -> [CoordColoured] -> [[CoordColoured]]
fillBoard _ []      = []
fillBoard size board = [ board ++ a | a <- combinations]
    where   combinations = combineLeftCells' left
            left = leftCells board size

-- Wyznacz mozliwe kombinacje dla kazdego z wymagan
readReqs :: Creek -> [[[CoordColoured]]]
readReqs (Creek (0, _) _)   = []
readReqs (Creek (_, 0) _)   = []
readReqs (Creek (_, _) [])  = []
readReqs (Creek size (x:xs)) =
    [ possibleCombinations' combinations coord size ] ++ readReqs (Creek size xs)
    where   combinations = possibleCombinations x size
            coord = fst x

-- UZUPELNIENIE WOLNYCH POL

-- Polacz wszystkie mozliwe kombinacje wszystkich wymagan
combineLeftCells' :: [Coordinate] -> [[CoordColoured]]
combineLeftCells' [] = []
combineLeftCells' x = foldl (combineLeftCells) [] x

-- Polacz nowe puste pole z poprzednimi kombinacjami wolnych pol
combineLeftCells :: [[CoordColoured]] -> Coordinate -> [[CoordColoured]]
combineLeftCells [] coord   = [ [(coord, a)] | a <- [True,False]]
combineLeftCells x coord    = [ a ++ [(coord, b)] | a <- x, b <- [True, False]]

-- Znajdz wolne pola ktore moga miec dowolna wartosc
leftCells :: [CoordColoured] -> BoardSize -> [Coordinate]
leftCells [] (m,n)      = [ (a,b) | a <- [1..m], b <- [1..n]]
leftCells board (m,n)   = [ (a,b) | a <- [1..m], b <- [1..n], (isElement (a,b) board) == False]

-- WYZNACZANIE PLANSZY ZE STRUMIENIEM NIEZAMALOWANYCH POL

-- sposrod kombinacji pol spelniajacych wymagania na przecieciach, wybiera te 
-- ktorych niezamalowane pola tworza strumien
getCreek :: [[CoordColoured]] -> [[CoordColoured]]
getCreek []     = []
getCreek boards = [ a | a <- boards, hasCreek a == True]

-- sprawdza czy na planszy niezamalowane pola tworza strumien
hasCreek :: [CoordColoured] -> Bool
hasCreek []     = True
hasCreek board 
    | (length whiteEl) > 0  = (compareCreek creek whiteEl)
    | otherwise             = True
    where   whiteEl = getWhite board
            creek   = singleCreek board (whiteEl !! 0)

-- sprawdza czy strumien zawiera wszystkie biale pola na planszy
compareCreek :: [CoordColoured] -> [CoordColoured] -> Bool
compareCreek _ [] = True
compareCreek [] _ = False
compareCreek (x:xs) board = compareCreek xs [ a | a <- board, a /= x ]


-- znajduje wszystkich kolejnych sasiadow zaczynajac od jednego bialego pola
-- tzn. wszystkie pola z jednego strumienia
singleCreek :: [CoordColoured] -> CoordColoured -> [CoordColoured]
singleCreek [] _     = []
singleCreek board x  = x : neighbours ++ (concat (map (singleCreek newBoard) neighbours))
    where   neighbours = getWhiteNeighbours x board
            newBoard = [ a | a <- board, isNeighbour a x == False, a /= x ]

-- FUNKCJE POMOCNICZE

-- zwroc niezamalowane elementy planszy
getWhite :: [CoordColoured] -> [CoordColoured]
getWhite [] = []
getWhite board = [ a | a <- board, snd a == False]

-- czy dwa punkty na planszy sasiaduja ze soba
isNeighbour :: CoordColoured -> CoordColoured -> Bool
isNeighbour ((a,b), _) ((c,d), _)
    | (a == c) && ( (b == (d-1)) || (b == (d+1) ))  = True
    | (b == d) && ( (a == (c-1)) || (a == (c+1) ))  = True
    | otherwise                                     = False

-- zwroc sasiadujace niezamalowane elementy
getWhiteNeighbours :: CoordColoured -> [CoordColoured] -> [CoordColoured]
getWhiteNeighbours ((m,n), _) board = 
    [ a | a <- board, 
        snd a == False, 
        fst a `elem` (filter (>= (1,1)) [(m, n-1), (m, n+1), (m-1, n), (m+1,n)])]

-- zwroc elementy z listy ktore maja podane wsporzedne
getElement :: Coordinate -> [CoordColoured] -> [CoordColoured]
getElement _ []         = []
getElement coord board  = [ a | a <- board, fst a == coord]

-- sprawdz czy podane wspolrzedne sa uwzglednione w planszy
isElement :: Coordinate -> [CoordColoured] -> Bool
isElement _ [] = False
isElement coord board
    | (length arr) > 0  = True
    | otherwise         = False
    where arr = [ a | a <- board, fst a == coord]

-- Generuj pusta plansze wypelniona niezamalowanymi polami
emptyBoard :: Coordinate -> [CoordColoured]
emptyBoard (m, n)
    | m < 1 || n < 1    = []
    | otherwise         = [ ((a,b), False) | a <- [1..m], b <- [1..n]]

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
removeDups []       = []
removeDups (x:xs)   = x : (removeDups [ a | a <- xs, a /= x])

-- usuniecie pustych list z listy
removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty []      = []
removeEmpty x       = [ a | a <- x, a /= []]

-- rozmiar planszy z jej konstruktora
boardSize :: Creek -> BoardSize
boardSize (Creek size _)   = size

-- sprawdzenie czy wspolrzedne wymagan na przecieciach
-- nie sa wieksze niz rozmiar planszy
validateReqs :: BoardSize -> [CoordValue] -> Bool
validateReqs (m, n) reqs
    | (length outRange) > 0     = False
    | otherwise                 = True
    where outRange = [ a | a <- reqs, ((fst (fst a)) > m || (snd (fst a)) > n || snd a < 0 || snd a > 4)] 

-- ROZPOCZECIE GRY

solveCreek :: Creek -> Either String [CoordColoured]
solveCreek (Creek (0, _) _) = Left "Niepoprawny rozmiar planszy"
solveCreek (Creek (_, 0) _) = Left "Niepoprawny rozmiar planszy"
solveCreek (Creek size [])  = Right (emptyBoard size)
solveCreek (Creek size reqs) 
    | (validateReqs size reqs) == False = Left "Niepoprawne wymagania."
    | (length creeks)  > 0              = Right (creeks !! 0)
    | otherwise                         = Left "Nie znaleziono jednoznacznego rozwiazania."
    where   creeks  = getCreek boards
            boards  = boardCombinations res size 
            res     = combineAllReq req
            req     = readReqs (Creek size reqs)
