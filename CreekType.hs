module CreekType where

-- BoardSize: (szerokosc, wysokosc) rozmiar planszy
type BoardSize = (Int, Int)

-- Coordinates: współrzędna na planszy, np. (1,1)
type Coordinate = (Int, Int)

-- CoordValue: współrzędna na planszy plus wartość z łamigłówki ile pól ma być zaciemnonych
-- np. (1,1), 3
type CoordValue = (Coordinate, Int)

-- Creek: plansza gry, tzn. jej rozmiar i pola do zaciemnienia
data Creek = Creek BoardSize [CoordValue]
    deriving (Show, Read)

-- CoordColoured: współrzędna na planszy plus oznaczenie czy powinna być pokolorowana
type CoordColoured = (Coordinate, Bool)

-- SolvedBoard: rozwiązana plansza
type SolvedBoard = [CoordColoured]
