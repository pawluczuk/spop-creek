module Creek where
import System.IO
import System.IO.Error
import Control.Exception
import CreekType
import Game
import PrintBoard

-- obsluga gry
creek :: IO ()
creek = do
    putStrLn "\n####################################################"
    putStrLn "\nWpisz 's' aby rozpoczac gre lub 'w' aby wyjsc:"
    cmd <- getChar
    case cmd of
        'w'     ->  return()
        's'     ->  do  putStrLn "\n####################################################"
                        putStrLn "Rozpoczynam gre. Prosze o podanie pliku wejsciowego: "
                        fname <- getLine
                        putStrLn ("Odczytywanie pliku: " ++ fname)
                        doRead fname
                        creek
        _       ->  creek

-- odczytywanie pliku wejsciowego
doRead :: String -> IO ()
doRead fname =
  catch (do handle      <- openFile fname ReadMode
            contents    <- hGetContents handle
            let args    = read contents :: Creek
            eVal <- try (print args) :: IO (Either SomeException ())
            case eVal of
                Left  _    -> putStrLn "Niepoprawny format pliku."
                Right ()  ->  doSolveGame args
            hClose handle
        ) errorHandler
        where
          errorHandler e =
            if ( openingError e )
            then putStrLn ("Nie udalo sie otworzyc pliku: " ++ fname ++ ".")
            else return ()

-- rozwiazanie gry jesli udalo sie odczytac plik
doSolveGame :: Creek -> IO ()
doSolveGame args = do
    let size    = boardSize args
        res     =  solveCreek args
    case (solveCreek args) of
        Left err -> putStrLn err
        Right res -> printBoard res size

-- obsluga roznych rodzajow bledow otwierania pliku
openingError :: IOError -> Bool
openingError e = isDoesNotExistError e || isAlreadyInUseError e 
                || isPermissionError e || isEOFError e


