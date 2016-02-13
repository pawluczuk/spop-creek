# spop-creek
Haskell Creek game (university project)
# Project structure
Game consists of few files:
* Creek.hs - resposnible for interface starting a game and handling file reading
* CreekType.hs - lists all created and used custom data types
* Game.hs - game logic
* PrintBoard.hs - printing board to output

There are some correct and incorrect text files included (starting with input* and mellow*).

# Loading and starting game
Go to project folder and start ghci in your terminal. Load game modules: `:l Creek`, then start game with command `creek`.
Game interface asks for chosen character either to start a game ("s") or to exit ("w").

# Algorithm
## Reading a file
After typing "s" and starting a game, program will ask for input file. Proper format should be like this:
`Creek (5,5) [((0,4),1), ((1,2), 2), ((2,1),3), ((2,4), 4), ((3,2), 1), ((4,1),2), ((4,2),1), ((4,3),2), ((4,5), 0)]`
If input file is correct (proper format, board size > 0 and requriements not exceeding board), program finds a solution (if exists) and prints it on the screen.
## Finding a solution
* Finding all field combinations for all requriements from input file.
* Finding all combinations of fields that have not been affected by requriements (can be either coloured or not)
* Combining boards from two previous points and finding the combination in which white fields are a creek