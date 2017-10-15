import Data.List
import Data.Maybe

{-

Connect 4 in haskell!

Model => List of lists of ints. 0 is blank, 1 is player 1, 2 is player 2.

Functions we need
----------------------------
> Show the board [CHECK]
> add a tile given the board and the player and column [CHECK]
> check if game is over (and output winner)

-}
exampleBoard1 :: [[Int]]
exampleBoard1 = [[1,1,2,1],[1,1,1,1],[1,1,1,2],[1,1,0,0],[1,0,0,0]]
exampleBoard2 :: [[Int]]
exampleBoard2 = makeTwoDimensionalZeroes 5 5

makeTwoDimensionalZeroes ::  Int -> Int -> [[Int]]
makeTwoDimensionalZeroes len wid = take len (repeat (take wid (repeat 0 )))

pieceDisplay :: (Eq a, Num a) => [Char] -> a -> [Char]
pieceDisplay prev n
    | (n == 0) = prev ++ "| "
    | (n == 1) = prev ++ "|X"
    | (n == 2) = prev ++ "|O"

rowDrawer :: (Foldable t, Num a, Eq a) => t a -> [Char]
rowDrawer row = foldl pieceDisplay "" row ++ "|" ++ "\n"

displayer :: (Foldable t1, Foldable t, Num a, Eq a) => t1 (t a) -> [Char]
displayer xs = (foldl (\x y->rowDrawer y ++ x) "" xs)

droptile' :: [Int] -> Int -> [Int]
droptile' [] player = error "This stack is full!"
droptile' (x:xs) player
    |(x == 0) = player : xs
    | otherwise = x : droptile' xs player

droptilefirst :: [[Int]] -> Int -> [[Int]]
droptilefirst board column = fst (splitAt column (transpose board))

droptilemoved :: [[Int]] -> Int -> Int -> [Int]
droptilemoved board column player = droptile' ((take 1 (snd (splitAt column (transpose board)))) !! 0) player

droptilelast :: [[Int]] -> Int -> [[Int]]
droptilelast  board column = drop 1 (snd (splitAt column (transpose board)))

droptile :: [[Int]] -> Int -> Int -> [[Int]]
droptile board column player = transpose (droptilefirst board column ++ [droptilemoved board column player] ++ droptilelast board column)


fourEquals :: [Int] -> Maybe [Char]
fourEquals [1,1,1,1] =  Just "Player1"
fourEquals [2,2,2,2] =  Just "Player2"
fourEquals xs = Nothing

helpCheck :: [Int] -> [Char]
helpCheck [] = "No Winner!"
helpCheck [x] = "No Winner!"
helpCheck [x,y] = "No Winner!"
helpCheck [x,y,z] = "No Winner!"
helpCheck (a:b:c:d:xs)
    | ((fourEquals [a,b,c,d]) == Just "Player1") = "Player1"
    | ((fourEquals [a,b,c,d]) == Just "Player2") = "Player2"
    | otherwise = helpCheck (b:c:d:xs)

foldCheck :: [[Char]] -> [Char]
foldCheck [] = "No Winner!"
foldCheck (x:xs)
    | (x == "No Winner!") = foldCheck xs
    | otherwise = x

rowCheck :: [[Int]] -> [Char]
rowCheck xs = foldCheck (map helpCheck xs)

horizontalCheck :: [[Int]] -> [Char]
horizontalCheck xs = foldCheck (map helpCheck (transpose xs))

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

leftDiagonal :: [[Int]] -> [Char]
leftDiagonal xs = foldCheck (map helpCheck (diagonals xs))

rightDiagonal :: [[Int]] -> [Char]
rightDiagonal xs = foldCheck $ map helpCheck $ diagonals $ map reverse xs

winner :: [[Int]] -> IO ()
winner xs
    | elem "Player1" [horizontalCheck xs, rowCheck xs, leftDiagonal xs, rightDiagonal xs] = putStrLn "Player 1 wins!"
    | elem "Player2" [horizontalCheck xs, rowCheck xs, leftDiagonal xs, rightDiagonal xs] = putStrLn "Player 2 wins!"
    | otherwise = putStrLn "No Winner yet!"


{--
User Interface plan for Connect Four

event_loop:
* draw board
* is it won?
    - print the win message
* not won?
    - get a move
    - event loop with updated board and next player
--}

type Board = [[Int]]
drawBoard = displayer
is_won = (\x -> False)
make_move board t int = (\x -> droptile board int t)
is_move_legal = (\board col-> elem 0 (board!!col))
main :: IO()
main = do
  let board = make_board 8 6
  event_loop board 1

event_loop :: Board -> Int -> IO()
event_loop board player = do
  putStrLn $ drawBoard board

  if is_won board then do
    win_message board player
    return ()
  else do
    col <- get_move player
    handle_move board player col

make_board :: Int -> Int -> Board
make_board cols rows =
    [ [0 | _ <- [1..cols] ] | _ <- [1..rows] ]

next_ player = 3 - player

get_move player = do
  putStrLn $ "(Enter -99 to quit.)"
  putStrLn $ "Player " ++(show player)++" moves."
  putStr $ "Column [0-7]? "
  x <- getLine
  return (get_number x)

-- get_number returns -1 for any invalid input
get_number :: String -> Int
get_number colIn
    = case (reads colIn)::[(Int,String)] of
        [(colnum, "")] -> colnum
        _              -> -1

handle_move board player col
    | col == -99              = goodbye
    | is_move_legal board col = event_loop new_board (next_ player)
    | otherwise = complain_and_restart
    where complain_and_restart = do
              putStrLn "ERROR: That is not a legal move."
              event_loop board player
          new_board = droptile board col player
          goodbye = do putStrLn "You quit"

win_message board player = do
    putStrLn $ "The game is over"
    putStrLn $ "Player "++(show $ next_ player)++" won!"
    -- note: win computed at the start of the next turn
    -- so current player is the loser
