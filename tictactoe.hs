data Marker = O 
            | X
            deriving (Eq)

data Square = Empty
            | Marked Marker
            deriving (Eq)

instance Show (Marker) where
    show O = "O"
    show X = "X"

instance Show (Square) where
    show Empty = " "
    show (Marked O) = "O"
    show (Marked X) = "X"

otherPlayer :: Marker -> Marker
otherPlayer O = X
otherPlayer X = O

renderRow :: [Square] -> String
renderRow xs = show (xs !! 0) ++ " | " ++ show (xs !! 1) ++ " | " ++ show (xs !! 2)

renderGame :: [Square] -> IO ()
renderGame xs = do
    putStrLn $ renderRow xs
    putStrLn "----------"
    putStrLn $ renderRow (drop 3 xs)
    putStrLn "----------"
    putStrLn $ renderRow (drop 6 xs)

placeMarker :: Int -> Square -> [Square] -> [Square]
placeMarker i mark (x:xs) | i == 0 = mark : xs
                          | otherwise = x : placeMarker (i-1) mark xs

checkVictory :: [Square] -> Marker -> Bool
checkVictory gameBoard mark = 
    or [
        gameBoard !! 0 == (Marked mark) && gameBoard !! 1 == (Marked mark) && gameBoard !! 2 == (Marked mark),
        gameBoard !! 3 == (Marked mark) && gameBoard !! 4 == (Marked mark) && gameBoard !! 5 == (Marked mark),
        gameBoard !! 6 == (Marked mark) && gameBoard !! 7 == (Marked mark) && gameBoard !! 8 == (Marked mark),
        gameBoard !! 2 == (Marked mark) && gameBoard !! 5 == (Marked mark) && gameBoard !! 8 == (Marked mark),
        gameBoard !! 1 == (Marked mark) && gameBoard !! 4 == (Marked mark) && gameBoard !! 7 == (Marked mark),
        gameBoard !! 0 == (Marked mark) && gameBoard !! 3 == (Marked mark) && gameBoard !! 6 == (Marked mark),
        gameBoard !! 2 == (Marked mark) && gameBoard !! 4 == (Marked mark) && gameBoard !! 6 == (Marked mark),
        gameBoard !! 0 == (Marked mark) && gameBoard !! 4 == (Marked mark) && gameBoard !! 8 == (Marked mark)
    ]

checkTie :: [Square] -> Bool
checkTie gameBoard | any (== Empty) gameBoard = False
                   | otherwise = True

playGame :: [Square] -> Marker -> IO ()
playGame gameBoard mark = do
    renderGame gameBoard
    if show mark == "X" then putStrLn "Player 1's turn, input an integer from 1 to 9 to place an X!"
    else putStrLn "Player 2's turn, input an integer from 1 to 9 to place an O!"
    input <- getLine
    let selection = read input :: Int
    if gameBoard !! (selection - 1) == Empty then
        do
        let updatedGameBoard = (placeMarker (selection - 1) (Marked mark) gameBoard)
        if checkVictory updatedGameBoard mark then
            do 
            if show mark == "X" then putStrLn "\nThree in a row! Player 1 wins!"
            else putStrLn "\nThree in a row! Player 2 wins!"
            renderGame updatedGameBoard
            return ()
        else if checkTie updatedGameBoard then
            do
            putStrLn "\nCat's game! Neither player wins!"
            renderGame updatedGameBoard
            return ()
        else playGame updatedGameBoard (otherPlayer mark)
    else 
        do
        putStrLn "\n!!! That square is already marked, choose a different one !!!\n"
        playGame gameBoard mark

tictactoe :: IO ()
tictactoe = do
    let gameBoard = [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
    playGame gameBoard X
