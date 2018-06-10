{-#LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE DataKinds #-}
import Data.List
import Data.Char
import Data.String
import Data.Maybe
import Control.Monad
import Control.Exception
import Data.Time
import Data.Maybe (listToMaybe)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

data Player = Human | Computer deriving (Eq)
data GameTree a = a :> [GameTree a] deriving (Show) 

data Field = Empty | Wolf | Sheep | Inaccesible deriving (Show, Eq) 
data Board = Board [Field] deriving (Show) 
data Position = Position (Int, Int) deriving (Eq, Show)

printBoard :: Board -> [Char]
printBoard board = ("  1 2 3 4 5 6 7 8" ++ (printBoard' 0 board))

displayField :: Field -> [Char]
displayField Wolf   = "w"
displayField Sheep  = "s"
displayField Empty  = " "

printBoard' :: Int -> Board -> [Char]
printBoard' _ (Board []) = "\n"
printBoard' index (Board (x:xs)) 
    | mod index 8 == 0 = ("\n" ++ show(num_y) ++"|" ++ (displayField x) ++ "|" ++ (printBoard' (index + 1) (Board xs)))
    | otherwise = ((displayField x) ++ "|" ++ (printBoard' (index + 1) (Board xs)))
    where num_y = ((div index 8) + 1)


getIndex :: Position -> Int
getIndex (Position (x, y)) = y*8+x

getY :: Position -> Int
getY (Position (x, y)) = y

getPosition :: Int -> Position
getPosition n = Position (mod n 8, quot n 8)

getField :: Board -> Position -> Field
getField (Board board) pos = board!!(getIndex pos)

getFieldByIndex :: Board -> Int -> Field
getFieldByIndex (Board board) index = board!!index

checkIfFieldEmpty :: Board -> Position -> Bool
checkIfFieldEmpty board pos = (getField board pos) == Empty

chooseField :: Position -> [Position] -> Position -> Field
chooseField wolf sheeps pos = 
    if wolf == pos
        then Wolf
        else if elem pos sheeps
            then Sheep
            else Empty

cartProd :: [Int] -> [Int] -> [Position]
cartProd xs ys = [Position (y, x) | x <- xs  , y <- ys ]

generateBoard ::  Position -> [Position] -> Board
generateBoard wolf sheeps = Board (map (chooseField wolf sheeps) (cartProd[0..7] [0..7]))


getWolfPosition :: Board -> Position
getWolfPosition (Board board) = getPosition $ (elemIndices Wolf board)!!0

getSheepPositions :: Board -> [Position]
getSheepPositions (Board board) = map getPosition $ elemIndices Sheep board

isInBoard :: Position -> Bool
isInBoard (Position (x, y))
    | x < 0 || x > 7 = False
    | y < 0 || y > 7 = False
    | otherwise = True

getPossibleSheepPositions :: Position -> [Position]
getPossibleSheepPositions (Position (x, y)) = filter isInBoard [Position (x+1, y+1), Position (x-1, y+1)] 

getPossibleWolfPositions :: Position -> [Position]
getPossibleWolfPositions (Position (x, y)) = filter isInBoard [Position (x-1, y-1), Position (x+1, y-1), Position (x+1, y+1), Position (x-1, y+1)]


getPossibleBoards :: Board -> Player -> [Board]
getPossibleBoards board Computer = getPossibleWolfBoards board
getPossibleBoards board Human = getPossibleSheepBoards board

removeItem :: Int -> [Position] -> [Position]
removeItem _ [] = []
removeItem x (y:ys) 
    | x == 0    = removeItem (x - 1) ys
    | otherwise = y : removeItem (x - 1) ys

getPossibleWolfBoards :: Board -> [Board]
getPossibleWolfBoards board = 
    let wolfPositions = filter (checkIfFieldEmpty board) $ getPossibleWolfPositions $ getWolfPosition board
        sheepPositions = getSheepPositions board
    in [generateBoard wolfPosition sheepPositions  | wolfPosition <- wolfPositions]

mapPossibleSheepPositions ::  [Position] -> [Position] -> [[Position]]
mapPossibleSheepPositions possibleSheepPositions currentSheepPositions = map (\n -> n : currentSheepPositions) possibleSheepPositions

removeDuplicatedPositions :: Int -> [[Position]] -> [Position] -> [[Position]]
removeDuplicatedPositions index [] _ = [[]]
removeDuplicatedPositions index [x] currentSheepPositions  = mapPossibleSheepPositions x (removeItem index currentSheepPositions)
removeDuplicatedPositions index (x:xs) currentSheepPositions =  mapPossibleSheepPositions x (removeItem index currentSheepPositions) ++ (removeDuplicatedPositions (index + 1) xs currentSheepPositions)

getPossibleSheepBoards :: Board -> [Board]
getPossibleSheepBoards board = 
    let currentSheepPositions = getSheepPositions board
        possibleSheepPositions = map (\n -> filter (checkIfFieldEmpty board) $ getPossibleSheepPositions n) $ currentSheepPositions
        wolfPosition = getWolfPosition board
    in [generateBoard wolfPosition sheepPosition  | sheepPosition <- (removeDuplicatedPositions 0 possibleSheepPositions currentSheepPositions)]


    
next :: Player -> Player
next Human = Computer
next Computer = Human 

betaMax = -100
alfaMax = 100

getSheepsBehind :: Position -> [Position] -> Int
getSheepsBehind (Position(wx, wy)) [] = 0
getSheepsBehind (Position(wx, wy)) (x:xs) = 
    if(wy <= (getY x))
        then 1 + (getSheepsBehind (Position(wx, wy)) xs )
        else getSheepsBehind (Position(wx, wy)) xs 

gameStatusEvaluation :: Board -> Int
gameStatusEvaluation board = 
    let Position (wx, wy) = getWolfPosition board
        wolfPossibleMoves = length (getPossibleWolfBoards board)
        sheepPositions = getSheepPositions board
        sheepBehind = getSheepsBehind (Position(wx, wy)) sheepPositions
    in 
        if (wy == 0)
            then
                alfaMax
            else if (wolfPossibleMoves == 0)
                then 
                    betaMax
                else 
                    sheepBehind * 15 + wolfPossibleMoves * 5 + (3 - wy) * 5
alphaBeta :: Int -> Board -> Board
alphaBeta depth board = snd (alphaBeta' depth betaMax alfaMax Computer board)

alphaBeta' :: Int -> Int -> Int -> Player -> Board -> (Int, Board)
alphaBeta' depth alfa beta player board
    | depth == 0 = (gameStatusEvaluation board, board)
    | (gameStatusEvaluation board) == alfaMax && player == Computer = (alfaMax, board)
    | (gameStatusEvaluation board) == betaMax && player == Computer = (betaMax, board)
    | otherwise = 
        let (value, funcExtreme) = if player == Computer then (alfa, max) else (beta, min)
            helper alfa beta v [] lastBoard = (v, lastBoard)
            helper alfa beta v (x:xs) lastBoard =
                let v' = alphaBeta' (depth - 1) alfa beta (next player) x
                    newValue = funcExtreme v (fst v')
                    newX = if newValue == v 
                        then lastBoard
                        else x
                    (newAlfa, newBeta) = if player == Computer
                            then (funcExtreme alfa newValue, beta)
                            else (alfa, funcExtreme beta newValue)
                in if newBeta <= newAlfa
                    then (newValue, newX)
                    else helper newAlfa newBeta newValue xs newX
        in helper alfa beta value (getPossibleBoards board player) (Board [])

-- FUNKCJE WALIDUJACE INPUT
-- Funkcje które sprawdzają errory "*err*" zwracają FALSE, jeżeli nie ma błędów i TRUE jeżeli są

investinput_current :: Board -> IO (Int,Int)
investinput_current board = do
    --putStrLn "-> Choose X coordindate (horizontal axis (from 1 to 8): "
    --tmpX <- getLine
    --putStrLn "-> Choose Y coordindate (vertical axis (from 1 to 8): "
    --tmpY <- getLine
    val <- get_input
    let x = val!!0
    let y = val!!1
    --putStrLn(show(val))
    --let x = read tmpX ::Int
    --let y = read tmpY :: Int
    let occupied_positions = ((getSheepPositions board) ++ [(getWolfPosition board)])
    let err_possible_sheep = (checkinput_error_issheep board (Position((x-1), (y-1))))
    let err_check_future_motion_right = (checkinput_error_ismotion (Position(x,y)) occupied_positions)
    let err_check_future_motion_left = (checkinput_error_ismotion (Position((x-2),y)) occupied_positions)
    let err_check_motion = ((err_check_future_motion_left == True) && (err_check_future_motion_right == True))

    if ((err_possible_sheep == False) && (err_check_motion == False)) then return ((x-1), (y-1))
    else do putStrLn "-> Incorrect coordinates"
            putStrLn "-> Please try again"
            investinput_current board

get_input :: IO([Int])
get_input = do
    putStrLn "-> Choose X coordindate (horizontal axis (from 1 to 8): "
    tmpX <- getLine
    putStrLn "-> Choose Y coordindate (vertical axis (from 1 to 8): "
    tmpY <- getLine
    let check_empty = not(isDigit (tmpX!!0)) || not(isDigit (tmpY!!0)) 
    let check_digit = (length tmpX) == 0 || (length tmpY) == 0
    if (check_digit||check_empty) then do 
        putStrLn "-> Incorrect input type"
        putStrLn "-> Please try again"
        get_input
    else do
        let x = read tmpX ::Int
        let y = read tmpY :: Int
        return [x, y]


checkinput_error_ismotion :: Position -> [Position] -> Bool
checkinput_error_ismotion (Position(x,y)) occupied = 
    do let err_occupy = (elem (Position(x,y)) occupied)
       let err_out = (checkinput_error_boundaries (Position(x,y)))
       let side_err = (err_out || err_occupy)
       if (side_err) then True
       else False

investinput_destination :: Position -> Board -> IO(Int, Int)
investinput_destination (Position(x, y)) board = do
    putStrLn "-> Choose side which you want to move [L - left or R - right]"
    tmpside <- getLine
    let side = tmpside
    let occupied_positions = ((getSheepPositions board) ++ [(getWolfPosition board)])
    let new_coor = (investinput_find_motion side (Position(x,y)))

    let err_check_motion = (checkinput_error_ismotion (Position(new_coor)) occupied_positions)
    if ((err_check_motion == False) &&  (new_coor /= (0,0))) then return new_coor
    else do 
        putStrLn "-> You can't move this sheep in this direction, please try again:"
        investinput_destination (Position(x, y)) board
    

checkinput_error_boundaries :: Position -> Bool
checkinput_error_boundaries (Position(x,y))  =
    do if ((x<0) || (x>7) || (y<0) || (y>7)) then True
        else False

investinput_find_motion :: String -> Position ->(Int, Int)
investinput_find_motion side (Position(x,y))
    | side == "L" = ((x-1),(y+1))
    | side == "R" = ((x+1),(y+1))
    |otherwise    = (0,0)

checkinput_error_issheep :: Board -> Position -> Bool
checkinput_error_issheep board p
    | (elem p possible) = False
    | otherwise = True
    where possible = (getSheepPositions board)


updateBoard :: Board -> Int -> Int -> Board
updateBoard board current future = Board $ updateBoard' board current future 0

updateBoard' :: Board -> Int -> Int -> Int -> [Field]
updateBoard' _ _ _ 64 = []
updateBoard' board current future index 
    | index == current = (Empty : (updateBoard' board current future (index + 1)))
    | index == future = (Sheep : (updateBoard' board current future (index + 1)))
    | otherwise =  ((getFieldByIndex board index) : (updateBoard' board current future (index + 1)))

rList :: String -> [Int]
rList = read

getWolf :: [Position] -> Position
getWolf (x:xs) = x

getSheep :: [Position] -> [Position]
getSheep (x:xs) = xs

loadFile gameCycle board player depth = do
    putStrLn "-> Which file would you like to load? (type exit to go back to menu)"
    location <- getLine
    if (location == "exit")
        then do gameCycle board player depth
        else do
            tryLoad <- try (readFile location) :: IO (Either SomeException String)
            case tryLoad of
                Left ex  -> do
                    putStrLn ("Something is wrong. Try again.")
                    loadFile gameCycle board player depth
                Right val -> do
                    loadedGame <- (readFile location)
                    let loadedGamePos = map getPosition (rList loadedGame)
                    let loadedWolf = getWolf loadedGamePos
                    let loadedSheep = getSheep loadedGamePos
                    putStrLn "-> Game successfully loaded"
                    let loadedBoard = generateBoard (loadedWolf) loadedSheep
                    tryGame <- try (putStrLn (printBoard loadedBoard)) :: IO (Either SomeException ())
                    case tryGame of
                        Left ex -> do
                            putStrLn ("\n[ERROR] Something is wrong with input data. Try again")
                            loadFile gameCycle board player depth
                        Right val -> do
                            gameCycle loadedBoard Human depth


saveFile gameCycle board player depth = do
    let wolfPos = getIndex (getWolfPosition board)
    let sheepPos = map getIndex (getSheepPositions board)
    let wolfAndSheep = wolfPos:sheepPos
    putStrLn "-> Where do you want to save game? (type exit to go back to menu)"
    location <- getLine
    if (location == "exit")
        then do gameCycle board player depth
        else do
            result <- try (writeFile location (show wolfAndSheep)) :: IO (Either SomeException ())
            case result of
                Left ex  -> do
                    putStrLn ("Something is wrong. Try again.")
                    saveFile gameCycle board player depth
                Right val -> do
                    putStrLn "-> Game successfully saved"
                    putStrLn (printBoard board)
                    gameCycle board player depth

gameCycle board player depth = do
    let gameScore = gameStatusEvaluation board
    if(gameScore == 100)
        then do
            putStrLn "-> Wolf won, if you want to play agian click enter, if you want to quit type Quit"
            command <- getLine
            if(command == "Quit" || command == "quit")
                    then do putStrLn "-> Bye bye!"
                    else do main
        else if (gameScore == (-100))
            then do
                putStrLn "-> You won, if you want to play agian click enter, if you want to quit type Quit"
                command <- getLine
                if(command == "Quit" || command == "quit")
                        then do putStrLn "-> Bye bye!"
                        else do main
        else do
            if(player == Computer)
                then do
                    let currentBoard = alphaBeta depth board
                    putStrLn "-> Now your turn!"
                    putStrLn (printBoard currentBoard)
                    gameCycle currentBoard (next player) depth
                else do
                    putStrLn "-> Wanna play?"
                    putStrLn "Enter - play"
                    putStrLn "Load - load game"
                    putStrLn "Save - save game"
                    putStrLn "Restart - start new game"
                    putStrLn "Quit - quit game"
                    line <- getLine
                    if (line == "Quit" || line == "quit")
                        then do putStrLn "-> Bye bye!"
                        else if (line == "Save" || line == "save")
                            then do
                                saveFile gameCycle board player depth
                            else if (line == "Load" || line == "load")
                                then do
                                    loadFile gameCycle board player depth
                                else if (line == "Restart" || line == "restart")
                                    then do
                                        putStrLn "-> New game"
                                        main
                                    else do
                                        from <- (investinput_current board)
                                        let occupied = getSheepPositions board
                                        to <- (investinput_destination (Position from) board)
                                        let currentBoard = updateBoard board (getIndex (Position from)) (getIndex (Position to))
                                        putStrLn "-> WOOF! WOOF! wolf's turn"
                                        putStrLn (printBoard currentBoard)
                                        gameCycle currentBoard (next player) depth

input_game_diff :: IO(Int)
input_game_diff = do
    putStrLn "-> Select game difficulty from 1 to 10"
    diff <- getLine
    let check_empty = not(isDigit (diff!!0))
    let check_digit = ((length diff) == 0)
    if (check_digit||check_empty) then do 
        putStrLn "-> Incorrect input type"
        putStrLn "-> Please try again"
        input_game_diff
    else do
        let new_diff = read diff ::Int
        if ((new_diff > 0) && (new_diff < 11)) then
            return new_diff
        else 
            input_game_diff

main = do
    depth <- input_game_diff
    gameCycle (generateBoard (Position (0, 7)) [Position (1, 0), Position (3, 0), Position (5, 0), Position (7, 0)]) Computer depth
    