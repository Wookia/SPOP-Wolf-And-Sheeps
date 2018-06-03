{-#LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE DataKinds #-}
import Data.List
import Data.Maybe
import Control.Monad

data Player = Human | Computer deriving (Eq)
data GameTree a = a :> [GameTree a] deriving (Show) 

data Field = Empty | Wolf | Sheep | Inaccesible deriving (Show, Eq) 
data Board = Board [Field] deriving (Show) 
data Position = Position (Int, Int) deriving (Eq, Show)

printBoard :: Board -> [Char]
printBoard board = (printBoard' 0 board)

displayField :: Field -> [Char]
displayField Wolf = "w"
displayField Sheep = "s"
displayField Empty = "o"

printBoard' :: Int -> Board -> [Char]
printBoard' _ (Board []) = "\n"
printBoard' index (Board (x:xs)) 
    | mod index 8 == 0 = ("\n" ++ (displayField x) ++ (printBoard' (index + 1) (Board xs)))
    | otherwise = ((displayField x) ++ (printBoard' (index + 1) (Board xs)))


getIndex :: Position -> Int
getIndex (Position (x, y)) = y*8+x

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

xsd ::  [Position] -> [Position] -> [[Position]]
xsd possibleSheepPositions currentSheepPositions = map (\n -> n : currentSheepPositions) possibleSheepPositions

abgh :: Int -> [[Position]] -> [Position] -> [[Position]]
abgh index [[]] _ = [[]]
abgh index [x] currentSheepPositions  = xsd x (removeItem index currentSheepPositions)
abgh index (x:xs) currentSheepPositions =  xsd x (removeItem index currentSheepPositions) ++ (abgh (index + 1) xs currentSheepPositions)

getPossibleSheepBoards :: Board -> [Board]
getPossibleSheepBoards board = 
    let currentSheepPositions = getSheepPositions board
        possibleSheepPositions = map (\n -> filter (checkIfFieldEmpty board) $ getPossibleSheepPositions n) $ currentSheepPositions
        wolfPosition = getWolfPosition board
    in [generateBoard wolfPosition sheepPosition  | sheepPosition <- (abgh 0 possibleSheepPositions currentSheepPositions)]


    
next :: Player -> Player
next Human = Computer
next Computer = Human 

betaMax = -100
alfaMax = 100

gameStatusEvaluation :: Board -> Int
gameStatusEvaluation _ = 1

alphaBeta :: Int -> Board -> Board
alphaBeta depth board = snd (alphaBeta' depth betaMax alfaMax Computer board)

alphaBeta' :: Int -> Int -> Int -> Player -> Board -> (Int, Board)
alphaBeta' depth alfa beta player board
    | depth == 0 = (gameStatusEvaluation board, board)
    | otherwise = 
        let (value, funcExtreme) = if player == Computer then (alfa, max) else (beta, min)
            helper alfa beta v [] = (v, Board [])
            helper alfa beta v [x] = (v, x)
            helper alfa beta v (x:xs) =
                let v' = alphaBeta' (depth - 1) alfa beta (next player) x
                    newValue = funcExtreme v (fst v')
                    (newAlfa, newBeta) = if player == Computer
                            then (funcExtreme alfa newValue, beta)
                            else (alfa, funcExtreme beta newValue)
                in if newBeta <= newAlfa
                    then (newValue, x)
                    else helper newAlfa newBeta newValue xs
        in helper alfa beta value (getPossibleBoards board player)



investinput :: IO (Int, Int)
investinput = do
    putStrLn "x "
    tmpX <- getLine
    putStrLn "y"
    tmpY <- getLine
    let x = read tmpX
    let y = read tmpY
    return (x, y)

updateBoard :: Board -> Int -> Int -> Board
updateBoard board current future = Board $ updateBoard' board current future 0

updateBoard' :: Board -> Int -> Int -> Int -> [Field]
updateBoard' _ _ _ 64 = []
updateBoard' board current future index 
    | index == current = (Empty : (updateBoard' board current future (index + 1)))
    | index == future = (Sheep : (updateBoard' board current future (index + 1)))
    | otherwise =  ((getFieldByIndex board index) : (updateBoard' board current future (index + 1)))

depth = 5


gameCycle board player = do  
    if(player == Computer)
        then do
            let currentBoard = alphaBeta depth board
            putStrLn (printBoard currentBoard)
            gameCycle currentBoard (next player)
        else do
            line <- getLine
            unless (line == "q") $ do
                putStrLn "Current position: "
                from <- investinput
                putStrLn "Destination: "
                to <- investinput
                let currentBoard = updateBoard board (getIndex (Position from)) (getIndex (Position to))
                putStrLn (printBoard currentBoard)
                gameCycle currentBoard (next player)


main = do
    gameCycle (generateBoard (Position (0, 7)) [Position (1, 0), Position (3, 0), Position (5, 0), Position (7, 0)]) Computer
    