{-#LANGUAGE ScopedTypeVariables#-}

import Data.List
import Data.Maybe
-- NW N NE 
-- W     E
-- SW S SE

-- x - vertical
-- y - horizontal

-- data Move = NW | NE | SW | SE

-- data GameState = GameState (PawnPosition, PawnPosition, PawnPosition, PawnPosition, PawnPosition)

-- movePawn :: PawnPosition -> Move -> PawnPosition
-- movePawn PawnPosition pawn (x, y) NW = PawnPosition pawn (x-1, y+1)
-- movePawn PawnPosition pawn (x, y) NE = PawnPosition pawn (x+1, y+1)
-- movePawn PawnPosition pawn (x, y) SW = PawnPosition pawn (x-1, y-1)
-- movePawn PawnPosition pawn (x, y) SE = PawnPosition pawn (x+1, y-1)

data Pawn = Wolf | SheepOne | SheepTwo | SheepThree | SheepFour deriving (Eq)
data PawnPosition = Pawn := (Int, Int)

data Board = Board [PawnPosition]

data Player = Human | Computer deriving (Eq)
data GameTree a = a :> [GameTree a] deriving (Show) 

-- startingBoard = Board [Wolf := (0, 0), SheepOne := (1, 8), SheepTwo := (3, 8), SheepThree := (5, 8), SheepFour := (7, 8)]

-- currentPosition :: Board -> Pawn -> (Int, Int)
-- currentPosition  Board ((pawn := (x, y)):xs) pawnToCheck 
--     | pawn == pawnToCheck = (x)
--     | otherwise = currentPosition xs pawnToCheck

-- posibleBoards :: Board -> Pawn -> [(Int, Int)]
-- posibleBoards [(PawnPosition pawn (x, y)):xs] Wolf 
    
next :: Player -> Player
next Human = Computer
next Computer = Human 

rTr :: GameTree Int
rTr = 0 :> [
    (-1) :> [
        (-10) :> [
            (20) :> [], 
            (-10) :> [], 
            (-15) :> []
        ], 
        (-8) :> [
            (-29) :> [], 
            (-8) :> [],
            (-2) :> []
        ]
    ], 
    1 :> [
        10 :> [
            (8) :> [], 
            (6) :> []
        ], 
        (-6) :> [
            (-20) :> [], 
            (-10) :> []
        ]
    ]]

-- add board evaluation
gameStatusEvaluation :: a -> a
gameStatusEvaluation value = value

-- minMax :: Int -> Player -> GameTree Int -> ([Int], Int)
-- minMax 0 _ (state :> _) = ([], gameStatusEvaluation state)
-- minMax _ _ (state :> []) = ([], gameStatusEvaluation state)
-- minMax depth Human (state :> nextMoves) = (minimumIndex:(path!!minimumIndex), minimumVal)
--     where 
--         path = map (fst) mapped
--         minimumIndex = fromMaybe 0 (elemIndex minimumVal minimas)
--         minimumVal = minimum (minimas)
--         minimas =  map (snd) mapped
--         mapped = map (\x -> minMax (depth-1) Computer x) nextMoves

-- minMax depth Computer (state :> nextMoves)= (maximumIndex:(path!!maximumIndex), maximumVal)
--     where 
--         path = (map (fst) mapped)
--         maximumIndex = fromMaybe 0 (elemIndex maximumVal maximas)
--         maximumVal = maximum (maximas)
--         maximas =  map (snd) mapped
--         mapped = map (\x -> minMax (depth-1) Human x) nextMoves

betaMax = -100
alfaMax = 100

alphaBeta :: Int -> GameTree Int -> Int
alphaBeta depth moveTree = alphaBeta' depth betaMax alfaMax Computer moveTree
alphaBeta' :: Int -> Int -> Int -> Player -> GameTree Int -> Int
alphaBeta' _ _ _ _ (board :> []) = gameStatusEvaluation board
alphaBeta' depth alfa beta player (board :> rest)
    | depth == 0 = gameStatusEvaluation board
    | otherwise = 
        let (value, funcExtreme) = if player == Computer then (alfa, max) else (beta, min)
            helper alfa beta v [] = v
            helper alfa beta v (x:xs) =
                let v' = alphaBeta' (depth - 1) alfa beta (next player) x
                    newValue = funcExtreme v v'
                    (newAlfa, newBeta) = if player == Computer
                            then (funcExtreme alfa newValue, beta)
                            else (alfa, funcExtreme beta newValue)
                in if newBeta <= newAlfa
                    then newValue
                    else helper newAlfa newBeta newValue xs
        in helper alfa beta value rest

main = do  
    putStrLn "Provide depth"  
    depth :: Int  <- readLn
    print  (alphaBeta depth rTr)