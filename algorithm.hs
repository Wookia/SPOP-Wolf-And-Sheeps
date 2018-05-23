{-#LANGUAGE ScopedTypeVariables#-}

import Data.List
import Data.Maybe

data Player = Human | Computer
data GameTree a = a :> [GameTree a] deriving (Show) 

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

gameStatusEvaluation :: a -> a
gameStatusEvaluation value = value


minMax :: Int -> Player -> GameTree Int -> ([Int], Int)
minMax 0 _ (state :> _) = ([], gameStatusEvaluation state)
minMax _ _ (state :> []) = ([], gameStatusEvaluation state)
minMax depth Human (state :> nextMoves) = (minimumIndex:(path!!minimumIndex), minimumVal)
    where 
        path = map (fst) mapped
        minimumIndex = fromMaybe 0 (elemIndex minimumVal minimas)
        minimumVal = minimum (minimas)
        minimas =  map (snd) mapped
        mapped = map (\x -> minMax (depth-1) Computer x) nextMoves

minMax depth Computer (state :> nextMoves) = (maximumIndex:(path!!maximumIndex), maximumVal)
    where 
        path = (map (fst) mapped)
        maximumIndex = fromMaybe 0 (elemIndex maximumVal maximas)
        maximumVal = maximum (maximas)
        maximas =  map (snd) mapped
        mapped = map (\x -> minMax (depth-1) Human x) nextMoves

main = do  
    putStrLn "Provide depth"  
    depth :: Int  <- readLn
    print  (minMax depth Computer rTr)