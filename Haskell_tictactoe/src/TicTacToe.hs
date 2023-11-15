module TicTacToe (gameOver, validDimension, isTie, getMove, parsePosition, tryMove) where

import Data.List (nub)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver bd = foldr (||) False [isFull $ nub l | l <- concat [rows bd, cols bd, diags bd]]
    where 
        isFull [Taken X] = True
        isFull [Taken O] = True
        isFull _   = False

-- NEW function to check if game ended in a tie
isTie :: Board -> Bool
isTie bd = (not (gameOver bd)) && isBdFull bd

-- NEW function to check if board is full (extracted from isTie because the AI uses this function as well)
isBdFull :: Board -> Bool
isBdFull (Board n cs) = not (foldr (||) False (map isEmpty cs))

-- NEW function to check if player entered a valid board dimension (positive integer)
validDimension :: String -> Maybe Int
validDimension i 
    | Just x <- (readMaybe i :: Maybe Int) = if x > 0 then Just x else Nothing
    | otherwise                            = Nothing        

-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition xys 
    | [Just x, Just y] <- zs = Just (x, y)
    | otherwise  = Nothing
    where 
        zs = map (\w -> readMaybe w :: Maybe Int) (words xys)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (i,j) (Board n cs) 
    | i >= n || i < 0 || j >= n || j < 0 = Nothing
    | cs !! y /= Empty                   = Nothing
    | otherwise                          = Just (Board n (replace y (Taken p) cs))
    where 
        y = i*n + j

--NEW functions for unbeatable tictactoe AI using minimax algorithm (only for 3x3 board)
validMoves :: Board -> [Position]
validMoves (Board n cs) = [x `quotRem` n | (x, y) <- zip [0..(n*n-1)] cs, isEmpty y]

move p pos bd = fromJust $ tryMove p pos bd

-- Parameters: board, current player, bot player(constant)
-- Minimax calculates the score for each possible move, getMove returns one of the best moves
-- If the game is over and it's the bot's turn, the player won; and vice versa
minimax :: Board -> Player -> Player -> Int
minimax bd p bot 
    | gameOver bd && p == bot = -10
    | gameOver bd && p /= bot = 10
    | isTie bd                = 0
    | p == bot                = maximum [minimax (move p m bd) (swap p) bot | m <- vms]
    | otherwise               = minimum [minimax (move p m bd) (swap p) bot | m <- vms]
    where 
        vms = validMoves bd

getMove :: Board -> Player -> Board
getMove bd bot = head [(move bot pos bd) | (pos, score) <- lst, score == maximum scores]
    where
        vms = validMoves bd
        scores = map (\m -> minimax (move bot m bd) (swap bot) bot) vms
        lst = zip vms scores