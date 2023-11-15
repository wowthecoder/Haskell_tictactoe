module Main where

import TicTacToe
import Board
import Cell
import Player
import Helpers

import Text.Read (readMaybe)
import Data.List (intersperse)
import Control.Monad (mapM_)

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))

prettyPrint :: Board -> IO ()
prettyPrint bd@(Board n cs) = do 
  putStrLn ("  " ++ intersperse ' ' (concat $ map show [0..(n-1)]))
  let ts = zip [0..(n-1)] (rows bd)
  mapM_ (\(i,r) -> putStrLn (show i ++ " " ++ intersperse ' ' (concat $ map show r))) ts
  return ()

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- You might like to use one of the following signatures, as per the spec.
-- but this is up to you.
-- handles validDimension and parsePosition
-- doParseAction :: (String -> Maybe a) -> IO a
doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction f x = do
  let y = f x
  case y of 
    Just z -> return z 
    Nothing -> do 
      putStrLn "Invalid input. Try again: "
      inp <- getLine           
      doParseAction f inp

-- | Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn bd p = do
  inp <- getLine
  move <- doParseAction parsePosition inp
  let x = tryMove p move bd 
  case x of
    Just newBd -> return newBd
    Nothing -> do
      putStrLn "Invalid input. Try again: "
      takeTurn bd p

-- | Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame bd p = do
  prettyPrint bd
  putStrLn ("Player " ++ show p ++ ", please enter your next move:")
  newBd <- takeTurn bd p 
  if gameOver newBd
    then do 
      prettyPrint newBd
      putStrLn ("Congrats to Player " ++ show p ++ "! You won!")
  else if isTie newBd
    then do
      prettyPrint newBd
      putStrLn ("The match ended in a tie. Better luck next time!")
  else playGame newBd (swap p)

takeTurnAI :: Board -> Player -> Player -> IO Board
takeTurnAI bd p bot = do 
  if p == bot 
    then do
      putStrLn "Bot is thinking. Please wait..."
      let botMove = getMove bd bot
      return botMove
  else do
    putStrLn ("Player " ++ show p ++ ", please enter your next move:")
    newBd <- takeTurn bd p 
    return newBd

playBot :: Board -> Player -> Player -> IO ()
playBot bd p bot = do
  prettyPrint bd
  newBd <- takeTurnAI bd p bot
  if gameOver newBd && p == bot
    then do 
      prettyPrint newBd
      putStrLn "Oopsies, tough luck human. My AI remains unbeatable HAHAHA"
  else if isTie newBd
    then do
      prettyPrint newBd
      putStrLn "The match ended in a tie. Try harder next time!"
  else if gameOver newBd && p /= bot
    then do
      prettyPrint newBd
      putStrLn "Jeez human you actually won! I thought that's impossible..."
  else playBot newBd (swap p) bot


-- | Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
  disableBuffering -- don't remove!
  putStrLn "Welcome to JH's tictactoe! Have fun XD"
  putStrLn "Press M for multiplayer, press S to challenge the bot [M/S]:"
  op1 <- getLine
  if op1 == "M"
    then do 
      putStrLn "Please input a board dimension:"
      inp1 <- getLine
      n <- doParseAction validDimension inp1
      playGame (emptyBoard n) X
      return ()
  else if op1 == "S"
    then do 
      putStrLn "Press X to start first, press O to let AI start first [X/O]:"
      op2 <- getLine
      if op2 == "X"
        then do 
          playBot (emptyBoard 3) X O
      else if op2 == "O"
        then do
          playBot (emptyBoard 3) X X
      else do
        putStrLn "Invalid input. Exiting program."
        return ()
  else do
    putStrLn "Invalid input. Exiting program."
    return ()

{-|
When ran via `cabal run`, Haskell will "buffer" the input and output streams
for performance. This is annoying, since it means that some printing can happen
out-of-order -- the strings are only written when a newline is entered!
There are a few ways of getting around this, like using `hFlush stdout` after
each print. However, in this case, it is simplest to just disable the buffering
altogether. This function *must* be called at the start of your `main` function.
-}
disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
