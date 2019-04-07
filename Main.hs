module Main where

import Deck
import Data.Char
import System.Random


data Interface = Interface
  { iEmpty    :: Hand
  , iFullDeck :: Deck
  , iDraw     :: Deck -> (Hand, Deck)
  , iShuffle  :: IO Deck
--  , iGameOver :: Card -> Bool
  }


runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  let score = 0
  g <- (iShuffle i)
  gameLoop i g (iEmpty i) score

gameLoop :: Interface -> Deck -> Hand -> Integer -> IO ()
gameLoop i deck hand score = do

  if score >= 21 then do
    finish i hand deck score
   else do
    putStrLn "Draw another card? [y]"
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (hand', deck') = iDraw i deck
      let handP = addHand hand hand'

      let scoreP = sum (map totalValue handP)

      --let scoreC = scoreC + scoreP
      putStrLn ("Your current hand: " ++ show (handP))
      putStrLn ("Your current score: " ++ show (scoreP))
      gameLoop i deck' handP scoreP

     else
      finish i hand deck score

finish :: Interface -> Hand -> Deck -> Integer -> IO ()
finish i hand deck score = do

  if score > 21 then do
    putStrLn ("Você perdeu")
    else do
      putStrLn ("Você ganhou")




addHand :: Hand -> Hand -> Hand
addHand hand1 hand2 = hand1 ++ hand2



valueRank :: Value -> Integer
valueRank A   = 11
valueRank Two   = 2
valueRank Three = 3
valueRank Four  = 4
valueRank Five  = 5
valueRank Six   = 6
valueRank Seven = 7
valueRank Eight = 8
valueRank Nine  = 9
valueRank _     = 10


totalValue :: Card -> Integer
totalValue (Card value _) = valueRank value



empty :: Hand
empty = []


--valueC :: Hand -> Integer
--valueC empty                    = 0
--valueC hand
--  | totalValue hand <= 21 = totalValue hand
--  | totalValue hand > 21  = totalValue hand

--totalValue :: Hand -> Integer
--totalValue empty = 0
--totalValue hand = valueRank value + totalValue hand
-- Values each rank, returns the value in numbers

implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iDraw      = dealCards
  ,  iShuffle   = shuffleDeck
--  ,  iGameOver = gameOver
  }


main :: IO ()
main = runGame implementation