module Main where

import Deck
import Blackjack
import Data.Char
import System.Random


data Interface = Interface
  { iEmpty    :: Hand
  , iFullDeck :: Deck
  , iDraw     :: Deck -> (Hand, Deck)
  , iShuffle  :: IO Deck
  }


runGame :: Interface -> IO ()
runGame i = do
  putStrLn "Welcome to the game."
  g <- (iShuffle i)
  gameLoop i g (iEmpty i)

gameLoop :: Interface -> Deck -> Hand -> IO ()
gameLoop i deck hand = do


  if False then do
    finish i hand deck
   else do
    putStrLn "Draw another card? [y]"
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (hand', deck') = iDraw i deck
      let handP = addHand hand hand'
      putStrLn ("Your current hand: " ++ show (handP))
      gameLoop i deck' handP

     else
      finish i hand deck

finish :: Interface -> Hand -> Deck -> IO ()
finish i hand deck = do
  putStrLn ("Winner: ")







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

empty :: Hand
empty = []


implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iDraw      = dealCards
  ,  iShuffle   = shuffleDeck
  }


main :: IO ()
main = runGame implementation