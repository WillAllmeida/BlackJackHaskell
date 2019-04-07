module Main where

import Deck
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