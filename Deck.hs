module Deck where

import System.Random
import Data.Array.IO
import Control.Monad


data Card = Card {value :: Value, suit :: Suit} deriving (Eq)

instance Show Card where
  show (Card v n) = show v ++ " de " ++ show n ++ "s"

data Suit = Espada | Ouro | Copa | Pau deriving (Show, Enum, Eq, Ord, Bounded)

allSuits :: [Suit]
allSuits = [ (minBound :: Suit) .. ]

type Hand = [Card]
type Deck = [Card]

data Value = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Valete | Dama | Rei
         deriving (Enum, Eq, Ord, Bounded)

instance Show Value where
  show A = "As"
  show Valete = "J"
  show Dama = "Q"
  show Rei = "K"
  show n = show $ 1 + fromEnum n

allValues :: [Value]
allValues = [ (minBound :: Value) .. ]


fullDeck :: Deck
fullDeck = Card <$> allValues <*> allSuits

shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray len xs
    forM [1..len] $ \i -> do
      j <- randomRIO (i,len)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  where
    len = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray len = newListArray (1, len)

shuffleDeck  :: IO Deck
shuffleDeck = shuffle fullDeck

cardValues :: Value -> [Int]
cardValues A   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]


dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)