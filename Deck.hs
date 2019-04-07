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


shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled

  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

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


dealCards :: Deck -> (Hand, Deck)
dealCards deck = (take 1 deck, drop 1 deck)