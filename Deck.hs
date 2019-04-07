module Deck where

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad
import Data.Map

data Carta = Carta {valor :: Valor, naipe :: Naipe} deriving (Eq)

instance Show Carta where
  show (Carta v n) = show v ++ " de " ++ show n ++ "s"

data Naipe = Espada | Ouro | Copa | Pau deriving (Show, Enum, Eq, Ord, Bounded)

todosNaipes :: [Naipe]
todosNaipes = [ (minBound :: Naipe) .. ]

data Valor = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Valete | Dama | Rei
         deriving (Enum, Eq, Ord, Bounded)

instance Show Valor where
  show A = "As"
  show Valete = "J"
  show Dama = "Q"
  show Rei = "K"
  show n = show $ 1 + fromEnum n

todosValores :: [Valor]
todosValores = [ (minBound :: Valor) .. ]


baralho :: [Carta]
baralho = Carta <$> todosValores <*> todosNaipes
