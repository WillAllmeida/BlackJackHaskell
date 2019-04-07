import Data.Bool
import Data.Char
import System.Random
import Data.List

values = [11,11,11,11,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,
        8,8,8,8,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]
names = ["As de copas", "As de ouros", "As de paus", "As de espadas",
        "2 de copas", "2 de ouros", "2 de paus", "2 de espadas",
        "3 de copas", "3 de ouros", "3 de paus", "3 de espadas",
        "4 de copas", "4 de ouros", "4 de paus", "4 de espadas",
        "5 de copas", "5 de ouros", "5 de paus", "5 de espadas",
        "6 de copas", "6 de ouros", "6 de paus", "6 de espadas",
        "7 de copas", "7 de ouros", "7 de paus", "7 de espadas",
        "8 de copas", "8 de ouros", "8 de paus", "8 de espadas",
        "9 de copas", "9 de ouros", "9 de paus", "9 de espadas",
        "10 de copas", "10 de ouros", "10 de paus", "10 de espadas",
        "J de copas", "J de ouros", "J de paus", "J de espadas",
        "Q de copas", "Q de ouros", "Q de paus", "Q de espadas",
        "K de copas", "K de ouros", "K de paus", "K de espadas"]
ideck = [1..52]
player = []
dealer = []

