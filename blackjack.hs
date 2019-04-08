import Data.Bool
import Data.Char
import System.Random
import Data.List

sizeDeck :: [Int] -> Int
sizeDeck = length

shuffleDeck :: StdGen -> [Int] -> [Int]
shuffleDeck seed m = let (r,g) = randomR (0, sizeDeck m-1) seed
    in (m !! r: if sizeDeck m > 1 then shuffleDeck g $ delete (m !! r) m else [])

values :: [Int]
values = [11,11,11,11,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,
        8,8,8,8,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]

names :: [String]
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

deal :: Int -> [Int] -> [Int]
deal value hand = do
  let x = [value]
  hand ++ x

remove_h :: [Int] -> [Int]
remove_h x = tail x

playing :: [Int] -> [Int] -> [Int] -> IO ()
playing ideck player dealer = do
    if sum player > 21 && sum dealer <=21
    then do
        print "Voce perdeu :("
        print player
        print dealer
    else if sum dealer > 21 || sum player == 21
    then do
        print "Voce ganhou!!"
        print player
        print dealer
    else do
        print "----------------------------------"
        print "(1)Desce mais uma carta!--(2)Parar"
        opt <- getLine
        let x = head ideck
        --print x
        if opt == "1"
        then do
            let playerf = deal (values !! x) player
            let ideckf = remove_h ideck
            print "Voce tirou um(a):"
            print (names !! x)
            playing ideckf playerf dealer
        else if opt == "2"
        then do
            if sum dealer < sum player
            then do
                let dealerf = deal (values !! x) dealer
                print "O dealer tirou um(a):"
                print (names !! x)
                print "----------------------------------"
                if sum dealerf > sum player || sum dealerf <= 21
                then do
                    print "Voce perdeu :("
                    print player
                    print dealerf
                else do
                    print "Voce ganhou!!"
                    print player
                    print dealerf
            else do
                if sum dealer <= 21
                then do
                    print "Voce perdeu :("
                    print player
                    print dealer
                else do
                    print "Voce ganhou!!"
                    print player
                    print dealer
        else do
            print "Opcao indisponivel"
            playing ideck player dealer

-------------------------------------------------------------------------------

main :: IO () 
main = do
seed <- getStdGen
let ideck = [0..51]
let ideckS = shuffleDeck seed ideck
let player = [values !! (ideckS !! 0),values !! (ideckS !! 2)]
let dealer = [values !! (ideckS !! 1),values !! (ideckS !! 3)]
print "----------------------------------"
print "Bem vindo ao black jack"
print "----------------------------------"
print "Cartas iniciais do jogador:"
let a = names !! (ideckS !! 0)
let b = names !! (ideckS !! 2)
let c = names !! (ideckS !! 1)
print a
print b
print "Carta aberta inicial do dealer:"
print c
--print ideckS
let ideck = tail ideckS
let ideckS = tail ideck
let ideck = tail ideckS
let ideckS = tail ideck
--print ideckS
playing ideckS player dealer
