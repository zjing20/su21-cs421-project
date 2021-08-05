module Zurg where

import Prelude
import Data.List

data Toy = Buzz | Hamm | Rex | Woody deriving (Eq,Ord,Show)
data Pos = L | R                     deriving (Eq,Show)
type Group = [Toy]
type BridgePos = (Pos,Group) -- Pos: position of flashlight, Group: toys on the left side
type Move = Either Toy Group -- Move of a group from left to right | Move of one from right to left
type Space m s = [([m],s)]

backw :: Group -> [(Move,BridgePos)]
backw xs = [(Left x,(L,sort (x:(toys \\ xs)))) | x <- xs] -- xs: toys on the right

forw :: Group -> [(Move,BridgePos)]
forw xs = [(Right [x,y],(R,delete y ys)) |
              x <- xs,let ys=delete x xs, y <- ys, x<y] -- choose 2 (all possibilities where x<y)

trans :: BridgePos -> [(Move,BridgePos)] -- state -> a list of possible moves and new states
trans (L,l) = forw l
trans (R,l) = backw (toys \\ l)

toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody]

time :: Toy -> Int
time Buzz  = 5
time Woody = 10
time Rex   = 20
time Hamm  = 25

duration :: [Move] -> Int
duration = sum . map (either time (maximum.map time))

isSolution :: ([Move],BridgePos) -> Bool -- s: final state, [m]: list of moves
isSolution (ms,s) = s == (R,[]) && duration ms <= 60

space :: BridgePos -> Space Move BridgePos
space s = step ++ expand step
    where step = [ ([m],t) | (m,t) <- trans s ]
          expand ss = [ (ms++ns,t) | (ms,s) <- ss,
                                     (ns,t) <- space s ]

solutions :: BridgePos -> Space Move BridgePos
solutions = filter isSolution . space

solution = solutions (L,toys)