-- https://udel.edu/~os/riddle.html

module Einstein where

import Prelude
import Data.List

data Location    = One | Two | Three | Four | Five                  deriving (Eq,Enum,Show)
data Color       = Red | White | Green | Yellow | Blue              deriving (Eq,Enum,Show)
data Nationality = Brit | Swede | Dane | Norwegian | German         deriving (Eq,Enum,Show)
data Beverage    = Tea | Coffee | Milk | Beer | Water               deriving (Eq,Enum,Show)
data Smoke       = Pallmall | Dunhill | Bluemaster | Prince | Blend deriving (Eq,Enum,Show)
data Pet         = Dog | Bird | Cat | Horse | Fish                  deriving (Eq,Enum,Show)

type Profile = (Location, Color, Nationality, Beverage, Smoke, Pet)
type Candidates = [Profile] -- candidates that do not violate any of the non-proximity rules
type Solution = [Profile] -- one possible solution, need to ensure mutual exclusivity 
type Space = [Solution] -- all possible solutions

-- combine conditions
combine :: [a -> Bool] -> a -> Bool
combine (f:fs) xs = (f xs) && (combine fs xs)
combine []     xs = True

-- candidates filtered with all non-proximity rules
candidates :: Candidates
candidates = filter (combine [h1,h2,h3,h5,h6,h7,h8,h9,h12,h13]) 
             [(l, c, n, b, s, p) | 
               c <- [Red ..],
               n <- [Brit ..],
               b <- [Tea ..],
               s <- [Pallmall ..],
               p <- [Dog ..],
               l <- [One ..]
             ]

-- check if two profiles are in conflict
conf :: Profile -> Profile -> Bool
conf (l1, c1, n1, b1, s1, p1) (l2, c2, n2, b2, s2, p2) = (l1 == l2) || (c1 == c2) || (n1 == n2) || (b1 == b2) || (s1 == s2) || (p1 == p2)

-- mutually exclusive
mutex :: Solution -> Bool
mutex (x:xs) = (not (any (conf x) xs)) && (mutex xs)
mutex []     = True

-- solution space
space :: Space
space = filter (combine [h4,h10,h11,h14,h15,mutex]) 
        [[pf1, pf2, pf3, pf4, pf5] | 
          pf1 <- filter r1 candidates, 
          pf2 <- filter r2 candidates, 
          pf3 <- filter r3 candidates, 
          pf4 <- filter r4 candidates, 
          pf5 <- filter r5 candidates]

-- location rules
r1 :: Profile -> Bool
r1 (One, _, _, _, _, _) = True
r1 _ = False

r2 :: Profile -> Bool
r2 (Two, _, _, _, _, _) = True
r2 _ = False

r3 :: Profile -> Bool
r3 (Three, _, _, _, _, _) = True
r3 _ = False

r4 :: Profile -> Bool
r4 (Four, _, _, _, _, _) = True
r4 _ = False

r5 :: Profile -> Bool
r5 (Five, _, _, _, _, _) = True
r5 _ = False

-- Riddle Hints
-- h1: the Brit lives in the red house
h1 :: Profile -> Bool
h1 (_, Red, Brit, _, _, _) = True
h1 (_, Red, _,    _, _, _) = False
h1 (_, _,   Brit, _, _, _) = False
h1 _ = True

-- h2: the Swede keeps dogs as pets
h2 :: Profile -> Bool
h2 (_, _, Swede, _, _, Dog) = True
h2 (_, _, Swede, _, _, _)   = False
h2 (_, _, _,     _, _, Dog) = False
h2 _ = True

-- h3: the Dane drinks tea
h3 :: Profile -> Bool
h3 (_, _, Dane, Tea, _, _) = True
h3 (_, _, Dane, _,   _, _) = False
h3 (_, _, _,    Tea, _, _) = False
h3 _ = True

-- h4: the green house is on the left of the white house (proximity)
h4 :: Solution -> Bool
h4 ((l1, c1, n1, b1, s1, p1):(l2, c2, n2, b2, s2, p2):xs) = (c1 == Green && c2 == White) || (h4 ((l2, c2, n2, b2, s2, p2):xs))
h4 [x] = False

-- h5: the green house's owner drinks coffee
h5 :: Profile -> Bool
h5 (_, Green, _, Coffee, _, _) = True
h5 (_, Green, _, _,      _, _) = False
h5 (_, _,     _, Coffee, _, _) = False
h5 _ = True

-- h6: the person who smokes Pall Mall rears birds
h6 :: Profile -> Bool
h6 (_, _, _, _, Pallmall, Bird) = True
h6 (_, _, _, _, Pallmall, _)    = False
h6 (_, _, _, _, _,        Bird) = False
h6 _ = True

-- h7: the owner of the yellow house smokes Dunhill
h7 :: Profile -> Bool
h7 (_, Yellow, _, _, Dunhill, _) = True
h7 (_, Yellow, _, _, _,       _) = False
h7 (_, _,      _, _, Dunhill, _) = False
h7 _ = True

-- h8: the man living in the center house drinks milk
h8 :: Profile -> Bool
h8 (Three, _, _, Milk, _, _) = True
h8 (Three, _, _, _,    _, _) = False
h8 (_,     _, _, Milk, _, _) = False
h8 _ = True

-- h9: the Norwegian lives in the first house
h9 :: Profile -> Bool
h9 (One, _, Norwegian, _, _, _) = True
h9 (One, _, _,         _, _, _) = False
h9 (_,   _, Norwegian, _, _, _) = False
h9 _ = True

-- h10: the man who smokes blends lives next to the one who keeps cats (proximity)
h10 :: Solution -> Bool
h10 ((l1, c1, n1, b1, s1, p1):(l2, c2, n2, b2, s2, p2):xs) = (s1 == Blend && p2 == Cat) || (s2 == Blend && p1 == Cat) || (h10 ((l2, c2, n2, b2, s2, p2):xs))
h10 [x] = False

-- h11: the man who keeps horses lives next to the man who smokes Dunhill (proximity)
h11 :: Solution -> Bool
h11 ((l1, c1, n1, b1, s1, p1):(l2, c2, n2, b2, s2, p2):xs) = (s1 == Dunhill && p2 == Horse) || (s2 == Dunhill && p1 == Horse) || (h11 ((l2, c2, n2, b2, s2, p2):xs))
h11 [x] = False

-- h12: the owner who smokes BlueMaster drinks beer
h12 :: Profile -> Bool
h12 (_, _, _, Beer, Bluemaster, _) = True
h12 (_, _, _, Beer, _,          _) = False
h12 (_, _, _, _,    Bluemaster, _) = False
h12 _ = True

-- h13: the German smokes Prince
h13 :: Profile -> Bool
h13 (_, _, German, _, Prince, _) = True
h13 (_, _, German, _, _,      _) = False
h13 (_, _, _,      _, Prince, _) = False
h13 _ = True

-- h14: the Norwegian lives next to the blue house (proximity)
h14 :: Solution -> Bool
h14 ((l1, c1, n1, b1, s1, p1):(l2, c2, n2, b2, s2, p2):xs) = (c1 == Blue && n2 == Norwegian) || (c2 == Blue && n1 == Norwegian) || (h14 ((l2, c2, n2, b2, s2, p2):xs))
h14 [x] = False

-- h15: the man who smokes blend has a neighbor who drinks water (proximity)
h15 :: Solution -> Bool
h15 ((l1, c1, n1, b1, s1, p1):(l2, c2, n2, b2, s2, p2):xs) = (s1 == Blend && b2 == Water) || (s2 == Blend && b1 == Water) || (h15 ((l2, c2, n2, b2, s2, p2):xs))
h15 [x] = False

-- final solution
solution = take 1 space