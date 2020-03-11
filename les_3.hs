-- test: Data structures
data SimpleVorm = Cirkel Float | Rechthoek Float Float

oppervlakte (Rechthoek x y) = x * y
oppervlakte (Cirkel r) = pi * r * r

-- Excercise 1: amount of branches in a tree
data Tree a = Leaf a 
            | Branch (Tree a) (Tree a) deriving (Show, Eq, Ord)

isLastNode (Leaf _) = True
isLastNode (Branch _ _) = False

tree1 = Branch  (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
tree2 = Branch  (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Branch(Leaf 4) (Leaf 5)))

amountLeaves (Leaf _) = 1
amountBranches (Branch left right)
    = amountLeaves left + amountLeaves right 

-- excercise 2: Find an element in the tree
findElement x (Leaf y) = x == y
findElement x (Branch left right)
    = findElement x left || findElement x right

-- excercise 3: Smallest element in the tree
smallestElement (Leaf y) = y
smallestElement (Branch left right) = min(smallestElement (left)  smallestElement (right))

-- excercise 4: Happy Numbers
-- Kwadrateer de afzonderlijke cijfers van het getal;
-- De som van deze kwadraten vormt een nieuw getal;
-- herhaal deze procedure zo lang totdat er ofwel een cyclus van getallen wordt doorlopen, ofwel het getal 1 optreedt;
-- wordt het getal 1 bereikt, dan is het oorspronkelijke getal een gelukkig getal.

squared x = x * x
splitSum x | (x < 10) == x
           | otherwise =  splitSum(splitSum(squared((x `div` 10))) + splitSum(squared((x `mod` 10))))
isHappy x = splitSum x == 1
