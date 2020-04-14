data Geslacht = M | V | X deriving (Eq, Show)

data Mens = Mens String Integer Geslacht deriving (Eq,Show)

data Tree a = Leaf a 
            | Branch (Tree a) (Tree a) deriving (Show, Eq, Ord)

createTree [x] = Leaf x
createTree xs = Branch (createTree left) (createTree right)
    where   amount = div (length xs) 2
            left = take amount xs
            right = drop amount xs

-- Determine amount of leaves in the tree        
amountLeaves (Leaf _) = 1
amountBranches (Branch left right)
    = amountLeaves left + amountLeaves right 

-- Find deepest element in the tree
deepestElement (Leaf _) = 1
deepestElement (Branch left right)
    |isLastNode left == False && isLastNode right == True = 1 + deepestElement (right)
    |isLastNode left == True && isLastNode right == False = 1 + deepestElement (left)
    |otherwise =   1 + deepestElement left  &&  1 + deepestElement right

isLastNode (Leaf _) = True
isLastNode (Branch _ _) = False    

-- Find an element in the tree
findElement x (Leaf y) = x == y
findElement x (Branch left right)
    = findElement x left || findElement x right

-- Smallest element in the tree
smallestElement (Leaf y) = y
smallestElement (Branch left right) = min (smallestElement (left)) (smallestElement (right))

-- Apply function on every element in tree
transformTree f (Leaf x) = Leaf (f x)
transformTree f (Branch left right) = Branch (transformTree left) (transformTree right)

-- Use a filter on a tree


-- Input
-- tree1 = Branch  (Branch (Leaf Mens("Kwik" 7 M)) (Leaf Mens("Kwek" 12 V))) (Leaf Mens("Kwak" 3 M))
-- tree2 = Branch  (Branch (Leaf Mens("Kwik" 7 M)) (Leaf Mens("Kwek" 12 V))) (Branch (Leaf Mens("Kwak" 3 M)) (Branch(Leaf Mens("Jan" 9 M)) (Leaf Mens("Pim" 34 V))))
tree3 = createTree (map createTree [[1..x] | x <- [1..10]])