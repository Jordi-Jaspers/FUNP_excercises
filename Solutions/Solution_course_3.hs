
tweeMaal [] = []
tweeMaal xs = tweeMaal2' xs []


tweeMaal' :: [Integer] -> [Integer] -> [Integer]
tweeMaal' [] xs = reverse xs
tweeMaal' (x:xs) ys = tweeMaal' xs (x:x:ys)

tweeMaal2' :: [Integer] -> [Integer] -> [Integer]
tweeMaal2' [] xs = xs
tweeMaal2' (x:xs) ys = tweeMaal2' xs ys++[x,x]

maal getal [] = []
maal getal (x:xs) = (x*getal):(maal getal xs)


data Geslacht = M | V | X deriving (Eq, Show)
data Temperatuur = Warm | Normaal | Koud  
                 deriving (Eq,Ord, Show)
data Seizoen = Lente | Zomer | Herfst | Winter  
                 deriving (Eq,Ord, Show)


weer Zomer = Warm
weer Winter = Koud
weer _ = Normaal

data Mens = Mens String Integer Geslacht deriving (Eq,Show)



data Expr = Cte Integer 
		| Add Expr Expr 
		| Sub Expr Expr
		| Mult Expr Expr
		deriving Show

eval (Cte n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)

data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
			deriving (Eq, Show, Ord)

isEindKnoop (Leaf _) = True
isEindKnoop (Branch _ _) = False


boom1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
boom2 = Branch (Branch (Leaf 1) 
					   (Branch (Leaf 2) (Leaf 7)))
			   (Branch (Branch (Leaf 4) (Leaf 8)) 
					   (Branch (Leaf 9) (Leaf 6)))

maakBoom [x] = Leaf x
maakBoom xs = Branch (maakBoom eersteHelft) (maakBoom tweedeHelft)
    where aantal = div (length xs) 2
          eersteHelft = take aantal xs
          tweedeHelft = drop aantal xs
		  