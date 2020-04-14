data Edit = Insert Char
          | Replace Char Char
          | Delete Char
          | DeleteAll String
          | Copy Char
          deriving (Eq, Show)
          
transformeer :: String -> String -> [[Edit]]
transformeer [] [] = [[]]
transformeer xs [] = [[DeleteAll xs]]
transformeer [] ys = [map Insert ys]
transformeer (x:xs) (y:ys) 
  | x == y = addEdit (Copy x) (transformeer xs ys)
  | otherwise =
    addEdit (Delete x) (transformeer xs (y:ys))
      ++
    addEdit (Insert y) (transformeer (x:xs) ys)
      ++
    addEdit (Replace x y ) (transformeer xs ys)
    
addEdit edit lijstVanLijsten = map (edit:) lijstVanLijsten

afstand :: String -> String -> Int
afstand s1 s2 = foldr min (head lengteEdits) lengteEdits
   where lijstEdits = transformeer s1 s2  -- lijst van editlijsten
         zonderCopy = map (filter isNotCopy) lijstEdits
         lengteEdits = map length zonderCopy -- lijst van lengtes
         
isNotCopy (Copy _) = False
isNotCopy _ = True
         

--afstandGeneriek :: String -> String -> ([Edit] -> Float) -> Float
afstandGeneriek kostfunctie s1 s2 = foldr min (head kostEdits) kostEdits
   where lijstEdits = transformeer s1 s2  -- lijst van editlijsten
         kostEdits = map kostfunctie lijstEdits -- lijst van lengtes

         
-- kostfunctie :: [Edit] -> Float
kostFunctieLineair lijst = length lijst 

kostFunctieCopyGratis lijst = length (filter isNotCopy lijst)           
       
kost :: Edit -> Int       
kost (Copy _) = 1
kost (Insert _) = 2
kost (Delete _) = 2
kost (Replace _ _) = 3
kost (DeleteAll str) = div (length str) 2
          
kostFunctieKost lijst = foldr (+) 0 (map kost lijst)
          
afstandLineair = afstandGeneriek kostFunctieLineair
afstandOrigineel = afstandGeneriek kostFunctieCopyGratis
afstand3 = afstandGeneriek kostFunctieKost
         
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Eq, Show, Ord)

maakBoom [x] = Leaf x
maakBoom xs = Branch (maakBoom eersteHelft) (maakBoom tweedeHelft)
    where aantal = div (length xs) 2
          eersteHelft = take aantal xs
          tweedeHelft = drop aantal xs
