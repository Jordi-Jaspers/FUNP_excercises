-- Afstand tussen twee strings is het minimaal aantal wijzigingen nodig om van de ene tot de andere string te komen
-- Afstand is niet commutatief!
-- Afstand van s1 tot s2 != afstand van s2 tot s1
-- Afstand van “hallo” tot “h” = 1 want “allo” schrappen is één wijziging
-- Afstand van “h” tot “hallo” = 4 want 4 letters toe te voegen


-- Data type/Structure that consist of inserting, deleting or swapping a letter or deleting the rest of the string
data Edit   = Delete Char 
            | Insert Char 
            | Swap Char Char 
            | DeleteAll String 
            | Copy Char 
            deriving (Eq, Show)

-- head -> first element of the array
-- foldr -> Start from the right side and applies a function with a starting value on each element on that list and return one value
-- listEdit -> List of edit all the edit we did (list of list)
-- lengthEdit -> List of length (list)
distance    :: String -> String -> Int
distance s1 s2  = foldr min (head lengthEdit) lengthEdit
            where   listEdit = transform s1 s2
                    withoutCopy = map (filter noCopy) listEdit
                    lengthEdit = map length listEdit   


-- 2 Strings (list of char) transforming into a solution
-- map method -> applies function on each element and returns a list
transform   :: String -> String -> [[Edit]] 
transform   [] [] = [[]]
transform   xs [] = [[DeleteAll xs]]
transform   [] ys = [map Insert ys]
transform   (x:xs) (y:ys)
        |   x == y = addEdit (Copy x) (transform xs ys)
        |   otherwise = 
                addEdit (Delete x) (transform xs (y:ys))
                    ++
                addEdit (Insert y) (transform (x:xs) ys)
                    ++
                addEdit (Swap x y) (transform xs ys)   

-- applies the given edit function on the word we need
addEdit editFunction listOfLists = map (editFunction:) listOfLists

-- deletes the copies before applying the function
noCopy (Copy _) = False
noCopy (_) = True

distanceGenerator s1 s2 costFunction = foldr min (head costEdit) costEdit
    where   listEdit = transform s1 s2
            costEdit = map costFunction listEdit

-- costFunction :: [Edit] -> Float
costFunctionLinear list = length list
costFunctionCopyGratis list = length (filter noCopy list)


