f x = x + 3     -- functie met één parameter
g x y = x + y   -- functie met twee parameters

studenten = ["Tom", "Jons", "Simon"]
getallen = [2,3,4,5.5]

tweede [x,y] = y
tweede [x,y,z] = y
tweede [x,y,z,v] = y

twee (x1:x2:xs) = xs

laatste [x] = x
laatste (x:xs) = laatste xs

abs2 x = if x < 0 then -x
                  else x

som [] = 0
som (x:xs) = x + som xs

isIn x [] = False
isIn x (y:ys) = if x == y then True
                          else isIn x ys

isort [ ] = [ ]
isort (x:xs) = insert x (isort xs)

insert x [ ] = [x]
insert x (y:ys) = if (x<y) then (x:y:ys)
                           else y:(insert x ys)

halveer [] = []
halveer [x] = [x]
halveer (x:y:ys) = x:halveer ys

half [] = []
half xs = if even(length(xs)) then (half (tail xs))
                              else (head xs):(half (tail xs))

halfje [] = []
halfje (x:xs) = x:(onhalfje xs)

onhalfje [] = []
onhalfje (x:xs) = halfje xs


verdubbel [] = []
verdubbel (x:xs) = x:x:(verdubbel xs)

{-
	[ 1, 2, 3, 1, 2, 4, 1 ]

	1 : [2, 3, 1, 2, 4, 1 ]
	
	optie 1 = 1: ontdubbel (schrap 1 [2,3,1,2,4,1])

	optie 2 = als 1 voorkomt in de lijst
	              ontdubbel xs
		      anders 1: (ontdubbel xs)

-}

ontdubbel1 [] = []
ontdubbel1 (x:xs) = x:(ontdubbel1 (schrap x xs))

ontdubbel2 [] = []
ontdubbel2 (x:xs) | isIn x xs = ontdubbel2 xs
                  | otherwise = x:ontdubbel2 xs

ontdubbel3 [] = []
ontdubbel3 (x:xs) = voegEenKeerToe x (ontdubbel3 xs)

ontdubbel4 [] = []
ontdubbel4 (x:xs) = if isIn x staart then staart		
                                     else x:staart
    where staart = ontdubbel4 xs
	
schrap _ [] = []
schrap x (y:ys) | x == y = schrap x ys
                | otherwise = y:schrap x ys
				
voegEenKeerToe x [] = [x]
voegEenKeerToe x (y:ys) = if x == y then (y:ys)
                                    else y:(voegEenKeerToe x ys)
{-
   dit is commentaar g(x,y)
-}


fac 1 = 1
fac n = n * fac(n-1)  -- n! = n * (n-1)!

herm 1 = 1
herm n = (1/n)+herm(n-1)

myst 1 = 0
myst n | mod n 2 == 0 = 1 + myst (n `div` 2)
       | otherwise    = 1 + myst (3*n+1)

myst2 1 = [1]
myst2 n | mod n 2 == 0 = n:myst2 (n `div` 2)
        | otherwise    = n:myst2 (3*n+1)





