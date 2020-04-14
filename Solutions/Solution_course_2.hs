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

facStaartRecursief n = hulpfac n 1

hulpfac 1 fac = fac
hulpfac n getal = hulpfac (n-1) (getal*n)


herm 1 = 1
herm n = (1/n)+herm(n-1)

myst 1 = 0
myst n | mod n 2 == 0 = 1 + myst (n `div` 2)
       | otherwise    = 1 + myst (3*n+1)

myst2 1 = [1]
myst2 n | mod n 2 == 0 = n:myst2 (n `div` 2)
        | otherwise    = n:myst2 (3*n+1)

lengteLangsteDeelrij [] = 0
lengteLangsteDeelrij lijst = lld lijst 1 1

lld [] huidigeLengte voorlopigLangste = voorlopigLangste
lld [x] huidigeLengte voorlopigLangste = voorlopigLangste 
lld (x1:x2:xs) huidigeLengte voorlopigLangste 
   | x1 < x2 = lld (x2:xs) (huidigeLengte+1) 
						 (max (huidigeLengte+1) voorlopigLangste)
   | otherwise = lld (x2:xs) 1 voorlopigLangste

lengteLangsteDeelrij2 lijst = lld2 lijst 0 0

lld2 [] huidigeLengte voorlopigLangste = voorlopigLangste
lld2 [x] huidigeLengte voorlopigLangste = max (huidigeLengte+1)
											  voorlopigLangste 
lld2 (x1:x2:xs) huidigeLengte voorlopigLangste 
   | x1 < x2 = lld2 (x2:xs) (huidigeLengte+1) 
						 (max (huidigeLengte+1) voorlopigLangste)
   | otherwise = lld2 (x2:xs) 0 (max (huidigeLengte+1) voorlopigLangste)

fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fib2 n = hulpfib n 1 1 1

hulpfib n teller fibTeller fibTellerPlus1 
  | n == teller = fibTeller
  | otherwise   = hulpfib n (teller+1) fibTellerPlus1 
									   (fibTeller + fibTellerPlus1)
{--
meestVoorkomende [x] = x
meestVoorkomende xs = mvHulp xs 0 0
  
mvHulp [] count getal = getal 
mvHulp (x:xs) count getal = 
	let (aantalX, zonderX) = telAndDrop x xs
	in if aantalX+1 > count then mvHulp zonderX aantalX+1 x
						    else mvHulp zonderX count getal
--}

telAndDrop _ [] aantal lijstZonder = (aantal, lijstZonder)
telAndDrop x (y:ys) aantal lijstZonder
   | x == y     = telAndDrop x ys (aantal + 1) lijstZonder
   | otherwise  = telAndDrop x ys aantal       (y:lijstZonder)   

zonder _ [] = []
zonder x (y:ys) | x == y    = zonder x ys
				| otherwise = y:(zonder x ys)

zonder2 x xs = [y | y <- xs , x /= y]
aantal2 x xs = length [y | y <- xs, y == x]

aantal x [] = 0
aantal x (y:ys) | x == y    = 1 + aantal x ys
				| otherwise = aantal x ys
				 
meestVoorkomende2 (x:xs) = mvHulp2 (isort (x:xs)) x 1 0 0

mvHulp2 [] huidigAantal huidigGetal aantal getal = 
   if huidigAantal > aantal then huidigGetal
							else getal
mvHulp2 [x] huidigAantal huidigGetal aantal getal  = 
   if huidigAantal > aantal then huidigGetal
							else getal
							
mvHulp2 (x1:x2:xs) huidigAantal huidigGetal aantal getal
   | x1 == x2 = mvHulp2 (x2:xs) (huidigAantal+1) huidigGetal aantal getal
   | aantal > huidigAantal = mvHulp2 (x2:xs) 1 x2 aantal getal
   | otherwise = mvHulp2 (x2:xs) 1 x2 (huidigAantal+1) huidigGetal

				 
delers x = [ y | y <- [1..x] , mod x y == 0]
isPriem x = length (delers x) == 2

herhaal 0 f waarde = waarde
herhaal n f waarde = herhaal (n-1) f (f waarde)

bubble [] = []
bubble [x] = [x]
bubble (x:y:ys) | x < y     = x:(bubble (y:ys))
				| otherwise = y:(bubble (x:ys))

bubbleSort xs = herhaal (length xs) bubble xs

selectionSort1 [] = []
selectionSort1 (x:xs) = 
   let kleinste = foldr min x xs
       zonderKleinste = [y | y <- (x:xs) , y /= kleinste]
   in kleinste:(selectionSort1 zonderKleinste)
   
selectionSort2 xs = ss2 xs []
ss2 [] gesorteerd = gesorteerd
ss2 (x:xs) groteWaarden =
   let grootste = foldr max x xs
       zonderGrootste = [y | y <- (x:xs) , y /= grootste]
   in ss2 zonderGrootste (grootste:groteWaarden)
 
