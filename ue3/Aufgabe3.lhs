-------------------- Aufgabe 1 - START -----------------------

> type Zahlenliste		= [Integer]
> type Tripelprimzahl	= Integer 
> type Counter			= Integer

Als Parameter wird eine beliebige Zahlenliste uebergeben.
Hierbei wird jede Zahl in Primfaktoren zerlegt mithilfe von
"primFactors" und wenn die Laenge der Liste von Primfaktoren
genau 3 ergibt, so ist die Zahl eine Tripelprimzahl.
Ansonsten nicht und wird ignoriert

> schuerfen :: Zahlenliste -> [Tripelprimzahl]
> schuerfen [] = []
> schuerfen n@(x:xs)
>  | length (primFactors x primes 0) == 3 = x : schuerfen xs
>  | otherwise = schuerfen xs


Parameter:
 Integer 		-> Die Zahl selber .. a
 Zahlenliste 	-> Die (unendliche) Primzahlen
 Counter		-> Zaehler .. maximal 3
 
Ergebnis:
 Zahlenliste	-> Drei aufeinanderfolgende Primzahlen

Die Liste unendlicher Primzahlen ist definiert als
n@(x:xs) .. x ist der head, xs der Rest

Rekursionsablauf:
 a `mod` x == 0 = primFactors xs
 
 Also wenn a durch x 0 ergibt, so such mit dem Rest der
 Liste weiter.

> primFactors :: Integer -> Zahlenliste -> Counter -> Zahlenliste
> primFactors 0 _ _ = [0]
> primFactors a n@(x:xs) b
>  | b > 3 = []
>  | n !! 0 * n !! 1 * n !! 2 == a = [n !! 0, n !! 1, n !! 2]
>  | n !! 0 * n !! 1 * n !! 2 > a = [0,0,0,0]
>  | a `mod` x == 0 = x : primFactors a xs (b+1)
>  | otherwise = primFactors a xs b



Erzeuge eine unendliche Liste von Primzahlen
mithilfe von Sieb des Erathosthenes S.35

> primes :: Zahlenliste
> primes = sieve [2..]
>
> sieve :: Zahlenliste -> Zahlenliste
> sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]




-------------------- Aufgabe 1 - ENDE -----------------------

-------------------- Aufgabe 2 - START -----------------------

Source: 
Definition .. http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#t:Num
Implementierung .. https://stackoverflow.com/questions/27462918/creating-an-instance-of-num-class
Int Implementierung .. http://hackage.haskell.org/package/base-4.7.0.1/docs/src/GHC-Num.html#negate

Vorgehen:
Zuerst mal googeln wie das mit instance genauer funktioniert (siehe Implementierung)
Die Stelle (Scalar i1) + (Scalar i2) = Scalar (i1 + i2) zeigt, dass es
ähnlich wie ein Pattern Matching ist.
Ein Kurs hat immer das Format "K 5.5", kann man ausprobieren im Interpreter.
Wenn man versucht "K 5.5 + K 7", dann erscheint ein Fehler, da der
Interpreter keine Ahnung hat was abgeht. Nun zeigt man dem Typ, dass man
die Zahlen addiert und das "K" nur mitschleift und nichts besonders macht.

Auf der hackage haskell Seite sieht man die "minimal Implementierung" fuer
Num und das ist + * abs signum fromInteger und (negate oder -)
Natuerlich ist negate viel einfacher, also go.

Vieles kann man aus dem Sourcecode vom Int kopieren

> newtype Kurs			= K Float deriving (Eq, Ord, Show)
> newtype Pegelstand	= Pgl Float deriving (Eq, Ord, Show)


> instance Num Kurs where
>  negate (K k1) = K (0 - k1)
>  (K k1) + (K k2) = K (k1 + k2)
>  (K k1) - (K k2) = K (k1 - k2)
>  (K k1) * (K k2) = K (k1 * k2)
>  abs (K k1) = if k1 < 0 then negate (K k1) else K k1
>  signum (K k1) = K (signum k1)
>  fromInteger k1 = K (fromInteger k1)

> instance Num Pegelstand where
>  negate (Pgl k1) = Pgl (0 - k1)
>  (Pgl k1) + (Pgl k2) = Pgl (k1 + k2)
>  (Pgl k1) - (Pgl k2) = Pgl (k1 - k2)
>  (Pgl k1) * (Pgl k2) = Pgl (k1 * k2)
>  abs (Pgl k1) = if k1 < 0 then negate (Pgl k1) else Pgl k1
>  signum (Pgl k1) = Pgl (signum k1)
>  fromInteger k1 = Pgl (fromInteger k1)


-------------------- Aufgabe 2 - ENDE -----------------------



-------------------- Aufgabe 3 - START -----------------------

> curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
> curry3 f a b c = f (a,b,c)

> curry3Test :: (Integer, Integer, Integer) -> Integer
> curry3Test (a,b,c) = a * b * c



> uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
> uncurry3 f (a,b,c) = f a b c
>
> uncurryTest :: Bool -> Bool -> Bool -> Int
> uncurryTest f (a,b,c) = f a b c


> curry_flip :: ((a,b) -> c) -> (b -> a -> c)
> curry_flip f b a = f (a,b)
>
> cftest :: (Integer, Integer) -> Integer
> cftest  f(b, a) = f a b



> uncurry_flip :: (a -> b -> c) -> ((b,a) -> c
> uncurry_flip f (b,a) = f a b
>
> uftest :: Int -> Int -> Int
> uftest a b = a - b



-------------------- Aufgabe 3 - ENDE -----------------------



-------------------- Aufgabe 4 - START -----------------------

CopyPaste von Aufgabe 1
Source: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell

Per Parameter 3 Listen von Zahlen.
Die Elemente nach der Reihe von der ersten, zweiten und dritten
Liste nehmen und in eine neue Liste aneinenader reihen.

zB [1,2,3] [4,5,6] [7,8,9] = [1,4,7,2,5,8,3,6,9]

> verflechten3 :: [Int] -> [Int] -> [Int] -> [Int]
> verflechten3 [] [] [] = []
> verflechten3 a  [] [] = a
> verflechten3 [] b  [] = b
> verflechten3 [] [] c  = c
> verflechten3 a  b  [] = verflechten a b
> verflechten3 [] b  c  = verflechten b c
> verflechten3 a  [] c  = verflechten a c
> verflechten3 a@(x:xs) b@(y:ys) c@(z:zs) = x : y : z : verflechten3 xs ys zs


> verflechten :: [Int] -> [Int] -> [Int]
> verflechten xs [] = xs
> verflechten [] ys = ys
> verflechten (x:xs) (y:ys) = x : y : verflechten xs ys


-------------------- Aufgabe 4 - ENDE -----------------------
