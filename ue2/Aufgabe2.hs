import Data.List

-------------------- Aufgabe 1 - START -----------------------

type N1 = Int

-- Idee:
-- Man nehme beide Zahlen und suche den ggt
-- Der gefundene ggt wird durch die genueberliegende Zahl dividiert
-- zB n / ggt(m,n)  und  m / ggt(m,n)
p2p :: (N1,N1) -> (N1,N1)
p2p (m, n) 
 | m == n 		= (1,1) 
 | otherwise 	= ( n `div` (ggt(m, n)), m `div` (ggt(m, n)))


-- Findet den ggt eines Tupels
ggt :: (N1,N1) -> N1
ggt (m, n)
 | m == 0 		= n
 | n == 0 		= m
 | n > m 		= ggt(n, m)
 | otherwise 	= ggt(m `mod` n, n)


-- Zu Langsam
--
--p2p :: (N1, N1) -> (N1, N1)
--p2p (m,n) = findQP m n 1 1

-- p und q sind quasi Counter Variablen
-- es wird jeweils von 1 ausgegangen als Multiplikator
-- 
-- Vorgang:
--  Berechne das maximum von m und n
--  Das maximum ist jeweils das Ziel fuer die kleiner zahl
--  wenn die kleinere Zahl das maximum ueberschritten hat
--  ohne dass die Gleichung erfuellt ist, so wird das
--  maximum erhoeht
--findQP :: N1 -> N1 -> N1 -> N1 -> (N1,N1)
--findQP m n p q
--- | m * p == n * q = (p,q)
-- | m * p < n * q = findQP m n (p+1) q
-- | otherwise = findQP m n p (q+1)



-------------------- Aufgabe 1 - ENDE -----------------------


-------------------- Aufgabe 2 - START -----------------------

type Nat0               = Integer
type Nat1               = Integer
type GesamtKugelZahl    = Nat1
type GezogeneKugelZahl  = Nat1
type Spiel              = (GesamtKugelZahl, GezogeneKugelZahl)
type Gluecksspiel       = (Spiel, Spiel)
type AngeboteneSpiele   = [Gluecksspiel]

anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis (a,b) = binomK a * binomK b

-- Sortiert nach Anzahl Wettkombinationen und dann nach
-- gezogenen Kugeln
-- Ist die Anzahl der Kombinationen 0, so wird dieses Element
-- ausgelassen
attraktiveSpieleVorne :: AngeboteneSpiele -> [Gluecksspiel]
attraktiveSpieleVorne [] = []
attraktiveSpieleVorne [((a,b),(c,d))] = [((a,b),(c,d))]
attraktiveSpieleVorne n@(x:xs)
 | anzahlWettKombis x == 0 = attraktiveSpieleVorne xs
 | otherwise = sortBy compareSpiele n

-- Custom comparator
-- 
-- Source: https://stackoverflow.com/questions/5965326/haskell-sortby-function
-- 
-- Merge Sort von Hoogle... Data.List
-- http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-List.html#v:sortBy
--
-- Erstellen eines Custom Comparators um zuerst nach
-- Anzahl der Wettkombinationen zu sortieren
-- und danach nach gezogenenen Kugeln
compareSpiele :: Gluecksspiel -> Gluecksspiel -> Ordering
compareSpiele x y
 | anzahlWettKombis x < anzahlWettKombis y = LT
 | anzahlWettKombis x > anzahlWettKombis y = GT
 | snd (snd x) > snd (snd y) = GT
 | snd (snd x) < snd (snd y) = LT
 | otherwise = GT


binomK :: Spiel -> Nat0
binomK (n,k)
 | (n >= k) && n >= 0 && k >= 0 = div (fac n) (fac k * fac(n - k))
 | otherwise = 0

fac :: Nat1 -> Nat0
fac n = if n == 0 then 1 else n * fac(n-1)


-------------------- Aufgabe 2 - ENDE -----------------------



-------------------- Aufgabe 3 - START -----------------------
--
-- Eine Zahl kommt ins Toepfchen wenn die Anzahl der Einser
-- im 3er System durch 3, ohne Rest, teilbar ist
-- andernfalls in Kroepchen

type Toepfchen	 = [Int]
type Kroepfchen	 = [Int]
type Zahlenliste = [Int]

sortiere :: Zahlenliste -> (Toepfchen, Kroepfchen)
sortiere n = (setToepfchen n, setKroepfchen n)

aufteilen :: Zahlenliste -> (Toepfchen, Kroepfchen)
aufteilen n = (setToepfchen n, setKroepfchen n)


-- Toepfchen wenn Zahl im 3er System in der Summe
-- der Einser durch 3 teilbar ist
setToepfchen :: Zahlenliste -> Zahlenliste
setToepfchen [] = []
setToepfchen n@(x:xs)
 | testChain (head n) = x : setToepfchen xs
 | otherwise = setToepfchen xs

-- Kroepchen wenn Zahl im 3er System in der Summe
-- der Einser NICHT durch 3 teilbar ist
setKroepfchen :: Zahlenliste -> Zahlenliste
setKroepfchen [] = []
setKroepfchen n@(x:xs)
 | not (testChain (head n)) = x : setKroepfchen xs
 | otherwise = setKroepfchen xs


-- Wandelt die Zahl ins 3er System um
-- und ueberprueft ob durch 3 teilbar .. TRUE
-- sonst FALSE
testChain :: Int -> Bool
testChain n
 | testOne (extractOne n 0) = True
 | otherwise = False


-- Extrahiert die Einser aus einer Zahl n
--
-- Einfache Modulo Rechnung und nach jedem Schritt
-- wird die Zahl durch 3 genommen, quasi um eine
-- Stelle im ternaeren System weiter
--
-- Param 1 .. die Zahl selber, also n
-- Param 2 .. x zaehlt die Vorkommen aller Einser
extractOne :: Int -> Int -> Int
extractOne n x
 | n == 1 = x+1
 | n == 2 = x
 | n == 0 = x
 | n `mod` 3 == 1 = extractOne (n `div` 3) x+1
 | otherwise = extractOne (n `div` 3) x

-- Hilfsmethode zur Ueberpruefung ob
-- die uebergebene Zahl durch 3 Teilbar ist 
-- ohne Rest
testOne :: Int -> Bool
testOne n
 | n `mod` 3 == 0 = True
 | otherwise = False

-------------------- Aufgabe 3 - ENDE -----------------------


-------------------- Aufgabe 4 - START -----------------------

type Nat = [Int]
--ziffern = [0,1,2,3,4,5,6,7,8,9] :: Int

-- Ueberprueft ob Zahlenliste richtig dargestellt ist
--
-- Richtig bedeutet, die Zahl darf nur aus einer einzigen
-- Ziffer bestehen und diese im Zahlenbereich von 
-- 0-9 liegen. Keine negativen Zahlen oder zweistelligen
-- Zahlen.
istGueltig :: Nat -> Bool
istGueltig [] = True
istGueltig (x:xs)
 | x < 0 = False
 | x > 9 = False
 | otherwise = True && istGueltig xs


-- Bringt eine Zahlenliste in die Normalform
--
-- Die Normalform ist eine Liste ohne fuehrende Nullen
-- Und diese muss auch wiederum eine richtige Zahlenliste
-- darstellen
normalForm :: Nat -> Nat
normalForm [0] = [0]
normalForm [] = []
normalForm n@(x:xs)
 | x == 0 = normalForm xs
 | istGueltig n = n
 | otherwise = []


-- Counts the amount of digits of given Int
length' :: Int -> Int
length' n
 | n < 10 = 1
 | n >= 10 = 1 + length' (n `div` 10)
 | otherwise = 0

-- Die Idee ist, dass zuerst die Liste von Ziffern [Int]
-- in eine Zahl umgewandelt wird
-- Danach wird addiert und subtrahiert und danach wieder
-- in Ziffern umgewandelt und darauf die normalForm errechnet

addiere :: Nat -> Nat -> Nat
addiere m n = normalForm (digs(fromDigits m + fromDigits n))

subtrahiere :: Nat -> Nat -> Nat
subtrahiere m@(x:xs) n@(y:ys)
 | xs == [] && ys == [] = []
 | checkForNeg (fromDigits m) (fromDigits n) = [0]
 | otherwise = normalForm (digs(fromDigits m - fromDigits n))

checkForNeg :: Int -> Int -> Bool
checkForNeg m n
 | m < n = True
 | otherwise = False
 

-- Source: https://stackoverflow.com/a/29336839
-- Wandelt eine Liste von Int in ein komplette Zahl um
fromDigits :: [Int] -> Int
fromDigits xs = aux xs 0
    where aux [] acc = acc
          aux (x:xs) acc  = aux xs ((acc * 10) + x)

-- https://stackoverflow.com/a/3963286
-- Wandelt eine Zahl in eine Liste von Ziffern um
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]




-------------------- Aufgabe 4 - ENDE -----------------------


