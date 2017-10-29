-- Aufgabe 1 - Lotto

type Nat0               = Integer
type Nat1               = Integer
type GesamtKugelZahl    = Nat1
type GezogeneKugelZahl  = Nat1
type Spiel              = (GesamtKugelZahl, GezogeneKugelZahl)
type Gluecksspiel       = (Spiel, Spiel)

anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis (a,b) = binomK a * binomK b

binomK :: Spiel -> Nat0
binomK (n,k)
 | (n >= k) && n >= 0 && k >= 0 = div (fac n) (fac k * fac(n - k))
 | otherwise = 0

fac :: Nat1 -> Nat0
fac n = if n == 0 then 1 else n * fac(n-1)



-- Aufgabe 2 - Fibonacci
--
-- Source: https://wiki.haskell.org/The_Fibonacci_sequence
--            Binet's formula 
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

-- Infinite List of Fibnumbers
fibList = map fib [0..]

fib' :: Nat0 -> Nat0
fib' n = nthFib n fibList

-- Searches for the nth fib number
nthFib :: Nat0 -> [Nat0] -> Nat0
nthFib = indexInList 0


-- Searches through the list and returns an index of the wanted element
-- x:xs x..head, xs..tail
-- i .. counter variable
indexInList :: Nat0 -> Nat0 -> [Nat0] -> Nat0
indexInList i a (x:xs)
 | a == x = i
 | x < a = indexInList a (i+1) xs
 | otherwise = a


-- AUFGABE 3, nth Fib number
-- Returns an ascending list of fib number until the nth number
fibs n = map fib [0..n]


-- AUFGABE 4, merging two lists
-- Source: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell

verflechten :: [Int] -> [Int] -> [Int]
verflechten xs [] = xs
verflechten [] ys = ys
verflechten (x:xs) (y:ys) = x : y : verflechten xs ys




