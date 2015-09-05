--type is Pascal case
--function is camel case


sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n-1)

fact 0 = 1
fact 1 = 1
fact n = n * fact (n-1)

text "" = "Hi"
text s = "Hi " ++ s ++ " krub"

mylast [last] = last
mylast (x:xs) = mylast xs

myTwoLast [a,b] = [a,b]
myTwoLast (x:xs) = myTwoLast xs

len :: [Int] -> Int
len [] = 0
len (x:xs) = len xs +1

sumList :: [Int] -> Int
sumList [] = 0
sumList [a] = a
sumList [a,b] = a + b
sumList (x:xs) = x + sumList xs

doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList [a] = [2 * a]
doubleList [a,b] = [2 * a, 2* b]
doubleList [a,b,c] = [2 * a, 2* b, 2* c]
doubleList (x:xs) = (2 * x) : doubleList xs

double :: Int -> Int
double a = a *2

power :: Int -> Int
power a = a * a

doList :: (Int -> Int) -> [Int] -> [Int]
doList _ [] = []
doList fn [a] = [fn a]
doList fn [a,b] = [fn a, fn b]
doList fn (x:xs) = fn x : doList fn xs

absolute x = if x > 0 then x else -x

evenInt :: Int -> Bool
evenInt n = (mod n 2) == 0


filterInt :: [Int] -> (Int -> Bool) -> [Int]
filterInt [] _ = []
filterInt (x:xs) fn = if fn x then (x: filterInt xs fn) else filterInt xs fn

qsort [] = []
qsort (x:xs) = (qsort [l | l <- xs, l < x]) ++ [x] ++ (qsort [r | r <- xs, r >= x])
