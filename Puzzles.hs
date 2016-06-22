module Puzzles where
import Prelude hiding (zipWith)

--ALEC WEBB
--haskell exercises

--slow implementation of fib
slow_fib :: Int -> Int
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-1) + slow_fib (n-2)

--use an infinite list and find the position associated with the input
fiblist = 0 : 1 : zipWith (+) fiblist (tail fiblist)
fib :: Int -> Int
fib x = fiblist !! x

--pull the last element and concatenate it with the list excluding that element
reversed :: (Eq a) => [a] -> [a]
reversed [] = []
reversed [x] = [x]
reversed xs = last xs : reversed (init xs)

--use a list comprehension to calculate the values which n is divisible by
--if the list is empty then the number is prime
--here i limit the list to half of the input, but could sqrt for more speed
prime :: Int -> Bool
prime n = if n < 2 then False else null [ x | x <- [2..n `div` 2], n `mod` x == 0]

--use the fold from the left function
--use an empty list as the starting value
--determine if x is in the list, if not append it, if it is do not
nub :: (Eq a) => [a] -> [a]
nub = foldl (\scanned x -> if x `elem` scanned then scanned else scanned ++ [x]) []

--the function f takes the head of both lists and then operates on the rest of the lists
--creating a new list in the process
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

--coll calculates the next step of collatz
coll :: Int -> Int
coll n = if (odd n) then (n*3+1) else n `div` 2

--if n is 1 the list is 1, or prepend n with the list generated
--by collatz
collatz :: Int -> [Int]
collatz n
       | n == 1 = [n]
       | otherwise = n : collatz (coll n)

--mean helper function
mean :: [Int] -> Double
mean xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

--quicksort implementation found @ learnyouahaskell.com
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallersorted = quicksort [a | a <- xs, a <= x]
        biggersorted  = quicksort [a | a <- xs, a > x]
    in  smallersorted ++ [x] ++ biggersorted

--these are helper function for the median function
middle xs = (quicksort xs)!!(length xs `div` 2)
middletwo xs = (quicksort xs)!!(length xs `div` 2 - 1)
--if the list is odd then return the element at the halfpoint
--if the list is even aveage the two numbers in the middle
--use quicksort to put the list in order to do these operations
median :: [Int] -> Double
median xs = if length xs `mod` 2 /= 0
            then (fromIntegral (middle xs)) / 1
            else (fromIntegral (middle xs + middletwo xs)) / 2

--this function takes a number and a list as input and
--counts the number of times that number appears
counter :: Eq a => a -> [a] -> Int
counter i [] = 0
counter i (x:xs)
        | i == x = 1 + counter i xs
        | otherwise = counter i xs

--use a list comprehension to build a list with the unique elements
--and the counts associated with them when given a list
--list produced has tuples of the form (count, item)
freq xs = [(counter c xs, c) | c <- nub xs]

--(fst, snd)
--this finds the maximum count associated with any number in the frequency list
tupmax xs = maximum (fst (unzip (freq xs)))

--use a list comprehension to build a list by comparing the count value
--to the max found in tupmax
mode :: (Ord a) => [a] -> [a]
mode xs = [y | (x,y) <- freq xs, x == tupmax xs]

--use the helper functions
--need to coerce mean and median into type double..
listReport :: [Int] -> (Double, Double, [Int])
listReport xs = (mean xs, median xs, mode xs)
