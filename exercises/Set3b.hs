-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo


------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList start count end
  | count > 0 = start : buildList start (count - 1) end
  | otherwise = [end]


------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function

sum :: Num a => [a] -> a
sum [] = 0                  -- Base case: the sum of an empty list is 0
sum (x:xs) = x + sum xs    -- Recursive case: add the head of the list to the sum of the tail

sums :: Int -> [Int]
sums i = helper 1
  where
    helper n
      | n <= i = sum [1..n] : helper (n + 1)
      | otherwise = []


------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def [] = def
mylast _ [x] = x
mylast def (_:xs) = mylast def xs


------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def  -- Case when list is exhausted
indexDefault (x:xs) i def
  | i < 0     = def  -- Negative index case
  | i == 0    = x    -- Desired index is found
  | otherwise = indexDefault xs (i - 1) def  -- Recurse with tail and decremented index


------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.
--
-- Examples:
--   sorted [1,2,3] ==> True
--   sorted []      ==> True
--   sorted [2,7,7] ==> True
--   sorted [1,3,2] ==> False
--   sorted [7,2,7] ==> False

sorted :: [Int] -> Bool
sorted [] = True  -- An empty list is considered sorted
sorted [_] = True  -- A single-element list is considered sorted
sorted (x:y:xs)
  | x <= y    = sorted (y:xs)  -- If the current pair is in order, proceed to check the rest of the list
  | otherwise = False  -- If any pair is out of order, the whole list is not sorted


------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

sumsOf :: [Int] -> [Int]
sumsOf xs = go xs 0  -- Start the recursion with the original list and an initial total of 0
  where
    go :: [Int] -> Int -> [Int]  -- Helper function that takes the remaining list and the current total
    go [] _ = []  -- Base case: if the list is empty, return an empty list
    go (x:xs) total = (x + total) : go xs (x + total)  -- Recursive case: add the current element to the total and prepend it to the result of the recursive call


------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]


merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys  -- If the first list is empty, the result is the second list
merge xs [] = xs  -- If the second list is empty, the result is the first list
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)  -- If the head of the first list is smaller, prepend it to the merged result
  | otherwise = y : merge (x:xs) ys  -- If the head of the second list is smaller, prepend it to the merged result


------------------------------------------------------------------------------
-- Ex 8: compute the biggest element, using a comparison function
-- passed as an argument.
--
-- That is, implement the function mymaximum that takes
--
-- * a function `bigger` :: a -> a -> Bool
-- * a value `initial` of type a
-- * a list `xs` of values of type a
--
-- and returns the biggest value it sees, considering both `initial`
-- and all element in `xs`.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\(a,b) (c,d) -> b > d) ("",0) [("Banana",7),("Mouse",8)]
--     ==> ("Mouse",8)

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a
mymaximum _ initial [] = initial  -- If the list is empty, return the initial value
mymaximum bigger initial (x:xs)
  | bigger x initial = mymaximum bigger x xs  -- If x is bigger, use x as the new initial value for the rest of the list
  | otherwise        = mymaximum bigger initial xs  -- Otherwise, keep the current initial value


------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []  -- If the first list is empty, return an empty list
map2 _ _ [] = []  -- If the second list is empty, return an empty list
map2 f (a:as) (b:bs) = f a b : map2 f as bs  -- Apply f to the heads of the lists, and recurse on the tails



------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []  -- If the input list is empty, the result is an empty list
maybeMap f (x:xs) = case f x of
    Just y -> y : maybeMap f xs  -- If f x is Just y, include y in the result list
    Nothing -> maybeMap f xs     -- If f x is Nothing, skip x and proceed with the rest of the list
