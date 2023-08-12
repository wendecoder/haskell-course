-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

getValue :: [([Int], [Int])] -> Int
getValue (((_:_):(x:_)):_) = x
getValue _ = error "Value not found in nested list"


-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

-- Multiple function definition
removeElementsIf3OrMore :: [a] -> [a]
removeElementsIf3OrMore xs
    | length xs >= 3 = []
    | otherwise = xs

-- Case expressions
removeElementsIf3OrMore' :: [a] -> [a]
removeElementsIf3OrMore' xs = case length xs >= 3 of
    True -> []
    _ -> xs


-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

sumTuple3 :: (Integer, Integer, Integer) -> Integer
sumTuple3 (a, b, c) = a + b + c


-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False


-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs


-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

addOneIfEven :: Int -> Int
addOneIfEven n = case even n of
    True -> n + 1
    _ -> n

