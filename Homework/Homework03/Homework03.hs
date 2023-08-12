-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

checkMonthlyConsumption :: Double -> Double -> Double -> String
checkMonthlyConsumption hourlyConsumption dailyHours maxMonthlyConsumption
    | monthlyUsage <= maxMonthlyConsumption = "Monthly consumption is within limits."
    | otherwise = "Monthly consumption exceeds the limit."
  where
    monthlyUsage = hourlyConsumption * dailyHours * 30


-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

checkMonthlyConsumptionWithExcess :: Double -> Double -> Double -> String
checkMonthlyConsumptionWithExcess hourlyConsumption dailyHours maxMonthlyConsumption
    | monthlyUsage <= maxMonthlyConsumption = "Monthly consumption is within limits."
    | otherwise = "Monthly consumption exceeds the limit by " ++ show (monthlyUsage - maxMonthlyConsumption) ++ " kW."
  where
    monthlyUsage = hourlyConsumption * dailyHours * 30


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

splitExpression :: Double -> Double -> Double
splitExpression x y = result
  where
    intermediate1 = x + y
    intermediate2 = x * y
    result = let squaredSum = intermediate1 * intermediate1
                 squaredProduct = intermediate2 * intermediate2
             in squaredSum + squaredProduct


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  

divideAndStringify :: Double -> Double -> String
divideAndStringify _ 0 = "Division by zero is not possible."
divideAndStringify x y
    | quotient <= 1 = show quotient
    | otherwise = "Quotient exceeds 1."
  where
    quotient = if y /= 0 then x / y else 0


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.

sumOfSqrtProductAndQuotient :: Double -> Double -> Double
sumOfSqrtProductAndQuotient x y = result
  where
    result = let
        productResult = productSqrt x y
        quotientResult = quotientSqrt x y
      in
        productResult + quotientResult

    productSqrt a b = sqrt (a * b)
    quotientSqrt a b = let
        quotientValue = if b /= 0 then a / b else 0
      in
        sqrt quotientValue

