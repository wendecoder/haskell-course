
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)
f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)
f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)
f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]
f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{-Defining type signatures for functions is an essential practice in Haskell programming for several reasons:

    Documentation: Type signatures serve as documentation for your functions. They provide information about the input types and the output type, making it easier for both you and others to understand the intended purpose of the function.

    Compile-Time Errors: Type signatures help catch type-related errors at compile time. If there's a mismatch between the declared type and the actual implementation, the compiler will raise an error, helping you identify and fix the issue early in the development process.

    Code Clarity: Explicit type signatures make your code more readable and self-explanatory. They act as a form of communication between developers, conveying the expected types and aiding in understanding the codebase.

    Polymorphism: Type signatures allow you to define polymorphic functions that work with multiple types, providing flexibility and reusability in your code.
-}
-- Question 3
-- Why should you define type signatures for variables? How can they help you?
{-In Haskell, you cannot define type signatures for variables explicitly. Haskell is a statically typed language, and the type of a variable is inferred by the compiler based on how it is used in the code. When you define a variable, its type is determined at compile time, and it remains fixed throughout its scope. This process is known as "type inference."

By relying on type inference, Haskell provides a strong guarantee about type safety without the need for explicit type annotations on variables. This approach ensures that any type-related issues are caught during compilation, reducing the chance of runtime errors related to type mismatches.
-}
-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

{-Yes, Haskell provides various functions for converting between different types. Some commonly used type conversion functions are:

    fromIntegral: Used to convert between integral types (e.g., Int, Integer, Word, etc.) and floating-point types (Float and Double).

    toInteger: Converts an integral type to an arbitrary precision integer (Integer).

    toEnum: Converts an Int to an enumeration type.

    fromEnum: Converts an enumeration type to an Int.

    read: Parses a String and converts it to the desired type (requires the target type to be explicitly specified).

    show: Converts a value to its string representation.
-}
-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
-- Yes, you can define a list of lists in Haskell. It's also known as a 2-dimensional list or a list of lists. Here's an example of how you can define a list of lists:
listOfLists :: [[Int]]
listOfLists = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- To access the innermost elements, you can use indexing with double square brackets:
-- Accessing the element at row 2, column 3 (indexing is 0-based)
element :: Int
element = listOfLists !! 1 !! 2
-- The value of `element` would be 6.
