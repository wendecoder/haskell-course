# HaskellBootcamp

Repository containing the lectures and homework of the IOG's Haskell Bootcamp

## Repository structure

    Haskell-Bootcamp
        |   |
        |   |---- code
        |          |
        |          |---- Homework01 (homework for lesson 01)
        |          |---- Homework02 (homework for lesson 02)
        |          ...
        |
        |-------- lessons <- Lessons in Juptyer notebook format (accessed
                   |      through Binder. See instructions below.)
                   |
                   |---- 1-Introduction-to-haskell
                   |---- 2-Functions-Data-Types-and-Signatures

Everything else can be safely ignored

## To view the interactive lessons click on the button below

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/input-output-hk/haskell-for-marlowe-bootcamp/HEAD?labpath=%2Flessons%2F%2FWeek01%2F1.1-Introduction-to-haskell.ipynb)

## To do the homework

1. Clone this repository.
2. Create a [GitPod](https://www.gitpod.io/) account.
3. Select the `code/WeekXX` folder with the homework you want to complete.
4. Click this button to create a remote dev environment: [![Visual Studio Code](https://img.shields.io/badge/Visual%20Studio%20Code-0078d7.svg?style=flat&logo=visual-studio-code&logoColor=white)](https://gitpod.io/#https://github.com/rober-m/haskell-bootcamp)
5. Follow the instructions inside the app/Main.hs file.

## What we'll cover

### 1. Intro and tools

- Intro to the course and lectures
  - What we’ll cover
  - Repository structure
- Intro to Haskell
  - How to open and use JupyterLab
  - Purely functional programming language
  - Basic syntax
  - Haskell Type system
  - Lazyness
  - GHC (and GHCi)
- GitPod
  - How to open and use GitPod
  - Example of how to complete a homework assignment.

### 2. Functions, Data types, and Signatures

- Intro to types and signatures
- Fraquently used types
- Function’s signatures
- Infix and prefix functions
- Types in depth
  - Numeric types
    - Int, Integer
    - Float, Double
    - Rational
  - Bool
  - Char
  - Lists
  - Strings
  - Tuples + Tuples VS Lists
- Variables in Haskell
  - Parameters in functions
  - Names/Definitions
- Polymorphic values and type variables

### 3. Pattern matching, let, where

- If-then-else
- What is pattern matching
- Where can I use pattern matching + nested pattern matching
  - Pattern matching on function implementations
  - Pattern matching on on lists
  - Pattern matching on tuples
- Let and where
- Case
- Guards
- When to pattern match, if, case, and guards

### 4. More on functions and lists

- Higher order functions
- Curied functions
- Partial application
- Composing and applying functions (`.` and `$` operators)
- Recursion
- Dealing with lists
  - `zip`
  - `map`
  - `foldl`, `foldr`, `scan`
- Lambda functions

### 5. Intro to Type Classes

- What are type classes?
- Common type classes
  - `Eq`
  - `Ord`
  - `Integral`
  - `Floating`
  - `Num`
  - Mentioning `Read`, `Show`, `Enum`, `Bounded`, and `Foldable`.
- Class constraints with examples

### 6. Creating Types

- Type synonyms
  - How to define type synonyms
  - Why use type synonyms
- Defining new types
  - `data`
  - Value parameters
  - Pattern matching types
  - Record syntax
- Parameterizing types
  - Parameterizing type synonyms
  - Parameterizing new types
- Honorable mention of `newType`

#### 7. Creating Type Classes and Instances

- #TODO

#### Basic IO

- We need side effects
- What is IO
- main + putStrLn + composing other functions
- `>>`
- `>>=`
- do notation
  - `do`
  - `<-`
  - `let`
- Some examples
- Read/Write to console
- Read/Write to file

### Complete project using IO

- #TODO

### Pragmas, Modules, and Cabal

- Prelude
- pragmas/extensions
- Overview of base modules
- Importing base modules
- A few modules
  - Data.Char
  - Data.Tuple
  - Data.Array
- Creating our own modules
- Cabal
  - What is it and why we use it
  - Cabal file
  - Using external libraries with Cabal

### Learning on you own and Map

- Using GHCi to find out more
- Hoogle
- HaskellWiki
- Walking through while teaching Map module

### Maybe and Either (only practical use)

- Maybe
  - Why and when to use Maybe
  - Syntax
  - Examples
- Either
  - Why and when to use Either
  - Syntax
  - Examples
- Project using Maybe and IO

### Final project

- Final project recapping all the concepts

**If you want to continue your Haskell education, we invite you to complete the [Haskell for Plutus](#todo) bootcamp.**
