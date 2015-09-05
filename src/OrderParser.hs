module OrderParser where

import Data.Maybe (fromJust)

type Cash = Int
type Price = Int
type WrappersNeeded = Int
type ChocolateType = String
data Candy = Candy Cash Price WrappersNeeded ChocolateType deriving Show

-- Read from the file
-- Get the header, skip the newline, get the rest
-- Split the lines and zip those babies up (into a map-like structure)
-- Turn it all into some delicious candy
-- Oh, and it can deal with the header being in any order
run = do
  content <- readFile "input.txt"
  mapM_ print $ parseInputFile content

run2 = do
  content <- readFile "input2.txt"
  mapM_ print $ parseInputFile content

parseInputFile content = orders
  where (header:_:orderLines) = lines content
        splitHeader = splitLine header
        headerWithValues = map (zip splitHeader . splitLine) orderLines
        orders = map clean headerWithValues

-- Turn a line from the input file into a Candy
-- This is where some hard-coding lives, since the header
-- fields aren't able to be encoded statically
-- Deal with it
clean order = Candy cash price wrappersNeeded candyType
  where cash = read $ fromJust $ lookup "cash" order :: Int
        price = read $ fromJust $ lookup "price" order :: Int
        wrappersNeeded = read $ fromJust $ lookup "wrappers needed" order :: Int
        candyType = fromJust $ lookup "type" order

-- Sometimes, you just gotta go for String instead of
-- Data.Text, and that's when you build your own split
-- function
splitLine lineString = foldr foldFunction [""] lineString

-- Go letter by letter,
-- concatinating it to the first word
-- in the list if it is not the ',' delimiter
-- (make this configurable later),
-- otherwise, push a new empty word onto
-- the head of the list
--
-- Also, trim (maybe move somewhere else)
foldFunction :: Char -> [String] -> [String]
foldFunction = (\nextChar (word:restOfLine) -> if nextChar == ',' then []:(trim word:restOfLine) else ((nextChar:word):restOfLine))

-- Trim whitespace and single quotes
-- Inefficient, what, with all the reversing and dropWhiling
-- This is pretty much the best/easiest way when you don't
-- want to dip into Data.Text
trim = reverse . dropWhile dropCondition . reverse . dropWhile dropCondition
  where dropCondition = (flip elem) " '"
