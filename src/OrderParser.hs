module OrderParser
( parseInputFile
, Order
) where

import Data.Maybe (fromJust)

type Cash = Int
type Price = Int
type WrappersNeeded = Int
type ChocolateType = String
data Order = Order Cash Price WrappersNeeded ChocolateType deriving Show

-- I chose to use the "map-like" functions that Prelude provides
-- instead of going with an actual map
-- e.g. Prelude.lookup  :: Eq a => a -> [(a,b) -> Maybe b]
--      Data.Map.lookup :: Ord k => k -> Map k a -> Maybe a

-- This is the main function that will be used
-- Read from the input file
-- Oh, and it can deal with the header being in any order
parseInputFile :: FilePath -> IO [Order]
parseInputFile filePath = do
  content <- readFile filePath
  return $ parseInputContent content

-- Get the header, skip the newline, get the rest
parseInputContent :: String -> [Order]
parseInputContent content = orders
  where orders = map (parseInputLine header) orderLines
        (header:_:orderLines) = lines content

-- Split the lines and zip those babies up (into a map-like structure)
parseInputLine :: String -> String -> Order
parseInputLine headerLine orderLine = order
  where order = clean $ zip splitHeaderValues splitOrderValues
        splitHeaderValues = splitLine headerLine
        splitOrderValues = splitLine orderLine

-- Turn a line from the input file into an Order
-- This is where some hard-coding lives, since the header
-- fields aren't able to be encoded statically
-- Deal with it
clean :: [(ChocolateType, String)] -> Order
clean order = Order cash price wrappersNeeded candyType
  where cash = read $ fromJust $ lookup "cash" order :: Int
        price = read $ fromJust $ lookup "price" order :: Int
        wrappersNeeded = read $ fromJust $ lookup "wrappers needed" order :: Int
        candyType = fromJust $ lookup "type" order

-- Sometimes, you just gotta go for String instead of
-- Data.Text, and that's when you build your own split
-- function
splitLine :: String -> [String]
splitLine lineString = foldr splitLineFoldFunction [""] lineString

-- Go letter by letter,
-- concatinating it to the first word
-- in the list if it is not the ',' delimiter
-- (make this configurable later),
-- otherwise, push a new empty word onto
-- the head of the list
--
-- Also, trim (maybe move somewhere else)
splitLineFoldFunction :: Char -> [String] -> [String]
splitLineFoldFunction = (\nextChar (word:restOfLine) -> if nextChar == ',' then []:(trim word:restOfLine) else ((nextChar:word):restOfLine))

-- Trim whitespace and single quotes
-- Inefficient, what, with all the reversing and dropWhiling
-- This is pretty much the best/easiest way when you don't
-- want to dip into Data.Text
trim :: String -> String
trim = reverse . dropWhile dropCondition . reverse . dropWhile dropCondition
  where dropCondition = (flip elem) " '"

run = do
  content <- readFile "input.txt"
  mapM_ print $ parseInputContent content

run2 = do
  content <- readFile "input2.txt"
  mapM_ print $ parseInputContent content
