{-# LANGUAGE OverloadedStrings #-}

module OrderProcessor where

import ChocolateShop
import Prelude hiding (appendFile, readFile, writeFile, lines, init, tail)
import Data.Text hiding (filter)
import Data.Map (toList)
import Data.Text.IO

readInputAndRun :: IO ()
readInputAndRun = do
  content <- readFile "input.txt"
  let (header:orderLines) = filter (/= "") $ lines content
  let orders = Prelude.map lineToOrder orderLines
  let orderOutputs = Prelude.map runOrder orders
  mapM_ print orderOutputs
  writeFile "myoutput.txt" ""
  mapM_ (\t -> appendFile "myoutput.txt" (t `append` "\n")) orderOutputs

runOrder :: Order -> Text
runOrder (Order cash price wrappersNeeded chocolateType) = formattedOrder
  where output = redeemWrappers (purchaseChocolate cash price chocolateType) wrappersNeeded
        formattedOrder = intercalate ", " pre
        pre = Prelude.map (\(c,n) -> ((pack $ show c) `append` " " `append` (pack $ show n))) $ toList output

lineToOrder :: Text -> Order
lineToOrder line = Order cash price wrappersNeeded chocolateType
  where (ca:pr:wr:ch:[]) = Prelude.map strip $ splitOn "," line
        cash = read (unpack ca)
        price = read (unpack pr)
        wrappersNeeded = read (unpack wr)
        chocolateType = stringToChocolateType ch

-- TODO: The string format includes single quotes ('white')
--       so just hard-code strip them for now
stringToChocolateType :: Text -> ChocolateType
stringToChocolateType chocolateTypeString
  | cleanedChocolateTypeString == "milk" = Milk
  | cleanedChocolateTypeString == "dark" = Dark
  | cleanedChocolateTypeString == "white" = White
  | cleanedChocolateTypeString == "sugar free" = SugarFree
  where cleanedChocolateTypeString = (init . tail) chocolateTypeString
