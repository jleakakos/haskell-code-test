{-# LANGUAGE OverloadedStrings #-}

module ChocolateShop where

import Data.Map (elems, foldWithKey, fromList, toList, insert, lookup, map, mapWithKey, Map)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Data.Text hiding (all, foldl, filter)

milkOrder = redeemWrappers (purchaseChocolate 12 2 Milk) 5
darkOrder = redeemWrappers (purchaseChocolate 12 4 Dark) 4
sugarFreeOrder = redeemWrappers (purchaseChocolate 6 2 SugarFree) 2
whiteOrder = redeemWrappers (purchaseChocolate 6 2 White) 2
superSugarFree = redeemWrappers (purchaseChocolate 100 2 SugarFree) 5

type Cash = Int
type Price = Int
type WrappersNeeded = Int
data ChocolateType = Milk | Dark | White | SugarFree deriving (Enum, Eq, Ord, Show)

data Order = Order Cash Price WrappersNeeded ChocolateType deriving Show

runOrder :: Order -> Text
runOrder (Order cash price wrappersNeeded chocolateType) = formattedOrder
  where output = redeemWrappers (purchaseChocolate cash price chocolateType) wrappersNeeded
        formattedOrder = intercalate ", " pre
        pre = Prelude.map (\(c,n) -> ((pack $ show c) `append` " " `append` (pack $ show n))) $ toList output

emptyBasket = fromList $ Prelude.map (\t -> (t, 0)) [Milk ..]

purchaseChocolate :: Cash -> Price -> ChocolateType -> Map ChocolateType Int
purchaseChocolate cash price chocolateType = insert chocolateType total emptyBasket
  where total = cash `div` price

redeemWrappers :: Map ChocolateType Int -> WrappersNeeded -> Map ChocolateType Int
redeemWrappers basket wrappersNeeded = basketWithRemovedWrappers
  where basketWithRemovedWrappers = Data.Map.map fst basketWithWrappers
        basketWithWrappers = redeemWrappers' (Data.Map.map (\v -> (v,v)) basket) wrappersNeeded

redeemWrappers' :: Map ChocolateType (Int, Int) -> WrappersNeeded -> Map ChocolateType (Int, Int)
redeemWrappers' basket wrappersNeeded
  | all (\(_, unredeemedWrappers) -> unredeemedWrappers < wrappersNeeded) basket = basket
  | otherwise = redeemWrappers' updatedBasket wrappersNeeded
  where updatedBasket = foldl (\b c -> updateBasket b wrappersNeeded c) basket [Milk ..]

updateBasket :: Map ChocolateType (Int, Int) -> WrappersNeeded -> ChocolateType -> Map ChocolateType (Int, Int)
updateBasket basket wrappersNeeded chocolateType 
  | chocolateType == Milk || chocolateType == White = insert SugarFree (sugarFreeTotal + redeemedChocolates, sugarFreeWrappers + redeemedChocolates) beforePromotionBasket
  | chocolateType == SugarFree = insert Dark (darkTotal + redeemedChocolates, darkWrappers + redeemedChocolates) beforePromotionBasket
  | otherwise = beforePromotionBasket
  where beforePromotionBasket = insert chocolateType (totalForType + redeemedChocolates, redeemedChocolates + unredeemedWrappers) basket
        (sugarFreeTotal, sugarFreeWrappers) = fromJust $ lookup SugarFree basket
        (darkTotal, darkWrappers) = fromJust $ lookup Dark basket
        (redeemedChocolates, unredeemedWrappers) = wrappersForType `divMod` wrappersNeeded
        (totalForType, wrappersForType) = fromJust $ lookup chocolateType basket 
