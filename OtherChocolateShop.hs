-- Little Bob loves chocolates, and goes to the store with cash in his pocket. The price of each chocolate is price. The store offers a discount: for every 'wrappers needed' number of wrappers he gives the store, he’ll get one chocolate for free.
-- 
-- The free chocolate program has been successful, and they've decided to tweak the promotion.
-- 
-- Chocolate Types
-- 
-- The store is now pushing certain types of chocolates. There are four types, white, dark, milk and sugar free and the store is giving away an extra sample of chocolates in addition to the original wrapper promotion. For the original wrapper promotion, the free chocolates will be of the same type that you are buying for that purchase. Note that if Bob accumulates enough wrappers of the other type, he can trade those in as well.
-- 
-- If you trade in wrappers for milk or white, you get an extra sugar free chocolate along with every free milk or white chocolate that you would normally get.
-- If you trade in wrappers for sugar free chocolate, you get an extra dark chocolate along with every free sugar free chocolate that you get.
-- Since dark is all the rage, that is considered premium and there is no additional candy bonus.

-- cash, price, wrappers needed, type
-- 
-- 12, 2, 5, 'milk'
-- 12, 4, 4, 'dark'
-- 6, 2, 2, 'sugar free'
-- 6, 2, 2, 'white'o
-- 
-- milk 7, dark 0, white 0, sugar free 1
-- milk 0, dark 3, white 0, sugar free 0
-- milk 0, dark 3, white 0, sugar free 5
-- milk 0, dark 1, white 5, sugar free 3

import Data.Map (elems, fromList, insert, lookup, map, Map)
import Prelude hiding (lookup)
import Data.Maybe (fromJust)
import Debug.Trace

milkOrder = redeemWrappers (purchaseChocolate 12 2 Milk) 5
darkOrder = redeemWrappers (purchaseChocolate 12 4 Dark) 4
sugarFreeOrder = redeemWrappers (purchaseChocolate 6 2 SugarFree) 2
whiteOrder = redeemWrappers (purchaseChocolate 6 2 White) 2

type Cash = Int
type Price = Int
type WrappersNeeded = Int
data ChocolateType = Milk | Dark | White | SugarFree deriving (Enum, Eq, Ord, Show)

data Order = Order Cash Price WrappersNeeded ChocolateType deriving Show

emptyBasket = fromList $ Prelude.map (\t -> (t, 0)) [Milk ..]
emptyBasketWithWrappers = fromList $ Prelude.map (\t -> (t, (0,0))) [Milk ..]

purchaseChocolate :: Cash -> Price -> ChocolateType -> Map ChocolateType Int
purchaseChocolate cash price chocolateType = insert chocolateType total emptyBasket
  where total = cash `div` price

redeemWrappers :: Map ChocolateType Int -> WrappersNeeded -> Map ChocolateType (Int, Int)
redeemWrappers basket wrappersNeeded = redeemWrappers' (Data.Map.map (\v -> (v,v)) basket) wrappersNeeded

redeemWrappers' :: Map ChocolateType (Int, Int) -> WrappersNeeded -> Map ChocolateType (Int, Int)
redeemWrappers' basket wrappersNeeded
  | all (\(_, unredeemedWrappers) -> unredeemedWrappers < wrappersNeeded) basket = basket
  | otherwise = redeemWrappers' updatedBasket wrappersNeeded
  where updatedMilkBasket = updateBasket basket wrappersNeeded Milk
        updatedDarkBasket = updateBasket updatedMilkBasket wrappersNeeded Dark
        updatedSugarFreeBasket = updateBasket updatedDarkBasket wrappersNeeded SugarFree
        updatedWhiteBasket = updateBasket updatedSugarFreeBasket wrappersNeeded White
        updatedBasket = updatedWhiteBasket

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
