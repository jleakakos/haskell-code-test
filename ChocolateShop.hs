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

import Data.Map (fromList, insert, lookup, Map)
import Data.Maybe (fromJust)

type Cash = Int
type Price = Int
type WrappersNeeded = Int
data ChocolateType = Milk | Dark | White | SugarFree deriving (Enum, Eq, Ord, Show)

type Basket = Map ChocolateType Int
type TotalChocoaltes = Int
type WrappedChocolate = Int
type UnredeemedWrappers = Int
type BasketWithWrappers = Map ChocolateType (TotalChocoaltes, WrappedChocolate, UnredeemedWrappers)

data Scenario = Scenario { cash :: Cash, price :: Price, wrappersNeeded :: WrappersNeeded, chocolateType :: ChocolateType } deriving Show

emptyBasket :: Basket
emptyBasket = fromList $ Prelude.map (\chocolateType -> (chocolateType, 0)) [Milk ..]

purchaseInitialChocolates :: Scenario -> Basket
purchaseInitialChocolates scenario = insert (chocolateType scenario) totalBought emptyBasket
  where totalBought = cash scenario `div` price scenario

redeemPurchasedChocolates :: Basket -> ChocolateType -> WrappersNeeded -> BasketWithWrappers
{-redeemPurchasedChocolates basket chocolateType wrappersNeeded = insert chocolateType (totalForType + (totalForType `div` wrappersNeeded)) basket-}
redeemPurchasedChocolates basket chocolateType wrappersNeeded = redeem' (basketToBasketWithWrappers basket) chocolateType wrappersNeeded
  where maybeTotalForType = Data.Map.lookup chocolateType basket
        totalForType = fromJust maybeTotalForType

basketToBasketWithWrappers :: Basket -> BasketWithWrappers
basketToBasketWithWrappers basket = fmap (\num -> (num, num, 0)) basket

redeem' :: BasketWithWrappers -> ChocolateType -> WrappersNeeded -> BasketWithWrappers
redeem' basket chocolateType wrappersNeeded
  | wrapped + wrappers < wrappersNeeded = basket
  | otherwise = redeem' updatedBasket chocolateType wrappersNeeded
  where (total, wrapped, wrappers) = fromJust $ Data.Map.lookup chocolateType basket
        (redeemed, leftOver) = (wrapped + wrappers) `divMod` wrappersNeeded
        updatedBasket = insert chocolateType (total + redeemed, redeemed, leftOver) basket
       
        
  {-where basketWithWrappers = Data.Map.map (\numberOfChcolate -> (numberOfChocolates, numberOfChocolate, 0) basket-}
