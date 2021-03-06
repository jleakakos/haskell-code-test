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

import Data.Map (elems, fromList, insert, lookup, Map)
import Data.Maybe (fromJust)

type Cash = Int
type Price = Int
type WrappersNeeded = Int
data ChocolateType = Milk | Dark | White | SugarFree deriving (Enum, Eq, Ord, Show)

type Basket = Map ChocolateType Int
type TotalChocolates = Int
type WrappedChocolate = Int
type UnredeemedWrappers = Int
type BasketWithWrappers = Map ChocolateType (TotalChocolates, UnredeemedWrappers)

data Scenario = Scenario { cash :: Cash, price :: Price, wrappersNeeded :: WrappersNeeded, chocolateType :: ChocolateType } deriving Show

emptyBasket = fromList $ Prelude.map (\chocolateType -> (chocolateType, 0)) [Milk ..]
emptyBasketWithWrappers = basketToBasketWithWrappers emptyBasket

purchaseInitialChocolates :: Scenario -> Basket
purchaseInitialChocolates scenario = insert (chocolateType scenario) totalBought emptyBasket
  where totalBought = cash scenario `div` price scenario

redeemPurchasedChocolates :: Basket -> WrappersNeeded -> BasketWithWrappers
redeemPurchasedChocolates basket wrappersNeeded = redeemForAll (basketToBasketWithWrappers basket) wrappersNeeded

basketToBasketWithWrappers :: Basket -> BasketWithWrappers
basketToBasketWithWrappers basket = fmap (\num -> (num, num)) basket

{-redeem' :: BasketWithWrappers -> ChocolateType -> WrappersNeeded -> BasketWithWrappers-}
{-redeem' basket chocolateType wrappersNeeded-}
  {-| wrappers < wrappersNeeded = basket-}
  {-| otherwise = redeem' updatedBasket chocolateType wrappersNeeded-}
  {-where (total, wrappers) = fromJust $ Data.Map.lookup chocolateType basket-}
        {-(redeemed, leftOver) = wrappers `divMod` wrappersNeeded-}
        {-updatedBasket = updateBasketWithPromotion basket chocolateType total redeemed (redeemed + leftOver)-}

redeemForAll :: BasketWithWrappers -> WrappersNeeded -> BasketWithWrappers
redeemForAll basket wrappersNeeded
  | all (\(total,wrappers) -> total + wrappers < wrappersNeeded) basket = basket
  | otherwise = redeemForAll updatedBasket wrappersNeeded
  where listOfTotalsAndWrappers = elems basket
        listOfRedeemedAndLeftOver = map (\(_, wrappers) -> wrappers `divMod` wrappersNeeded) listOfTotalsAndWrappers
        updatedBaskets = map (\(chocolateType,(redeemed, leftOver)) -> updateBasketWithPromotion basket chocolateType redeemed (redeemed + leftOver)) $ zip [Milk ..] listOfRedeemedAndLeftOver
        updatedBasket = mconcat updatedBaskets

redeemForAll = undefined

{-type OriginalTotal = Int-}
{-type ChocolatesRedeemed = Int-}
{-updateBasketWithPromotion :: BasketWithWrappers -> ChocolateType -> ChocolatesRedeemed -> UnredeemedWrappers -> BasketWithWrappers-}
{-updateBasketWithPromotion basket chocolateType redeemed unredeemed-}
  {-| chocolateType == Milk || chocolateType == White = insert SugarFree (sugarFreeTotal + redeemed, sugarFreeWrappers + redeemed) updatedBasketWithoutPromotion-}
  {-| chocolateType == SugarFree = insert Dark (darkTotal + redeemed, darkWrappers + redeemed) updatedBasketWithoutPromotion-}
  {-| otherwise = updatedBasketWithoutPromotion-}
  {-where (darkTotal, darkWrappers) = fromJust $ Data.Map.lookup Dark basket-}
        {-(sugarFreeTotal, sugarFreeWrappers) = fromJust $ Data.Map.lookup SugarFree basket-}
        {-(currentTotal, currentWrappers) = fromJust $ Data.Map.lookup chocolateType basket-}
        {-updatedBasketWithoutPromotion = insert chocolateType (currentTotal+redeemed, unredeemed) basket-}
