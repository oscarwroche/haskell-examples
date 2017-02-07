import Data.Monoid
import Control.Monad.Trans.Writer

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Dat gang shit vs 9 fool")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y, log `mappend` newLog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got Number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
	a <- logNumber 3
	tell ["tg"]
	b <- logNumber 5
	return (a*b)



