newtype CharList = CharList { getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair { getPairs :: (a,b) } deriving (Eq, Show)

instance Functor (Pair c) where
	fmap f (Pair (x,y)) = Pair (f x ,y)

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

type IntList = [Int]

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int 
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right) 
	|abs ((left + n) - right) < 4 = Just (left+n, right)
	|otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right) 
	|abs (left - (right + n)) < 4 = Just (left, right+n)
	|otherwise = Nothing

routine :: Maybe Pole
routine = do
	start <- return (0,0)
	first <- landLeft 2 start
	Nothing
	second <- landRight 2 first
	landLeft 1 second

magicarpe :: Maybe Char
magicarpe = do
	(x:xs) <- Just ""
	return x

sevens :: [Int]
sevens = do
	x <- [1..50]
	y <- guard ('7' `elem` show x)
	return y