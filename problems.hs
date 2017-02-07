myLast :: [a] -> a
myLast x = foldl (\x y -> y ) (head x) x 

myLast' :: [a] -> a
myLast' = foldl1 (\_ x -> x)

myLast'' :: [a] -> a
myLast'' = foldr1 (\_ y -> y)

myLast''' :: [a] -> a
myLast''' = foldr1 (flip const) 

myButLast :: [a] -> a
myButLast [a] = error "error"
myButLast [a,_] = a
myButLast (_:xs) = myButLast xs 

elementAt :: [a] -> Int -> a
elementAt [] n = error "liste pas assez longue"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1) 

myLength :: [a] -> Int
myLength = foldl ( \acc  x -> acc + 1 ) 0

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc ) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = foldl (\acc (a,b) -> (a==b) && acc ) True (zip x (myReverse x)) 

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List(xs)) 
flatten (List []) = [] 


