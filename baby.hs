doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
parite xs = [if (x `mod` 2 == 1) then "odd" else "pair" | x<-xs]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 
length' xs = sum [1 | _ <- xs]
removeNonUpper :: [Char] -> [Char]
removeNonUpper xs = [ c | c <- xs, c `elem` ['A'..'Z'] ]  
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
lucky :: (Integral a) => a -> String
lucky 7 = "bien ouej"
lucky x = "loupe"
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = factorial(n-1) * n
capital :: String -> String
capital "" = "Y'en a pas"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
max' :: (Ord a)=> a -> a -> a
max' a b
	| a > b = a
	| otherwise  = b

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

