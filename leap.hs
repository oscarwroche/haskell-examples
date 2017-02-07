leap ::  Int -> String
leap x
	| x `mod` 400 = 0 = "This is a leap year."
	| x `mod` 100 = 0 = "This isn't a leap year." 
	| x `mod` 4 = 0 = "This is a leap year."
	| otherwise = "This isn't a leap year."