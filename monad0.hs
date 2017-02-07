import Control.Monad.State

addStuff :: Int -> Int
addStuff = do
	a <- (*2)
	b <- (+10)
	return (a+b)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int,Stack)
stackManip stack = let
	((),newStack1) = push 3 stack
	(a ,newStack2) = pop newStack1
	in pop newStack2

