module Main where

import Data.List

blank = ' '
symbol = '@'
width = 270

xor :: Char -> Char -> Bool
a `xor` b = not (isAlive a && isAlive b) && (isAlive a || isAlive b)

notBut :: Char -> Char -> Bool
a `notBut` b = not (isAlive a) && isAlive b

isAlive :: Char -> Bool
isAlive a = a == symbol

main :: IO ()
main =
  let first = [if (x == width) then symbol else blank | x <- [1 .. width]]
  in doRule110 first

doRule110 :: [Char] -> IO ()
doRule110 list
  | first == symbol = do
    print list
  | otherwise = do 
    print list
    calculateNext list
  where first:_ = list


getTransposedList :: [Char] -> [[Char]]
getTransposedList list =
  transpose [(blank:list), list, ((tail list) ++ [blank])]

calculateNext :: [Char] -> IO ()
calculateNext list =
  let decoratedList = getTransposedList list
  in doRule110 [getCell pre x suc | [pre, x, suc] <- decoratedList]

getCell :: Char -> Char -> Char -> Char
getCell pre x suc = if (pre `notBut` x || x `xor` suc) then symbol else blank
