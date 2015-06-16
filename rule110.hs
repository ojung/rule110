module Main where

import Data.List
import System.Random

blank = ' '
symbol = '@'
width = 140
iterations = 50

xor :: Char -> Char -> Bool
a `xor` b = not (isAlive a && isAlive b) && (isAlive a || isAlive b)

notBut :: Char -> Char -> Bool
a `notBut` b = not (isAlive a) && isAlive b

isAlive :: Char -> Bool
isAlive a = a == symbol

main :: IO ()
main =
  let first = [if x then symbol else blank | x <- getRandomBools]
  in doRule110 first 0

getRandomBools :: [Bool]
getRandomBools = take (width + 2) $ randoms (mkStdGen 1) :: [Bool]

doRule110 :: [Char] -> Int -> IO ()
doRule110 list@(_:xs) count
  | count == iterations = do
    print (init xs)
  | otherwise = do
    print (init xs)
    calculateNext list count

getTransposedList :: [Char] -> [[Char]]
getTransposedList list =
  transpose [(blank:list), list, ((tail list) ++ [blank])]

calculateNext :: [Char] -> Int -> IO ()
calculateNext list count =
  let decoratedList = getTransposedList list
  in doRule110 [getCell pre x suc | [pre, x, suc] <- decoratedList] (count + 1)

getCell :: Char -> Char -> Char -> Char
getCell pre x suc = if (pre `notBut` x || x `xor` suc) then symbol else blank

