module Main where

import Data.List
import System.Random

blank = ' '
symbol = '@'
width = 140
iterations = 50

main :: IO ()
main =
  prettyPrint $ doRule110 first [] 0
  where first = [if x then symbol else blank | x <- getRandomBools]

prettyPrint :: [String] -> IO ()
prettyPrint [x] = putStrLn x
prettyPrint (x:xs) = do
  putStrLn x
  prettyPrint xs

getRandomBools :: [Bool]
getRandomBools = take (width + 2) $ randoms (mkStdGen 1) :: [Bool]

doRule110 :: String -> [String] -> Integer -> [String]
doRule110 prev@(_:xs) acc count
  | count == iterations = acc
  | otherwise =
    let next = [getCell pre x suc | [pre, x, suc] <- decoratedList]
    in doRule110 next (acc ++ [next]) (count + 1)
  where decoratedList = transpose [blank:prev, prev, tail prev ++ [blank]]

getCell :: Char -> Char -> Char -> Char
getCell pre x suc = if pre `notBut` x || x `xor` suc then symbol else blank

xor :: Char -> Char -> Bool
a `xor` b = not (isAlive a && isAlive b) && (isAlive a || isAlive b)

notBut :: Char -> Char -> Bool
a `notBut` b = not (isAlive a) && isAlive b

isAlive :: Char -> Bool
isAlive a = a == symbol

