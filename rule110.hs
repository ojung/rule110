module Main where

import Data.List
import System.Random

blank :: Char
blank = ' '

symbol :: Char
symbol = '@'

width :: Int
width = 140

iterations :: Integer
iterations = 500

main :: IO ()
main = do
  gen <- newStdGen
  let random_bools = take (width + 2) $ randomRs (True, False) gen :: [Bool]
      first = [if x then symbol else blank | x <- random_bools]
  prettyPrint $ doRule110 first [] 0

prettyPrint :: [String] -> IO ()
prettyPrint [] = return ()
prettyPrint [x] = putStrLn x
prettyPrint (x:xs) = do
  putStrLn x
  prettyPrint xs

doRule110 :: String -> [String] -> Integer -> [String]
doRule110 prev acc count
  | count == iterations = acc
  | otherwise = doRule110 next_gen (acc ++ [next_gen]) (count + 1)
    where decorated_list = transpose [blank:prev, prev, tail prev ++ [blank]]
          next_gen = [getCell pre x suc | [pre, x, suc] <- decorated_list]

getCell :: Char -> Char -> Char -> Char
getCell pre x suc = if pre `notBut` x || x `xor` suc then symbol else blank

xor :: Char -> Char -> Bool
a `xor` b = not (isAlive a && isAlive b) && (isAlive a || isAlive b)

notBut :: Char -> Char -> Bool
a `notBut` b = not (isAlive a) && isAlive b

isAlive :: Char -> Bool
isAlive a = a == symbol

