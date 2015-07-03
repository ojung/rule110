module Main where

import Data.List
import System.Environment
import System.Exit
import System.Random

blank :: Char
blank = ' '

symbol :: Char
symbol = '@'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [width, iterations] -> do
      gen <- newStdGen
      let w = read width + 2
          first = take w $ randomRs (True, False) gen
      prettyPrint $ doRule110 first [] $ read iterations
    _ -> do
      putStrLn "Error! Usage: ./rule110 <width> <iterations>"
      exitFailure

prettyPrint :: [[Bool]] -> IO ()
prettyPrint nestedList =
  mapM_ putStrLn generations
  where generations = [transform list | list <- nestedList]
        transform list = [if x then symbol else blank | x <- list]


doRule110 :: [Bool] -> [[Bool]] -> Integer -> [[Bool]]
doRule110 prev acc count
  | count == 0 = acc
  | otherwise = doRule110 next_gen (acc ++ [next_gen]) (count - 1)
    where decorated_list = transpose [False:prev, prev, tail prev ++ [False]]
          next_gen = [getCell pre x suc | [pre, x, suc] <- decorated_list]

getCell :: Bool -> Bool -> Bool -> Bool
getCell pre x suc = not pre && x || not (x && suc) && (x || suc)

