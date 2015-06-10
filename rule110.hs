module Main where

import Data.List

blank = ' '
symbol = '~'
width = 110

a `xor` b = not (isAlive a && isAlive b) && (isAlive a || isAlive b)

a `notBut` b = not (isAlive a) && isAlive b

isAlive a = a == symbol

main =
  let first = [if (x == width) then symbol else blank | x <- [1 .. width]]
  in doRule110 first

doRule110 list = do
  print list
  calculateNext listWithPreAndSuc
  where listWithPreAndSuc = [x | x <- getTransposedList list]

getTransposedList list =
  transpose [(blank:list), list, ((tail list) ++ [blank])]

calculateNext decoratedList =
  doRule110 [if ( pre `notBut` x || x `xor` suc) then symbol else blank | [pre, x, suc] <- decoratedList]
