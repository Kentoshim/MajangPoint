module Calculation where


main :: IO ()
main = do
  [_, x] <- map read . words <$> getLine
  js <- getLine
  print $ calc x js

calc ::Int -> [Char] -> Int
calc current [] = current
calc current ('o':js) = calc (current + 1) js
calc 0 (_:js) = calc 0 js
calc current ('x':js) = calc (current - 1) js