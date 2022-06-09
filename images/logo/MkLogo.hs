#!/usr/bin/env stack
-- stack --resolver lts-19.3 script

import System.Random

main = do
  txt <- lines <$> readFile "title.txt"
  txt' <- (traverse . traverse) replace txt
  writeFile "logo.txt" (unlines txt')

chars = "<^>vT~@░ "

replace :: Char -> IO Char
replace ' ' = pick $ zip (replicate 8 0.005 ++ [1]) chars
replace _ = pick $ zip (replicate 4 0.2 ++ replicate 4 0.04 ++ [1]) chars

pick :: [(Double, a)] -> IO a
pick es = do
  r <- randomRIO (0 :: Double, 1)
  return $ go r es
 where
  go _ [(_, a)] = a
  go r ((p, a) : es)
    | r < p = a
    | otherwise = go (r - p) es
