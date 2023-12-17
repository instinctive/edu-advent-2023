-- https://adventofcode.com/2023/day/14
-- TODO Part 2

module Main where

import Grid

main = tgetContents <&> mkGrid . tlines >>=
    print . solve

solve grid@Grid{..} =
    sum $ column <$> [0.._gridCols-1]
  where
    r = _gridRows
    column c =
        go 0 r xx
      where
        xx = zip [r,r-1..] $ mapMaybe (atGrid grid) $ (,c) <$> [0..r-1]
        go n w [] = n
        go n w ((i,x):ixs) = case x of
            '.' -> go  n     w    ixs
            '#' -> go  n    (i-1) ixs
            'O' -> go (n+w) (w-1) ixs
