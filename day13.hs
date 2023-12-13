-- https://adventofcode.com/2023/day/13
-- TODO Part 2

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main = tgetContents <&> splitOn [""] . tlines >>= \pp -> do
    print . sum $ solve <$> pp

solve tt =
    100 * count (horz get) + count (vert get)
  where
    nrows = length tt
    ncols = tlength (head tt)
    rows = [0..nrows-1]
    cols = [0..ncols-1]
    ary = listArray (0,nrows-1) tt :: Array Int Text
    get r c = tindex (ary ! r) c & \case '.' -> 0; '#' -> 1
    vert get = mk (flip get) cols rows
    horz get = mk get rows cols
    mk get rows cols =
        [ foldl' f 0 $ get r <$> cols
        | r <- rows ]
      where f x q = 2*x + q

count xx =
    find match (zip aa bb)
    & maybe 0 (length.snd)
  where
    aa = tail $ tails xx
    bb = tail $ reverse . tail . tails $ reverse xx
    match (aa,bb) = and $ zipWith (==) aa bb
