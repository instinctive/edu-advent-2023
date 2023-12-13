-- https://adventofcode.com/2023/day/13
-- TODO Part 2

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main = tgetContents <&> splitOn [""] . tlines >>= \pp -> do
    print . sum $ solve <$> pp

solve tt =
    100 * count (horz tt) + count (vert tt)
  where
    mk x '.' = 2*x
    mk x '#' = 2*x + 1
    horz  = map (T.foldl' mk 0)
    vert tt =
        [ foldl' mk 0 $ (`tindex` i) <$> tt
        | i <- [0..tlength (head tt) - 1] ]

count xx =
    find match (zip aa bb)
    & maybe 0 (length.snd)
  where
    aa = tail $ tails xx
    bb = tail $ reverse . tail . tails $ reverse xx
    match (aa,bb) = and $ zipWith (==) aa bb
