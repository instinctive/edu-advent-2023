-- https://adventofcode.com/2023/day/11

module Main where

import qualified Data.Set as S

main = tgetContents <&> parse >>=
    each print . solve [2,1000000]

parse raw =
    [ V2 r c
    | (r,t) <- zip [0..] $ tlines raw
    , (c,x) <- zip [0..] $ tunpack t
    , x == '#' ]

solve mm gg =
    ans <$> mm
  where
    ans m = mhd + gap * (m-1)
    [ mhd, gap ] = map sum $ transpose
        [ dist a b | a:bb <- tails gg, b <- bb ]
    dist ab@(V2 a b) uv@(V2 u v) = [mhd,gap]
      where
        mhd = sum . abs $ ab - uv
        gap = gaps rows a u + gaps cols b v
        gaps xx a b | a > b = gaps xx b a
        gaps xx a b = b - a - used xx where
            used = length . takeWhile (<b) . dropWhile (<a)
    rows = view _x <$> gg & S.toList . S.fromList
    cols = view _y <$> gg & S.toList . S.fromList
