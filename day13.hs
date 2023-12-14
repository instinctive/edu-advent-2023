-- https://adventofcode.com/2023/day/13

{-# LANGUAGE OverloadedStrings #-}

module Main where

main = tgetContents <&> splitOn [""] . tlines >>= \pp -> do
    each print . map sum . transpose $ solve <$> pp

solve tt =
    [sum part1, sum part2]
  where
    ans get =
        ( (100 *) <$> count (horz get) )
        <> count (vert get)
    nrows = length tt
    ncols = tlength (head tt)
    rows = [0..nrows-1]
    cols = [0..ncols-1]
    ary = listArray (0,nrows-1) tt :: Array Int Text
    getraw r c = tindex (ary ! r) c & \case '.' -> 0; '#' -> 1
    vert get = mk (flip get) cols rows
    horz get = mk get rows cols
    mk get rows cols =
        [ foldl' f 0 $ get r <$> cols
        | r <- rows ]
      where f x q = 2*x + q
    smudge r' c' = \r c ->
        if (r,c) == (r',c')
        then 1 - getraw r c
        else getraw r c
    part1 = ans getraw
    part2 = fromJust $ find (not.null)
        [ ans (smudge r c) \\ part1 | r <- rows , c <- cols ]

count xx =
    filter match (zip aa bb) <&> length.snd & filter (/=0)
  where
    aa = tail $ tails xx
    bb = tail $ reverse . tail . tails $ reverse xx
    match (aa,bb) = and $ zipWith (==) aa bb
