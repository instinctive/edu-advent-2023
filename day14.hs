-- https://adventofcode.com/2023/day/14
-- TODO Part 2

module Main where

main = tgetContents <&> parse . tlines >>=
    print . solve

data Ary = Ary
    { _nrows :: Int
    , _ncols :: Int
    , _getraw :: Int -> Int -> Char
    }

parse tt =
    Ary nrows ncols getraw
  where
    nrows = length tt
    ncols = tlength (head tt)
    rows = [0..nrows-1]
    cols = [0..ncols-1]
    ary = listArray (0,nrows-1) tt :: Array Int Text
    getraw r c = tindex (ary ! r) c

solve Ary{..} =
    sum $ column <$> [0.._ncols-1]
  where
    column c =
        go 0 _nrows xx
      where
        xx = zip [_nrows,_nrows-1..] $ flip _getraw c <$> [0.._nrows-1]
        go n w [] = n
        go n w ((i,x):ixs) = case x of
            '.' -> go  n     w    ixs
            '#' -> go  n    (i-1) ixs
            'O' -> go (n+w) (w-1) ixs
