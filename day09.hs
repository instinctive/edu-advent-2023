-- https://adventofcode.com/2023/day/9

module Main where

main = tgetContents <&> parse >>= \xxx -> do
    print $ solve last (+) xxx
    print $ solve head (-) xxx

parse = map (map tsigned . twords) . tlines

solve acc op = sum . map go where
    go xx | all (==0) xx = 0
    go xx = op (acc xx) (go diff) where
        diff = zipWith (-) (tail xx) xx
