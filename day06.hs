-- https://adventofcode.com/2023/day/6

module Main where

main = tgetContents <&> map (tail.twords) . tlines >>= \pp -> do
    print $ part1 pp
    print $ part2 pp

part1 [tt,dd] = fmap product . sequence $
    zipWith solve (tdecimal <$> tt) (tdecimal <$> dd)

part2 [tt,dd] = solve (tdecimal $ mconcat tt) (tdecimal $ mconcat dd)

solve t d = find pred [1..half] <&> answer where
    half = div (t+1) 2
    pred i = i * (t-i) > d
    answer i = t - 2*i + 1
