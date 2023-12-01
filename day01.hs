-- https://adventofcode.com/2023/day/1

module Main where

main = getContents >>= \s -> do
    print $ solve part1 s
    print $ solve part2 s

solve f = sum . map (line f) . lines

line f s = 10 * head ints + last ints
  where ints = mapMaybe f (tails s)

part1 (c:_) | isDigit c = Just $ digitToInt c
part1 _ = Nothing

part2 s = part1 s <|> asum (match <$> numbers) where
    match (n,w) | isPrefixOf w s = Just n
    match _ = Nothing

numbers = zip [1..] $ words "one two three four five six seven eight nine"
