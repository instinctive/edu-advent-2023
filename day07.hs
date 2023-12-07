-- https://adventofcode.com/2023/day/7

module Main where

main = tgetContents <&> map parse . tlines >>= \pp -> do
    print $ solve $ map part1 pp
    print $ solve $ map part2 pp

solve = sum . zipWith f [1..] . sort where
    f rank (_,_,hb) = rank * hb

parse (twords -> [cards,bid]) = (tmap tr cards, tdecimal bid)

part1 (hc, hb) = (mk hc, hc, hb) where
    mk = mkType . mkRuns . tunpack

part2 (hc, hb) = (mk hc, tmap joker hc, hb) where
    mk = mkType . addjokers . mkRuns . nojokers . tunpack
    nojokers = filter (/= tr 'J')
    addjokers [] = [5]
    addjokers nnn@(n:nn) = n + 5 - sum nnn : nn
    joker c = if c == tr 'J' then '0' else c

mkRuns = sortBy (comparing Down) . map length . group . sort

mkType = tpack . map (chr.(+ ord '0'))

tr 'T' = 'A'
tr 'J' = 'B'
tr 'Q' = 'C'
tr 'K' = 'D'
tr 'A' = 'E'
tr c = c
