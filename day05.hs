-- https://adventofcode.com/2023/day/5

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.IntMap.Strict as IM

main = tgetContents <&> parse . tlines >>= \almanac -> do
    print $ part1 almanac
    print $ part2 almanac

parse tt =
    (seeds, maps)
  where
    [sraw]:mraw = splitOn [""] tt
    seeds = tdecimal <$> tail (twords sraw)
    maps = mkMap . tail <$> mraw
    mkMap = IM.fromList . map mkItem
    mkItem = mk . map tdecimal . twords where
        mk [dst,src,rng] = (src,(dst,rng))

convert x m =
    case IM.lookupLE x m of
        Nothing -> x
        Just (src,(dst,rng))
            | src + rng <= x -> x
            | otherwise -> dst + x - src

part1 (ss,mm) = minimum $ ss <&> \s ->
    foldl' convert s mm

part2 (ss,mm) = minimum $ chunksOf 2 ss <&> \[s,n] ->
    minimum $ [s..s+n-1] <&> \s ->
    foldl' convert s mm
