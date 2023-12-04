-- https://adventofcode.com/2023/day/4

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set     as S
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main = T.getContents <&> map parse . T.lines >>= \mm -> do
    print $ part1 mm
    print $ part2 mm

parse t =
    S.size $ S.intersection wset nset
  where
    (ww,nn) = bimap (drop 2) tail . break (=="|") $ T.words t
    wset = S.fromList ww
    nset = S.fromList nn

part1 = sum . map power where
    power 0 = 0
    power m = 2^(m-1)

part2 = sum . cata alg where
    alg Nil = []
    alg (Cons m nn) = 1 + sum (take m nn) : nn
