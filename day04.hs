-- https://adventofcode.com/2023/day/4

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Set     as S
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main = T.getContents <&> map parse . T.lines >>= \tt -> do
    print $ solve tt

parse = bimap (drop 2) tail . break (=="|") . T.words

solve pp = sum $ points <$> pp

points (ww,nn) =
    if nmatch == 0 then 0 else 2^(nmatch-1)
  where
    nmatch = S.size $ S.intersection wset nset
    wset = S.fromList ww
    nset = S.fromList nn
