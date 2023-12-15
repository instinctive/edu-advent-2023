-- https://adventofcode.com/2023/day/15

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main = tgetContents <&> tlines >>= \[t] ->
    print . sum $ xhash <$> tsplitOn "," t

xhash = T.foldl' f 0 where
    f z c = 17 * (z + ord c) .&. 0xff
