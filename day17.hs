-- https://adventofcode.com/2023/day/17

module Main where

import Grid

main = tgetContents <&> mkGrid . tlines >>=
    print . solve

solve _ = 0
