-- https://adventofcode.com/2023/day/3

module Main where

import Data.Array.IArray ((!))
import Data.Array.ST
import Data.List.Split ( wordsBy )
import Data.Map.Strict ( Map )
import Data.Text       ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main = T.getContents <&> T.lines >>= \tt -> do
    print $ part1 tt

part1 tt = sum
    [ read @Int w
    | (row,t) <- zip [0..] tt
    , let xx = zip (T.unpack t) (partRow row)
    , (w,bb) <- unzip <$> wordsBy (not.isDigit.fst) xx
    , or bb
    ]
  where
    nrows = length tt
    ncols = T.length (head tt)
    parts = runSTUArray do
        ary <- newArray ((-1,-1),(nrows,ncols)) False
        let setbit r c dr dc = writeArray ary (r+dr,c+dc) True
        for_ (zip [0..] tt) \(row,t) ->
            for_ [0..ncols-1] \col -> do
                let c = T.index t col
                when (c /= '.' && not (isDigit c)) do
                    sequence_ $ setbit row col <$> [-1..1] <*> [-1..1]
        pure ary
    partRow r = (parts!).(r,) <$> [0..ncols-1]
