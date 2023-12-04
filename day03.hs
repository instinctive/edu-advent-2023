-- https://adventofcode.com/2023/day/3

module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

main = T.getContents <&> T.lines >>= \tt -> do
    each print $ solve tt

solve tt =
    (part1,part2)
  where
    nrows = length tt
    ncols = T.length (head tt)

    gears = runSTArray do
        ary <- newArray ((-1,-1),(nrows,ncols)) []
        let addGear r c dr dc = modifyArray ary (r+dr,c+dc) ((r,c):)
        for_ (zip [0..] tt) \(row,t) ->
            for_ [0..ncols-1] \col -> do
                let c = T.index t col
                when (c /= '.' && not (isDigit c)) do
                    sequence_ $ addGear row col <$> [-1..1] <*> [-1..1]
        pure ary

    partsAndGears =
        [ (read @Int p, nubOrd $ concat ggg)
        | (row,t) <- zip [0..] tt
        , let xx = zip (T.unpack t) $ (gears!).(row,) <$> [0..ncols-1]
        , (p,ggg) <- unzip <$> wordsBy (not.isDigit.fst) xx
        , any (not.null) ggg
        ]

    part1 = sum $ fst <$> partsAndGears

    part2
        = concatMap invert partsAndGears
        & M.fromListWith (<>)
        & M.filter twoItems
        & sum . fmap product
      where
        invert (part,gears) = (,[part]) <$> gears
        twoItems [_,_] = True
        twoItems _     = False
