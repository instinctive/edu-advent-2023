-- https://adventofcode.com/2023/day/12
-- TODO Part 2

{-# LANGUAGE OverloadedStrings #-}

module Main where

main = tgetContents <&> map parse . tlines >>= \pp -> do
    print . sum $ solve <$> pp

parse t =
    ( filter (not.tnull) $ tsplitOn "." chart
    , tdecimal <$> tsplitOn "," runs )
  where
    [chart,runs] = twords t

hasHash = telem '#'

solve (tt,rr) =
    go tt rr
  where
    go [] [] = 1
    go tt [] = bool 1 0 $ any hasHash tt
    go [] rr = 0
    go (t:tt) (r:rr)
        | tnull t        = skip
        | tlength t <  r = if hasHash t then 0    else        skip
        | tlength t == r = if hasHash t then full else full + skip
        | otherwise = case (tindex t 0, tindex t r) of
            ('#','#') -> 0
            ('#','?') -> curr
            ('?','#') -> next
            ('?','?') -> curr + next
      where
        skip = go                  tt   (r : rr )  -- skip t entirely
        full = go                  tt        rr    -- fully consume t
        curr = go (tdrop (r+1) t : tt )      rr    -- consume front of t
        next = go (tdrop    1  t : tt ) (r : rr )  -- let the first '?' pass
