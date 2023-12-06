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
        mk [dst,src,rng] = (src,(rng,dst-src))

part1 (ss,mm) =
    minimum $ ss <&> \s -> foldl' convert s mm
  where
    convert x m = IM.lookupLE x m & maybe x (morph x)
    morph x (src,(rng,delta))
        | src + rng <= x = x
        | otherwise      = x + delta

part2 (ss,mm) =
    fst . head $
    foldl' almanac seeds mm
  where
    seeds = cleanup $ unfoldr f ss where
        f [] = Nothing
        f (s:n:more) = Just ((s,n),more)
    almanac s m = cleanup $ go s (IM.assocs m)

    go [] _ = []
    go sss [] = sss
    go ((_,0):ss) aaa     = go ss aaa
    go sss ((_,(0,_)):aa) = go sss aa
    go sss@((s,sn):ss) aaa@((a,(an,delta)):aa)
        | s + sn <= a =           (s,sn) : go ss aaa
        | a + an <= s =                    go sss aa
        | s < a = let q = a - s in (s,q) : go ((s,sn-q):ss) aaa
        | a < s = let q = s - a in         go sss ((a+q,(an-q,delta)):aa)
        | otherwise =
            let n = min sn an in
            (s + delta, n) : go ((s+n,sn-n):ss) ((a+n,(an-n,delta)):aa)

cleanup = go . sort where
    go ((_,0):more)                    = go more
    go ((a,m):(b,n):more) | a + m >= b = go ((a, max (a+m) (b+n) - a):more)
    go (x:more)                        = x : go more
    go []                              = []
