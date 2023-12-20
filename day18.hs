-- https://adventofcode.com/2023/day/18

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.IntMap.Strict as M

import Grid

main = tgetContents <&> unzip . map parse . tlines >>=
    each (print . solve)

dir "U" = N
dir "L" = W
dir "D" = S
dir "R" = E

parse (twords -> [d,n,c]) =
    (part1,part2)
  where
    part1 = ( tdecimal n, dir d )
    part2 = ( hex, dir $ tpack [x] )
    x = "RDLU" !! (digitToInt $ tindex c 7)
    hex = foldl' f 0 [2..6] where
        f z i = z * 16 + digitToInt (tindex c i)

solve pp =
    sum $ count <$> rows
  where
    path = concatMap (uncurry replicate) pp
    ptypes = map ptype $ zip path (tail $ cycle path)
    locs = tail $ scanl move (0,0) path
    rows = map sort . M.elems $ foldl' f M.empty $ zip locs ptypes where
        f m ((r,c),p) = m & at r . non [] %~ ((c,p):)

data PType = Vert | Horz | Exit NWSE | Enter NWSE deriving (Eq,Ord,Show)

ptype (N,N) = Vert
ptype (S,S) = Vert
ptype (W,W) = Horz
ptype (E,E) = Horz
ptype (E,d) = Exit d
ptype (d,W) = Exit d
ptype (d,E) = Enter d
ptype (W,d) = Enter d

count [] = 0
count ((c,p):more) =
    case p of
        Vert    -> inside more
        Enter d -> border d False more
  where
    inside [] = error "ended while empty"
    inside ((c',Vert):more)            = c' - c + 1 + count more
    inside ((c',Enter d):more)         = border d True more
    inside x = error $ "inside: " <> show x
    border d isInside ((c',Horz):more) = border d isInside more
    border d isInside ((c',Exit d'):more)
        | (d == d') == isInside =
            c' - c + 1 + count more
        | otherwise =
            inside more
