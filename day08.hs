-- https://adventofcode.com/2023/day/8

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M

main = tgetContents <&> parse . tlines >>= \(instr,nodes) -> do
    let next i x = fromJust (M.lookup x nodes) & \(l,r) ->
            case tindex instr (i `mod` tlength instr) of
                'L' -> l
                'R' -> r
    let anodes = filter ((=='A').tlast) (M.keys nodes)
    print $ part1 next "AAA"
    print $ part2 next anodes

parse (inst:_:nodes) =
    (inst, M.fromList $ mk <$> nodes)
  where
    mk = mkItem . twords . tmap tr
    mkItem [a,b,c] = (a,(b,c))
    tr c = if isAlphaNum c then c else ' '

part1 next = go 0 where
    go i "ZZZ" = Just i
    go i x = go (i+1) (next i x)

part2 next = go 0 where
    go i zz | all ((=='Z').tlast) zz = Just i
    go i zz = go (i+1) $ map (next i) zz
