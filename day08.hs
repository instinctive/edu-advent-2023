-- https://adventofcode.com/2023/day/8

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M

main = tgetContents <&> parse . tlines >>= \(inst,nodes) -> do
    let step x i = nodes M.! x & \(l,r) ->
            case tindex inst (i `mod` tlength inst) of
                'L' -> l
                'R' -> r
    let anodes = filter ((=='A').tlast) (M.keys nodes)
    print $ part1 $ path step "AAA"
    print $ part2 $ path step <$> anodes

parse (inst:_:specs) =
    (inst, M.fromList $ mk <$> specs)
  where
    mk t = tmap tr t & twords & \[a,b,c] -> (a,(b,c))
      where tr c = if isAlphaNum c then c else ' '

path step x = zip [0..] $ scanl step x [0..]

part1 = find ((=="ZZZ").snd)

part2 = foldl' lcm 1 . map delta where
    zeds (i,t) = bool Nothing (Just i) $ tlast t == 'Z'
    delta xx = mapMaybe zeds xx & \(a:b:_) -> b - a
