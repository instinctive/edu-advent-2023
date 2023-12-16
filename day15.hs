-- https://adventofcode.com/2023/day/15

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          as T
import qualified Data.IntMap.Strict as IM

main = tgetContents <&> tlines >>= \[t] -> do
    print . sum   $ xhash <$> tsplitOn "," t
    print . part2 $ parse <$> tsplitOn "," t

xhash = T.foldl' f 0 where
    f z c = 17 * (z + ord c) .&. 0xff

data Command = Del Int Text | Add Int Text Int

parse t =
    command
  where
    (label,params) = T.span isAlpha t
    Just (op,vtext) = T.uncons params
    value = tdecimal vtext
    command = case op of
        '=' -> Add (xhash label) label value
        '-' -> Del (xhash label) label

del l [] = []
del l ((l',_):more) | l == l' = more
del l (q:more) = q : del l more

add l v [] = [(l,v)]
add l v ((l',_):more) | l == l' = (l,v):more
add l v (q:more) = q : add l v more

part2 =
    IM.foldlWithKey' ans 0 . foldl' cmd IM.empty
  where
    cmd m (Del x l)   = IM.adjust (del l) x m
    cmd m (Add x l v) = IM.alter f x m where
        f Nothing = Just [(l,v)]
        f (Just qq) = Just $ add l v qq
    ans z x qq = z + (x+1) * sum (zipWith (*) [1..] $ snd <$> qq)
