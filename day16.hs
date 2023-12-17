-- https://adventofcode.com/2023/day/16

module Main where

import qualified Data.Map.Strict as M

import Grid

main = tgetContents <&> mkGrid . tlines >>= \g -> do
    print $ solve g (0,0) E
    print $ part2 g

part2 grid@Grid{..} =
    maximum $ uncurry (solve grid) <$> cands
  where
    r = _gridRows - 1
    c = _gridCols - 1
    cands =
        ( (,S) <$> (0,) <$> [0..c] ) <>
        ( (,N) <$> (r,) <$> [0..c] ) <>
        ( (,E) <$> (,0) <$> [0..r] ) <>
        ( (,W) <$> (,c) <$> [0..r] )

solve grid rc d =
    M.size $ go M.empty rc d
  where
    go m rc d = atGrid grid rc & maybe m \case
        _ | elem d (M.findWithDefault [] rc m) -> m
        '.'  -> go m' (move rc d) d
        '/'  -> go m' (move rc fwdslash) fwdslash
        '\\' -> go m' (move rc bwdslash) bwdslash
        '|'  -> foldl' (branch rc) m' (splitPipe d)
        '-'  -> foldl' (branch rc) m' (splitDash d)
      where
        m' = M.insertWith (<>) rc [d] m
        fwdslash = reflectFwdSlash d
        bwdslash = reflectBwdSlash d
        branch rc m d = go m (move rc d) d

