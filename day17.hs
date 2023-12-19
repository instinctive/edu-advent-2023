-- https://adventofcode.com/2023/day/17

module Main where

import Prelude hiding ( insert, view )
import Control.Monad.State ( evalState, gets, modify' )
import qualified Data.Map.Strict as M

import Grid
import qualified Heap as H

type GRID = (?grid :: Grid)

main = tgetContents <&> mkGrid . tlines >>= \grid ->
    let ?grid = grid in
    each print ( solve part1, solve part2)

insert w key@((r,c),_,_) h = do
    let v = w - r - c
    gets (M.lookup key) >>= \case
        Just v' | v >= v' -> pure h
        _ -> do
            modify' (M.insert key v)
            pure $ H.insert (v,key) h

view = fmap (first f) . H.view where
    f (w,key@((r,c),_,_)) = (w+r+c,key)

isFinal (r,c) =
    r == _gridRows ?grid - 1 &&
    c == _gridCols ?grid - 1

solve part =
    flip evalState M.empty $ do
        h <- insert 0 ((0,1),E,1) H.empty
        h <- insert 0 ((1,0),S,1) h
        part h

part1 ( view -> Just (q@(w,(rc,d,n)),h) )
    | n > 3     = part1 h
    | otherwise = atGrid ?grid rc & maybe (part1 h) \x ->
        let v = digitToInt x + w
            l = ccwTurn d
            r =  cwTurn d
        in
        if isFinal rc then pure v else do
            h' <- insert v (move rc d, d, n+1) h
            h' <- insert v (move rc l, l,   1) h'
            h' <- insert v (move rc r, r,   1) h'
            part1 h'

part2 ( view -> Just (q@(w,(rc,d,n)),h) )
    | n > 10    = part2 h
    | otherwise = atGrid ?grid rc & maybe (part2 h) \x ->
        let v = digitToInt x + w
            l = ccwTurn d
            r =  cwTurn d
        in
        if n < 4 then do
            h' <- insert v (move rc d, d, n+1) h
            part2 h'
        else if isFinal rc then pure v
        else do
            h' <- insert v (move rc d, d, n+1) h
            h' <- insert v (move rc l, l,   1) h'
            h' <- insert v (move rc r, r,   1) h'
            part2 h'
