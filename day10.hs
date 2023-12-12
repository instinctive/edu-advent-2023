-- https://adventofcode.com/2023/day/10

module Main where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import qualified Data.Text   as T

data Dir = N | E | S | W deriving (Eq,Show)
type Pos = (Int,Int)

main = tgetContents <&> tlines >>= each print . solve

opp N = S
opp S = N
opp E = W
opp W = E

move N (r,c) = (r-1,c)
move S (r,c) = (r+1,c)
move E (r,c) = (r,c+1)
move W (r,c) = (r,c-1)

allow N = "|JL"
allow S = "|7F"
allow E = "-FL"
allow W = "-J7"

steer E '-' = E
steer E '7' = S
steer E 'J' = N
steer N '7' = W
steer N 'F' = E
steer N '|' = N
steer S 'J' = W
steer S 'L' = E
steer S '|' = S
steer W '-' = W
steer W 'F' = S
steer W 'L' = N
steer d x = error $ "steer: " <> show d <> " " <> show x

start '-' = W
start '7' = W
start 'J' = W
start 'L' = E
start 'F' = S
start '|' = S

solve tt =
    (part1,part2)
  where
    nrows = length tt
    ncols = tlength (head tt)

    [ spos ] = mapMaybe f $ zip [0..] tt where
        f (r,t) = T.findIndex (=='S') t <&> (r,)

    ary = listArray (0,nrows-1) tt :: Array Int Text

    get (r,c) | r < 0 || r >= nrows || c < 0 || c >= ncols = '.'
              | otherwise = tindex (ary ! r) c

    [ schar ] = foldl1' intersect $ mapMaybe f [N,S,E,W] where
        f d | elem (get $ move d spos) (allow $ opp d) = Just $ allow d
            | otherwise = Nothing

    get' rc | rc == spos = schar
            | otherwise = get rc

    path = go S.empty spos (start schar) where
        go path rc d | not (S.null path) && rc == spos = path
                     | otherwise = go (S.insert rc path) rc' d'
          where
            rc' = move d rc
            d' = steer d (get' rc')

    get'' rc | S.member rc path = get' rc
             | otherwise = '.'

    part1 = div (S.size path) 2

    part2 = sum $ inside <$> [0..nrows-1] where
        inside r = go 0 False $ get'' . (r,) <$> [0..ncols-1]
        go n True "" = error $ "eol while inside"
        go n False "" = n
        go n b ('|':cc) = go n (not b) cc
        go n b ('.':cc) = go n' b cc where n' = if b then n+1 else n
        go n b ('F':'7':cc) = go n b cc
        go n b ('L':'J':cc) = go n b cc
        go n b ('F':'-':cc) = go n b ('F':cc)
        go n b ('L':'-':cc) = go n b ('L':cc)
        go n b ('F':'J':cc) = go n b ('|':cc)
        go n b ('L':'7':cc) = go n b ('|':cc)
        go _ _ cc = error $ "missing pattern: " <> show cc
