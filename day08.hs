-- https://adventofcode.com/2023/day/8

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M

main = tgetContents <&> parse >>= \p -> do
    print $ part1 p
    print $ part2 p

data Problem = Problem
    { _inst   :: Text
    , _nnodes :: Int
    , _aaa    :: Int
    , _zzz    :: Int
    , _alphas :: [Int]
    , _zeds   :: [Int]
    , _paths  :: UArray (Char,Int) Int
    } deriving Show

parse t = Problem
    { _inst   = inst
    , _nnodes = nnodes
    , _aaa    = label "AAA"
    , _zzz    = label "ZZZ"
    , _alphas = endsIn 'A'
    , _zeds   = endsIn 'Z'
    , _paths  = listArray (('0',0),('1',nnodes-1)) $
        map label lefts <> map label rights
    }
  where
    inst':_:nodes' = tlines t
    inst = tmap tr inst'
      where tr 'L' = '0'; tr 'R' = '1'
    [ nodes, lefts, rights ] = transpose $ mk <$> nodes' where
        mk = twords . tmap \c -> if isAlphaNum c then c else ' '
    nodeIds = M.fromList $ zip nodes [0..]
    endsIn c = M.assocs nodeIds & filter ((==c).tlast.fst) & map snd
    label t = nodeIds M.! t
    nnodes = M.size nodeIds

next Problem{..} i x = _paths ! (c,x)
  where c = tindex _inst (i `mod` tlength _inst)

part1 prob@Problem{..} = go 0 _aaa where
    go i x = if x == _zzz then i else go (i+1) (next prob i x)

part2 prob@Problem{..} = runST do
    let nalphas = length _alphas
    zarray <- newArray (0,_nnodes-1) False :: ST s (STUArray s Int Bool)
    for_ _zeds do \z -> writeArray zarray z True
    ghosts <- newListArray (1, nalphas) _alphas :: ST s (STUArray s Int Int)
    let ztest i = readArray ghosts i >>= readArray zarray
    let loop i = ifM (allM ztest [1..nalphas]) (pure i) do
            for_ [1..nalphas] $ \g ->
                modifyArray ghosts g (next prob i)
            loop (i+1)
    loop 0

-- -- TODO naive and slow "solution"
-- part2 next = go 0 where
--     go i zz | all ((=='Z').tlast) zz = Just i
--     go i zz = go (i+1) $ map (next i) zz
