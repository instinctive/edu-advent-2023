module Grid where

type Pos = (Int,Int)

data NWSE = N | W | S | E deriving (Eq,Ord,Show)

opp N = S
opp S = N
opp E = W
opp W = E

cwTurn N = E
cwTurn E = S
cwTurn S = W
cwTurn W = N

ccwTurn N = W
ccwTurn W = S
ccwTurn S = E
ccwTurn E = N

reflectFwdSlash N = E
reflectFwdSlash W = S
reflectFwdSlash S = W
reflectFwdSlash E = N

reflectBwdSlash N = W
reflectBwdSlash W = N
reflectBwdSlash S = E
reflectBwdSlash E = S

splitPipe N = [N]
splitPipe S = [S]
splitPipe E = [N,S]
splitPipe W = [N,S]

splitDash N = [E,W]
splitDash S = [E,W]
splitDash E = [E]
splitDash W = [W]

move (r,c) N = (r-1,c)
move (r,c) S = (r+1,c)
move (r,c) W = (r,c-1)
move (r,c) E = (r,c+1)

data Grid = Grid
    { _gridRows  :: Int
    , _gridCols  :: Int
    , _gridArray :: Array Int Text
    } deriving Show

mkGrid t =
    Grid nrows ncols ary
  where
    tt = tlines t
    nrows = length tt
    ncols = tlength (head tt)
    ary = listArray (0,nrows-1) tt

atGrid Grid{..} (r,c)
    | r < 0 || r >= _gridRows = Nothing
    | c < 0 || c >= _gridCols = Nothing
    | otherwise = Just (tindex (_gridArray ! r) c)
