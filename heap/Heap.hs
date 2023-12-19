module Heap ( Heap, heap, empty, insert, view, merge ) where

import Prelude hiding ( empty, insert, view )

data Heap a = Leaf | Heap a (Heap a) (Heap a) deriving Show

heap a = Heap a Leaf Leaf

empty = Leaf

insert a h = merge (heap a) h

view Leaf = Nothing
view (Heap a l r) = Just (a, merge l r)

merge Leaf h = h
merge h Leaf = h
merge u@(Heap a l r) v@(Heap b _ _)
    | a > b = merge v u
    | rank r' > rank l = Heap a r' l
    | otherwise        = Heap a l r'
  where r' = merge r v

rank Leaf = 0
rank (Heap _ _ r) = rank r + 1
