{-# LANGUAGE NoImplicitPrelude #-}

module JediPrelude
    ( module X
    , Map, Set, Text
    , modifyArray
    ) where

import Prelude                   as X hiding ( index, lazy, uncons )
import Control.Lens              as X
import Data.Array.ST             as X hiding ( index )
import Data.Array.IArray         as X hiding ( index, indices )
import Data.Containers.ListUtils as X
import Data.List.Split           as X

import Data.Map.Strict ( Map  )
import Data.Set        ( Set  )
import Data.Text       ( Text )

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray ary k f = readArray ary k >>= writeArray ary k . f

