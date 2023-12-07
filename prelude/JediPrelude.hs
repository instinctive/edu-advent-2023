{-# LANGUAGE NoImplicitPrelude #-}

module JediPrelude
    ( module X
    , IntMap, Map, Set, Text
    , modifyArray
    , tgetContents, tindex, tlength, tlines, tmap, tpack, tunpack, twords
    , tdecimal
    ) where

import Prelude                   as X hiding ( index, lazy, uncons )
import Control.Lens              as X hiding ( para )
import Data.Array.ST             as X hiding ( index )
import Data.Array.IArray         as X hiding ( index, indices )
import Data.Containers.ListUtils as X
import Data.Functor.Base         as X hiding ( head, tail )
import Data.Functor.Foldable     as X hiding ( fold, gunfold )
import Data.List.Split           as X

import Data.IntMap.Strict ( IntMap )
import Data.Map.Strict    ( Map    )
import Data.Set           ( Set    )
import Data.Text          ( Text   )

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray ary k f = readArray ary k >>= writeArray ary k . f

tgetContents = T.getContents
tindex       = T.index
tlength      = T.length
tlines       = T.lines
tmap         = T.map
tpack        = T.pack
tunpack      = T.unpack
twords       = T.words

tdecimal :: Text -> Int
tdecimal t = either err fst $ T.decimal t where
    err s = error $ s <> " " <> show t
