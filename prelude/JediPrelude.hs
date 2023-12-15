{-# LANGUAGE NoImplicitPrelude #-}

module JediPrelude
    ( module X
    , UArray
    , IntMap, Map, Set, Text
    , V2(..), _x, _y
    , modifyArray
    , tdrop, telem, tgetContents, tindex, tlast, tlength, tlines
    , tmap, tnull, tpack, tsplitOn, tunpack, twords
    , tsigned, tdecimal
    ) where

import Prelude                   as X hiding ( index, lazy, loop, uncons )
import Control.Lens              as X hiding ( para )
import Control.Monad.Extra       as X
import Data.Array.ST             as X hiding ( index )
import Data.Array.IArray         as X hiding ( index, indices )
import Data.Array.Unsafe         as X
import Data.Containers.ListUtils as X
import Data.Functor.Base         as X hiding ( head, tail )
import Data.Functor.Foldable     as X hiding ( fold, gunfold )
import Data.List.Split           as X

import Data.Array.Unboxed ( UArray   )
import Data.IntMap.Strict ( IntMap   )
import Data.Map.Strict    ( Map      )
import Data.Set           ( Set      )
import Data.Text          ( Text     )
import Linear.V2          ( V2(..), _x, _y )

import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import qualified Data.Text.Read as T

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray ary k f = readArray ary k >>= writeArray ary k . f

tdrop        = T.drop
telem        = T.elem
tgetContents = T.getContents
tindex       = T.index
tlast        = T.last
tlength      = T.length
tlines       = T.lines
tmap         = T.map
tnull        = T.null
tpack        = T.pack
tsplitOn     = T.splitOn
tunpack      = T.unpack
twords       = T.words

tsigned :: Text -> Int
tsigned t = either err check $ T.signed T.decimal t where
    err s = error $ s <> " " <> show t
    check (x,u)
        | T.null u = x
        | otherwise = error $ "incomplete tsigned: " <> show (x,u)

tdecimal :: Text -> Int
tdecimal t = either err check $ T.decimal t where
    err s = error $ s <> " " <> show t
    check (x,u)
        | T.null u = x
        | otherwise = error $ "incomplete tdecimal: " <> show (x,u)
