-- https://adventofcode.com/2023/day/2

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding ( some )

import Data.Map.Strict ( Map  )
import Data.Text       ( Text )
import Text.Megaparsec hiding ( parse )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

type Parser = Parsec Void Text

main = map parse . T.lines <$> T.getContents >>= \pp -> do
    print $ part1 pp
    print $ part2 pp
  where
    parse = fromJust . parseMaybe gameParser

gameParser :: Parser (Int, [Map Text Int])
gameParser =
    (,) <$> gameId <*> reveals
  where
    gameId = string "Game " *> decimal <* string ": "
    reveals = sepBy1 samples (string "; ")
    samples = sepBy1 sample (string ", ") <&> M.fromList
    sample = do
        count <- decimal
        spaceChar
        color <- some letterChar <&> T.pack
        pure (color, count)

part1 = sum . map fst . filter (all (`leq` bag) . snd)

part2 = sum . map power where
    power = product . M.elems . M.unionsWith max . snd

leq a b = all f (M.assocs a) where
    f (k,v) = v <= M.findWithDefault 0 k b

bag = M.fromList
    [ ("red",   12)
    , ("green", 13)
    , ("blue",  14)
    ]
