-- https://adventofcode.com/2023/day/2

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding ( some )
import Text.Megaparsec hiding ( parse )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import qualified Data.Map.Strict as M

type Parser = Parsec Void Text

main = map parse . tlines <$> tgetContents >>= \pp -> do
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
        color <- some letterChar <&> tpack
        pure (color, count)

part1 = sum . map fst . filter (all (`leq` bag) . snd)

part2 = sum . map (product . M.unionsWith max . snd)

leq a b = all f (M.assocs a) where
    f (k,v) = v <= M.findWithDefault 0 k b

bag = M.fromList
    [ ("red",   12)
    , ("green", 13)
    , ("blue",  14)
    ]
