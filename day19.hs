-- https://adventofcode.com/2023/day/19

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding ( many, some, sepBy, Item, try, Op )
import Text.Megaparsec hiding ( parse )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type Parser = Parsec Void Text

data Cat    = X | M | A | S               deriving (Eq,Show)
data Action = Accept | Reject | Name Text deriving Show
type Rule   = (Maybe (Cat,Ordering,Int), Action)
type Flow   = (Text,[Rule])
type Item   = [(Cat,Int)]

main = tgetContents >>= out . parse parser
  where
    parse p =
        either (error . errorBundlePretty) id .
        runParser p "foo"
    out (ff,ii) = do
        -- traverse_ print ff
        -- traverse_ print ii
        print $ sum $ run (M.fromList ff) <$> ii

run :: Map Text [Rule] -> Item -> Int
run rules = go start where
    start = rules M.! "in"
    go [(Nothing,a)] i = action a i
    go ((Just test,a):more) i
        | check test i = action a i
        | otherwise = go more i
    check (k,ord,v) i = fmap (`compare` v) (lookup k i) == Just ord
    action Reject _   = 0
    action Accept i   = sum $ snd <$> i
    action (Name t) i = go (rules M.! t) i

parser :: Parser ([Flow],[Item])
parser =
    (,) <$> some flow <*> (newline *> some item)
  where
    flow = (,) <$> name <*> braces (commas rule) <* newline
    item = braces (commas rating) <* newline
    rule = try test <|> (action <&> (Nothing,))
    test = (,) <$> check <*> (char ':' *> action)
    action = accept <|> reject <|> (name <&> Name)
    accept = char 'A' <&> const Accept
    reject = char 'R' <&> const Reject
    check = reln "<>" <&> Just
    rating = reln "=" <&> \(k,_,v) -> (k,v)
    reln (opchars :: String) = (,,) <$> cat <*> op opchars <*> decimal
    op opchars = satisfy (`elem` opchars) <&> \case '<' -> LT; '>' -> GT; '=' -> EQ
    cat = satisfy (`elem` ("xmas"::String)) <&> \case 'x' -> X; 'm' -> M; 'a' -> A; 's' -> S
    name = some letterChar <&> T.pack
    braces = between (char '{') (char '}')
    commas p = sepBy p (char ',')
