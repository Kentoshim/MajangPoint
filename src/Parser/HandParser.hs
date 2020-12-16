{-# LANGUAGE OverloadedStrings #-}

module Parser.HandParser where

import qualified Data.Attoparsec.Text as PT
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Control.Applicative

import qualified Types.Tiles as Tile

-- hand :: PT.Parser Tile.Hand
-- hand = usualHand <|> thirteenOrphans <|> sevenPairs

-- usualHand :: PT.Parser Tile.Hand
-- usualHand =

element :: PT.Parser Tile.Element
element = 
  (Tile.SequenceElement <$> Parser.HandParser.seq) <|>
  (Tile.QuadElement <$> quad) <|>
  (Tile.TripleElement <$> triple) <|>
  (Tile.PairElement <$> pair)

pair :: PT.Parser Tile.Pair
pair = do
  t1 <- tile
  t2 <- tile
  if t1 == t2
    then return $ Tile.Pair t1
    else fail $ "not Pair " ++ show t1 ++ " and " ++ show t2

triple :: PT.Parser Tile.Triple
triple = do
  t1 <- tile
  t2 <- tile
  t3 <- tile
  if Tile.allSame [t2, t3]
    then return $ Tile.Triple t1
    else fail $ "not triple " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3

quad :: PT.Parser Tile.Quad
quad = do 
  (t:ts) <- replicateM 4 tile
  if Prelude.all (==t) ts
    then return $ Tile.Quad t
    else fail $ "not quad " ++ concatMap show (t:ts)

seq :: PT.Parser Tile.Seq
seq = do
  s1 <- suit
  s2 <- suit
  s3 <- suit
  if Tile.isSequence s1 s2 s3
    then return $ Tile.Seq (Tile.Suit s1) (Tile.Suit s2) (Tile.Suit s3)
    else fail $ "not seq" ++ concatMap show [s1,s2,s3]

tile :: PT.Parser Tile.Tile
tile = (Tile.Suit <$> suit) <|> (Tile.Dragon <$> dragon) <|> ( Tile.Wind <$> wind)

dragon :: PT.Parser Tile.Dragon
dragon = white <|> red <|> green

white :: PT.Parser Tile.Dragon
white = Tile.White <$ PT.char '白'

green :: PT.Parser Tile.Dragon
green = Tile.Green <$ (charCI 'r' <|> PT.char '發')

red :: PT.Parser Tile.Dragon
red = Tile.Red <$ PT.char '中'

wind ::PT.Parser Tile.Wind
wind = east <|> south <|> west <|> north

east :: PT.Parser Tile.Wind
east = Tile.East <$ (charCI 'e' <|> PT.char '東')

south :: PT.Parser Tile.Wind
south = Tile.South <$ (charCI 's' <|> PT.char '南')

west :: PT.Parser Tile.Wind
west = Tile.West <$ (charCI 'w' <|> PT.char '西')

north :: PT.Parser Tile.Wind
north = Tile.North <$ (charCI 'n' <|> PT.char '北')

number :: PT.Parser Tile.Number
number = do
  i <- digitToInt <$> PT.digit
  case Tile.fromInt i of
    Nothing -> fail $ "not a Number" ++ show i
    Just n -> return n

suit :: PT.Parser Tile.Suit
suit = character <|> circle <|> banboo

character :: PT.Parser Tile.Suit
character = Tile.Character <$> number 
            <* (charCI 'm' <|> PT.char '萬')

circle :: PT.Parser Tile.Suit
circle = Tile.Character <$> number 
  <* (charCI 'p' <|> PT.char '筒')

banboo :: PT.Parser Tile.Suit
banboo = Tile.Banboo <$> number <* (charCI 'b' <|> PT.char '索')

charCI :: Char -> PT.Parser Char
charCI input = do
  PT.satisfy (\c -> Data.Char.toLower c == Data.Char.toLower input) PT.<?> "not " ++ show input