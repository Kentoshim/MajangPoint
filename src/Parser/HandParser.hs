{-# LANGUAGE OverloadedStrings #-}

module Parser.HandParser where

import qualified Data.Attoparsec.Text as PT
import Data.Char
import Data.Text
import Control.Applicative

import qualified Types.Tiles as Tile

-- hand :: PT.Parser Tile.Hand
-- hand = usualHand <|> thirteenOrphans <|> sevenPairs

-- usualHand :: PT.Parser Tile.Hand
-- usualHand =

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