{-# LANGUAGE OverloadedStrings #-}

module Parser.HandParser where

import qualified Data.Attoparsec.Text as PT
import Data.Char
import Data.Text
import Data.ByteString
import Control.Applicative
import Control.Monad.Trans.Maybe

import qualified Types.Tiles as Tile

tile :: PT.Parser Tile.Tile
tile = (Tile.Suit <$> suit) <|> (Tile.Dragon <$> dragon) -- <|> ( Tile.Wind <$> wind)

dragon :: PT.Parser Tile.Dragon
dragon = white <|> red <|> green

white :: PT.Parser Tile.Dragon
white = Tile.White <$ (charCI 'w' <|> PT.char '白')

red :: PT.Parser Tile.Dragon
red = Tile.Red <$ (charCI 'r' <|> PT.char '白')

green :: PT.Parser Tile.Dragon
green = Tile.Green <$ (charCI 'g' <|> PT.char '發')

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