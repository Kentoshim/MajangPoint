{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.TileParser where

import Types.Tiles
import Types.Roles

import Control.Exception
import Control.Monad.State
import Control.Monad
import Control.Applicative ((<$>), (<*>), Alternative)
import Data.Char
import Data.Void
import Data.Proxy
import Text.Megaparsec
import Text.Megaparsec.Stream

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.List as DL

type Parser = Parsec Void [Tile]

instance VisualStream [Tile] where
  showTokens Proxy =
    -- stringPretty $ NE.fromList $ concat  $ NE.toList $ NE.map show tiles
    -- stringPretty $ NE.fromList . concatMap showTile $ NE.toList tiles
    DL.intercalate "|" .NE.toList . fmap showTile
instance TraversableStream [Tile] where
  reachOffsetNoLine o PosState {..} =
    PosState 
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix = pstateLinePrefix
      }
    where
      (pre, post) = splitAt (o - pstateOffset) pstateInput
      spos = foldl go pstateSourcePos pre
      go (SourcePos n l c) ch =
        let c' = unPos c
            w = unPos pstateTabWidth
        in SourcePos n l (c <> pos1)

anyTile :: Parser Tile
anyTile = anySingle

simple :: Parser Tile
simple = satisfy isSimpleTile

isA :: Tile -> Parser Tile
isA t = satisfy (== t)
