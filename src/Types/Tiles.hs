module Types.Tiles where

import Data.List

-- 麻雀牌
data Tile = Dragon Dragon
          | Wind Wind
          | Suit Suit
  deriving (Show, Eq)

instance Ord Tile where
  compare (Dragon _) (Wind _) = GT
  compare (Dragon _) (Suit _) = GT
  compare (Wind _) (Dragon _) = LT
  compare (Wind _) (Suit _) = GT
  compare (Suit _) (Dragon _) = LT
  compare (Suit _) (Wind _) = LT
  compare (Dragon l) (Dragon r) = compare l r
  compare (Wind l) (Wind r) = compare l r
  compare (Suit l) (Suit r) = compare l r


isSimpleTile :: Tile -> Bool
isSimpleTile (Suit s) = isSimple $ numOfSuit s
isSimpleTile _ = False 
-- 風
data Wind = West
          | South
          | East
          | North
  deriving (Show, Eq, Enum)
instance Ord Wind where
  compare l r
    | l == r = EQ
    | otherwise = case (l, r) of
        (West, _) -> GT
        (South, West) -> LT
        (South, East) -> GT
        (South, North) -> GT
        (East, South) -> LT
        (East, West) -> LT
        (East, North) -> GT
        (North, North) -> EQ
        (North, _) -> LT

-- 三元牌
data Dragon = White
            | Green
            | Red
  deriving (Show, Eq, Enum)
instance Ord Dragon where
  compare White Green = GT
  compare White Red = GT
  compare Green White = LT
  compare Green Red = GT
  compare Red White = LT
  compare Red Green = LT
  compare _ _ = EQ

-- 数牌
data Suit = Character Number
          | Circle Number
          | Banboo Number
  deriving (Show, Eq)
instance Ord Suit where
  compare (Character _) (Circle _) = GT
  compare (Character _) (Banboo _) = GT
  compare (Circle _) (Character _) = LT
  compare (Circle _) (Banboo _) = GT
  compare (Banboo _) (Character _) = LT
  compare (Banboo _) (Circle _) = LT
  compare (Character l) (Character r) = compare l r
  compare (Circle l) (Circle r) = compare l r
  compare (Banboo l) (Banboo r) = compare l r

data Number = One
            | Two
            | Three
            | Four
            | Five
            | Six
            | Seven
            | Eight
            | Nine
  deriving (Show, Eq, Ord, Enum)

fromInt :: Int -> Maybe Number
fromInt i
  | i < 1 || 9 < i = Nothing
  | otherwise =
      Just $ case i of
        1 -> One
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        6 -> Six
        7 -> Seven
        8 -> Eight
        9 -> Nine

isSimple :: Number -> Bool
isSimple One = False
isSimple Nine = False
isSimple _ = True

numOfSuit :: Suit -> Number
numOfSuit (Character a) = a
numOfSuit (Circle a) = a
numOfSuit (Banboo a) = a

-- 最終的な手牌
data Hand
  = UsualHand Pair Element Element Element Element
  | SevenPairsHand Pair Pair Pair Pair Pair Pair Pair
  | ThirteenOrphansHand Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile
  | None Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile

elements :: Hand -> [Element]
elements (UsualHand _ e1 e2 e3 e4) = [e1, e2, e3, e4]
elements _ = []

isSeq :: Element -> Bool
isSeq (Seq _) = True
isSeq _ = False

isSimpleElement :: Element -> Bool
isSimpleElement (Seq (a, b, c)) = all isSimpleTile [a,b,c]
isSimpleElement (Triple t) = isSimpleTile t

-- 和了牌
data WinninigTile
  = SelfDraw Tile
  | FromDiscard Tile

-- 対子
newtype Pair = Pair Tile
unPair :: Pair -> Tile
unPair (Pair a) = a

-- 面子
data Element 
  = Seq (Tile, Tile, Tile)
  | Triple Tile
  | Quad Tile
  deriving (Show, Eq)
