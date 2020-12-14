module Types.Tiles where

class Tiles a where

-- 麻雀牌
data Tile = Dragon Dragon
          | Suit Suit
          | Wind Wind
  deriving (Show, Eq, Ord)

isSimpleTile :: Tile -> Bool
isSimpleTile (Suit s) = isSimple $ numOfSuit s
isSimpleTile _ = False 
-- 風
data Wind = West
          | South
          | East
          | North
  deriving (Show, Eq, Ord, Enum)
instance Tiles Wind where

-- 三元牌
data Dragon = White
            | Green
            | Red
  deriving (Show, Eq, Ord, Enum)
instance Tiles Dragon where

-- 数牌
data Suit = Character Number
          | Circle Number
          | Banboo Number
  deriving (Show, Eq, Ord)
instance Tiles Suit where

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
