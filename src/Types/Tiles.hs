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

suitsNumber :: Suit -> Number
suitsNumber (Character n) = n
suitsNumber (Circle n) = n
suitsNumber (Banboo n) = n

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

hand :: [Tile] -> Either String Hand
hand ts
  | length ts /= 14 = Left "hand must consist of 14"
  | otherwise = undefined

elements :: Hand -> [Element]
elements (UsualHand _ e1 e2 e3 e4) = [e1, e2, e3, e4]
elements _ = []

isSeq :: Element -> Bool
isSeq SequenceElement {} = True
isSeq _ = False

isSimpleElement :: Element -> Bool
isSimpleElement el = all isSimpleTile $ tiles el

-- 和了牌
data WinninigTile
  = SelfDraw Tile
  | FromDiscard Tile

data Element
  = PairElement Pair
  | SequenceElement Seq
  | TripleElement Triple
  | QuadElement Quad
  deriving (Show, Eq)

-- 対子
newtype Pair = Pair { getPair :: Tile }
  deriving (Show, Eq, Ord)
-- 順子
data Seq = Seq Tile Tile Tile
  deriving (Show, Eq, Ord)
-- 刻子
newtype Triple = Triple { getTriple :: Tile }
  deriving (Show, Eq, Ord)
-- 槓子
newtype Quad = Quad { getQuad :: Tile }
  deriving (Show, Eq, Ord)

-- ElementからTileを取り出す
tiles :: Element -> [Tile]
tiles (PairElement p) = [getPair p]
tiles (TripleElement t) = [getTriple t]
tiles (QuadElement q) = [getQuad q]
tiles (SequenceElement (Seq a b c)) = [a, b, c]

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (==x) xs

allSameSuit :: [Suit] -> Bool
allSameSuit [] = True
allSameSuit (s:ss) =
  case s of
    Character _ -> all isCharacter ss
    Circle _ -> all isCircle ss
    Banboo _ -> all isBanboo ss
  where
    isCharacter (Character _) = True
    isCharacter _ = False
    isCircle (Circle _) = True
    isCircle _ = False
    isBanboo (Banboo _) = True
    isBanboo _ = False

continuous :: (Enum a, Eq a) => a -> a -> Bool
continuous a b = b == succ a

isSequence :: Suit -> Suit -> Suit -> Bool
isSequence a b c = allSameSuit [a, b, c] && na `continuous` nb && nb `continuous` nc
  where 
    na = suitsNumber a
    nb = suitsNumber b
    nc = suitsNumber c

