{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Game where

import Data.List
import Control.Applicative

-- 対局
data Game = Game GameName [User] 

-- 対局名
newtype GameName = GameName String

-- 対局結果 (試合全体)
data GameResult = GameResult UserPoints UserPoints UserPoints UserPoints
  deriving (Show, Eq)

-- TODO
-- 点数計算のルールを適用して順位順でUserResultを並び替えてGameResultにする
gameResult :: GameConfig -> UserBones -> UserBones -> UserBones -> UserBones -> GameResult
gameResult config a b c d =
  applyBonus config tmpResult
  where
    points = map (toPoint config) $ sortBy (flip compare) [a,b,c,d]
    tmpResult = fromList points
    fromList :: [UserPoints] -> GameResult
    fromList [a,b,c,d] = GameResult a b c d

-- ウマオカの計算をして勝点に反映する
applyBonus :: GameConfig -> GameResult -> GameResult
applyBonus (GameConfig t f mbonus) (GameResult a b c d) =
  GameResult (a `add` first `add` topBonus) (b `add` second) (c `sub` second) (d `sub` first)
  where
    topBonus = Point . round . (/ 1000) . fromIntegral $ (unTally t - unFirstPoint f) * 4
    bonus = case mbonus of
      Nothing -> (Point 0, Point 0)
      Just (RankBonus f s) -> (f, s)
    first = fst bonus
    second = snd bonus
    add :: UserPoints -> Point -> UserPoints
    add (UserPoints u p) bonus = UserPoints u (p + bonus)
    sub :: UserPoints -> Point -> UserPoints
    sub (UserPoints u p) bonus = UserPoints u (p - bonus)

-- 対局結果(個人)
data UserBones = UserBones { userInfo :: User, userBone :: Bone}
  deriving (Show, Eq)
instance Ord UserBones where
  compare (UserBones u1 a) (UserBones u2 b)
    | compared == EQ = comparedWind
    | otherwise = a `compare` b
    where 
      compared = a `compare` b
      comparedWind = userWind u2 `compare` userWind u1

data UserPoints = UserPoints User Point
  deriving (Show, Eq)
instance Ord UserPoints where
  compare (UserPoints _ a) (UserPoints _ b) = compare a b

-- 得点から勝点へ
toPoint :: GameConfig -> UserBones -> UserPoints
toPoint (GameConfig t _ _) (UserBones user (Bone bone)) =
  let  p = ent6 . (/ 1000) . fromIntegral $ bone - unTally t
  in UserPoints user (Point p)

-- 五捨六入する
ent6 ::(RealFrac a, Integral b) => a -> b
ent6 i
  | r < 6 = n
  | otherwise = if i > 0 
      then n + 1
      else n - 1
  where 
    (n, fl) = properFraction i
    r = round . (* 10) $ abs fl


newtype Point = Point Int
  deriving (Show, Eq, Ord, Num)

-- 点数 英語では点棒をBonesと言う
newtype Bone = Bone Int
  deriving (Show, Eq, Ord, Num)

-- ユーザー
data User = User { userName :: UserName, userID :: UserID, userWind :: Wind }
  deriving (Show, Eq)

instance Ord User where
  compare (User _ _ wind) (User _ _ wind') = compare wind wind'

-- ユーザー名
newtype UserName = UserName String
  deriving (Show, Eq, Ord)

-- ユーザーID
newtype UserID = UserID String
  deriving (Show, Eq, Ord)

-- Wind
data Wind = East | South | West | North
  deriving (Show, Eq, Ord)

-- 基本的なゲーム設定 25000点持ち30000点返し、オカ: 一位30、二位10
defaultGameConfig = GameConfig (Tally 30000) (FirstPoint 25000) (Just (RankBonus 30 10))
data GameConfig = GameConfig Tally FirstPoint (Maybe RankBonus)

-- 対局のルール、原点やウマ、オカなど
gameConfig :: Int -> Int -> Maybe (Int, Int) ->  GameConfig
gameConfig t f bonus =
  case bonus of
    Nothing -> GameConfig (Tally t) (FirstPoint f) Nothing
    Just (fs, sn) -> GameConfig (Tally t) (FirstPoint f) (Just (RankBonus (Point fs) (Point sn)))

-- 初期の持ち点
newtype FirstPoint = FirstPoint { unFirstPoint :: Int }
  deriving (Show, Eq, Num)

-- 原点
newtype Tally = Tally  { unTally :: Int }
  deriving (Show, Eq, Num)

-- ウマ
data RankBonus = RankBonus Point Point
