{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Game where

import Data.List
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
gameResult (GameConfig tally bonus) a b c d =
  case bonus of
    Nothing -> tmpResult
    Just b -> applyBonus b tmpResult
  where
    points = sortBy (flip compare)  $ map (toPoint tally) [a,b,c,d]
    tmpResult = fromList points
    fromList :: [UserPoints] -> GameResult
    fromList [a,b,c,d] = GameResult a b c d

applyBonus :: RankBonus -> GameResult -> GameResult
applyBonus (RankBonus first second) (GameResult a b c d) =
  GameResult (a `add` first) (b `add` second) (c `sub` second) (d `sub` first)
  where 
    add :: UserPoints -> Point -> UserPoints
    add (UserPoints u p) bonus = UserPoints u (p + bonus)
    sub :: UserPoints -> Point -> UserPoints
    sub (UserPoints u p) bonus = UserPoints u (p - bonus)

-- 対局結果(個人)
data UserBones = UserBones User Bone
  deriving (Show, Eq)
instance Ord UserBones where
  compare (UserBones _ a) (UserBones _ b) = compare a b

data UserPoints = UserPoints User Point
  deriving (Show, Eq)
instance Ord UserPoints where
  compare (UserPoints _ a) (UserPoints _ b) = compare a b

-- 得点から勝点へ
toPoint :: Tally -> UserBones -> UserPoints
toPoint (Tally t) (UserBones user (Bone bone)) =
  let  p = ent6 . (/ 1000) . fromIntegral $ bone - t
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
data User = User UserName UserID
  deriving (Show, Eq)

-- ユーザー名
newtype UserName = UserName String
  deriving (Show, Eq)

-- ユーザーID
newtype UserID = UserID String
  deriving (Show, Eq)

-- 対局のルール、原点やウマ、オカなど
defaultGameConfig = GameConfig (Tally 30000) (Just (RankBonus 30 10))
data GameConfig = GameConfig Tally (Maybe RankBonus)


-- 原点
newtype Tally = Tally Int
  deriving (Show, Eq, Num)

-- ウマ
data RankBonus = RankBonus Point Point
