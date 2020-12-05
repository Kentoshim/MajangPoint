module Types.Game where

-- 対局
data Game = Game GameName [User] 

-- 対局名
newtype GameName = GameName String

-- 対局結果 (試合全体)
data GameResult = GameResult UserResult UserResult UserResult UserResult

-- TODO
-- 点数計算のルールを適用して順位順でUserResultを並び替えてGameResultにする
gameResult :: GameConfig -> UserResult -> UserResult -> UserResult -> UserResult -> GameResult
gameResult config a b c d =
  let list = [a,b,c,d]
  in  undefined

-- 対局結果(個人)
newtype UserResult = UserResult (User, Bone)

-- 点数 英語では点棒をBonesと言う
newtype Bone = Bone Int

-- ユーザー
data User = User UserName UserID

-- ユーザー名
newtype UserName = UserName String

-- ユーザーID
newtype UserID = UserID String

-- 対局のルール、原点やウマ、オカなど
data GameConfig = GameConfig Tally (Maybe RankBonus) (Maybe TopPrize)

-- 原点
newtype Tally = Tally Int

-- ウマ
data RankBonus = RankBonus Int Int

-- オカ
newtype TopPrize = TopPrize Int
