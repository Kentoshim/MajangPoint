module Test where

import Types.Game
import Test.Tasty
import Test.Tasty.HUnit

unit_calculate_gameResult :: IO ()
unit_calculate_gameResult =
  gameResult defaultGameConfig ub1 ub2 ub3 ub4 @?= GameResult ur1 ur2 ur3 ur4
  where
    user = User (UserName "hoge") (UserID "hoge")
    ub1 = UserBones user (Bone 60000)
    ub2 = UserBones user (Bone 32000)
    ub3 = UserBones user (Bone 12600)
    ub4 = UserBones user (Bone (-4600))
    ur1 = UserPoints user (Point 60)
    ur2 = UserPoints user (Point 12)
    ur3 = UserPoints user (Point (-27))
    ur4 = UserPoints user (Point (-65))
