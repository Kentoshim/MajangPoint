module Test where

import Types.Game
import Test.Tasty
import Test.Tasty.HUnit

-- 点数順に順位がつけられて、ウマオカの勝点が適用されることを確認
unit_calculate_gameResult_1 :: IO ()
unit_calculate_gameResult_1 =
  gameResult defaultGameConfig ub1 ub2 ub3 ub4 @?= GameResult ur1 ur2 ur3 ur4
  where
    user :: User
    user = User (UserName "hoge") (UserID "hoge") East
    ub1 = UserBones user (Bone 60000)
    ub2 = UserBones user (Bone 32000)
    ub3 = UserBones user (Bone 12600)
    ub4 = UserBones user (Bone (-4600))
    ur1 = UserPoints user (Point 80)
    ur2 = UserPoints user (Point 12)
    ur3 = UserPoints user (Point (-27))
    ur4 = UserPoints user (Point (-65))

-- 同点で終わった場合、席順に順位が決まる
unit_calculate_gameResult_2 :: IO ()
unit_calculate_gameResult_2 =
  gameResult defaultGameConfig ub1 ub2 ub3 ub4 @?= GameResult ur1 ur2 ur3 ur4
  where 
    user name wind = User (UserName name) (UserID "hoge") wind
    bone = Bone 25000
    east = user "1" East
    south = user "2" South
    west = user "3" West
    north = user "4" North
    ub1 = UserBones east bone
    ub2 = UserBones south bone
    ub3 = UserBones west bone
    ub4 = UserBones north bone
    ur1 = UserPoints east $ Point 45
    ur2 = UserPoints south $ Point 5
    ur3 = UserPoints west $ Point (-15)
    ur4 = UserPoints north $ Point (-35)

unit_make_gameConfig :: IO ()
unit_make_gameConfig =
  gameConfig 30000 25000 (Just (30, 10)) @?= defaultGameConfig

