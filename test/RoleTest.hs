module RoleTest where

import Test.Tasty
import Test.Tasty.HUnit
import Types.Roles
import Types.Tiles

-- タンヤオ
unit_isAllShimple :: IO ()
unit_isAllShimple =
  detectAllSimple hand @?= Just (AllSimple 1)
  where
    hand :: Hand
    hand 
      = UsualHand (Pair . Suit $ Character Five)
             (Seq (Suit $ Character Five, Suit $ Character Six, Suit $ Character Seven))
             (Seq (Suit $ Banboo Five, Suit $ Banboo Six, Suit $ Character Seven))
             (Seq (Suit $ Circle Three, Suit $ Circle Four, Suit $ Circle Five))
             (Triple (Suit $ Circle Three))
