module Types.Roles where

import Types.Tiles as Tiles

data Role
  -- 平和
  = AllSequence Int
  | AllSimple Int
  | FourTriple Int
  | SevenPairs Int
  | ThirteenOrphans Int
  deriving (Show, Eq, Ord)

detectAllSequence :: Hand -> Maybe Role
detectAllSequence h = if all isSeq $ elements h 
  then Just allSequence
  else Nothing

detectAllSimple :: Hand -> Maybe Role
detectAllSimple h@(UsualHand p _ _ _ _) =
  let pair = isSimpleTile $ unPair p
      element = all isSimpleElement $ elements h
  in  if pair && element
    then Just allSimple
    else Nothing

allSimple :: Role
allSimple = AllSimple 1

allSequence :: Role
allSequence = AllSequence 2

sevenPairs :: Role
sevenPairs = SevenPairs 2

thirteenOrphans :: Role
thirteenOrphans = ThirteenOrphans 13

detectHand :: Hand -> [Role]
detectHand SevenPairsHand {} = [sevenPairs]
detectHand ThirteenOrphansHand {} = [thirteenOrphans]
detectHand (UsualHand p e1 e2 e3 e4) =
  undefined

