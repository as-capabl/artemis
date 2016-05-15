module
    Control.Arrow.Artemis.Type
      (
        CPat(..),
        Command(..),
        ArrowExp (..)
      )
where

import Language.Haskell.TH

-- |Cartesian Pattern
data CPat =
    CPTail |
    CPIdent Name |
    CPPair CPat CPat |
    CPUnnamed | -- _
    CPUnit -- ()
  deriving Eq

instance
    Show CPat
  where
    show CPTail = "*"
    show (CPIdent n) = nameBase n
    show (CPPair x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show CPUnnamed = "_"
    show CPUnit = "()"

-- |Arrow command
data Command = Command {
    cmdInput :: CPat,
    cmdOutput :: CPat,
    cmdBody :: Exp
  }
  deriving Show

data Composition =
    CmpIdentity CPat |
    CmpSplit CPat |
    CmpCommand Command |
    CmpConcat Composition Composition |
    CmpParalell Composition Composition
  deriving Show

data ArrowExp = ArrowExp {
    expInput :: CPat,
    expBody :: [Command]
  }
  deriving Show

