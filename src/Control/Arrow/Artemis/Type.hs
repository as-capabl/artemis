{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module
    Control.Arrow.Artemis.Type
      (
        CPat'(..),
        CPat,
        listToCPat,
        Command(..),
        ArrowExp (..),
        NameSet,
        setToCPat,
      )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Hashable
import qualified Data.HashSet as H
import qualified Data.Traversable as Tv
import Data.Set as Set

-- |Cartesian Pattern
data CPat' a =
    CPTail |
    CPIdent a |
    CPPair (CPat' a) (CPat' a) |
    CPUnnamed | -- _
    CPUnit -- ()
  deriving (Eq, Functor, Foldable, Traversable)

type CPat = CPat' Name

instance
    Show a => Show (CPat' a)
  where
    show CPTail = "*"
    show (CPIdent n) = show n
    show (CPPair x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show CPUnnamed = "_"
    show CPUnit = "()"

listToCPat :: [Name] -> CPat
listToCPat (x:xs) = CPPair (CPIdent x) (listToCPat xs)
listToCPat [] = CPUnit

setToCPat :: NameSet -> CPat
setToCPat  = listToCPat . Set.toList

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

--
-- Name hash
--
type NameSet = Set.Set Name

{-
instance
    Hashable OccName
  where
    hash = hash . show

instance
    Hashable NameSpace
  where
    hash = hash . show

instance
    Hashable NameFlavour
  where
    hash = hash . show

instance
    Hashable Name
  where
    hash = hash . showName
-}
