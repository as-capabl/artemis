{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module
    Control.Arrow.Artemis.Type
      (
        CPat'(..),
        CPat,
        listToCPat,
        setToCPat,
        Command(..),
        ArrowExp (..),
        NameSet,
        setToCPat
      )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Hashable
import Data.Witherable
import Data.Maybe (fromJust)
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

instance
    Witherable CPat'
  where
    mapMaybe f (CPIdent (f -> Nothing))  = CPUnnamed
    mapMaybe f (CPPair x y) = CPPair (mapMaybe f x) (mapMaybe f y)
    mapMaybe f cpat = fmap (fromJust . f) cpat

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
    cmdInput :: NameSet,
    cmdOutput :: CPat,
    cmdBody :: Either [Dec] Exp
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

