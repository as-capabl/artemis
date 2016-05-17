{-# LANGUAGE TemplateHaskell #-}

module
    Control.Arrow.Artemis.Modification
      (
        splitLambda,
        makeLambda,
        expToCPat,
        listInputs
      )
where

import Text.Parsec
import Text.Parsec.String
import Language.Haskell.TH
import Data.Generics

import Control.Arrow.Artemis.Type
import qualified Data.Set as Set

--
-- compose/decompose lambda
--
splitLambda :: Exp -> (CPat, Exp)
splitLambda (LamE pat body) =
    if length pat == 1
        then (expToCPat (head pat), body)
        else error "Internal error: Modification 20"
splitLambda noLambda = (CPUnit, noLambda)

makeLambda :: CPat -> Exp -> Exp
makeLambda cpat body = LamE [cpatToPat cpat] body

--
-- CPat <-> Pat
--
expToCPat :: Pat -> CPat
expToCPat (TupP l) =
    case l
      of
        (x:(y:[])) -> CPPair (expToCPat x) (expToCPat y)
        _ -> error "tuple length >= 3 not allowed"
expToCPat (VarP n) = CPIdent n
expToCPat (WildP) = CPUnnamed
expToCPat (ConP c _) =
    if c == '()
        then CPUnit
        else error ("Pattern match " ++ show c ++ " not allowed")

cpatToPat :: CPat -> Pat
cpatToPat (CPPair x y) = TupP [cpatToPat x, cpatToPat y]
cpatToPat (CPIdent x) = VarP x
cpatToPat (CPUnnamed) = WildP
cpatToPat (CPUnit) = ConP '() []
cpatToPat (CPTail) = error "Internal error: Modification 55"

--
-- Traverse expression
--
listInputs :: NameSet -> Exp -> NameSet
listInputs ns = everything Set.union (mkQ Set.empty unPat)
  where
    unPat (VarE n) | Set.member n ns = Set.singleton n
    unPat _ = Set.empty

