{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module
    Control.Arrow.Artemis.Build
      (
        buildArrow
      )
where

import Prelude hiding (id, (.))
import Control.Category
import Language.Haskell.TH

import Control.Arrow.Artemis.Type

buildArrow :: ArrowExp -> Q Exp
buildArrow aexp =
  do
    runIO $ print aexp
    let c = head $ expBody aexp
    p <- makeAllPicker $ cmdInput c
    return $ AppE (cmdBody c) p


-- Lookup name, failing is error
lookupMember :: String -> Q Name
lookupMember s =
  do
    mx <- lookupValueName s
    maybe (error $ "No function named " ++ s) return mx


-- Make combinatin to pick value.
makePicker :: CPat -> CPat -> Q (Maybe Exp)
makePicker x y | x == y =
  do
    id' <- lookupMember "id"
    return $ Just (VarE id')

makePicker (CPPair x1 x2) y =
  do
    mr1 <- makePicker x1 y
    mr2 <- makePicker x2 y
    case (mr1, mr2)
      of
        (Just _, Just _) -> error "Internal error: Patter redundant"
        (Just r1, Nothing) -> prepend "fst" r1 >>= return . Just
        (Nothing, Just r2) -> prepend "snd" r2 >>= return . Just
        _ -> return Nothing
  where
    prepend s exp =
      do
        dot' <- lookupMember "."
        f' <- lookupMember s
        return $ AppE (AppE (VarE dot') (VarE f')) exp

makePicker _ _ = return Nothing

-- Test: make picker for every member
makeAllPicker :: CPat -> Q Exp
makeAllPicker pat = go pat pat
  where
    go x (CPPair y1 y2) =
      do
        r1 <- go x y1
        r2 <- go x y2
        return $ TupE [r1, r2]

    go x CPUnnamed = return $ VarE 'undefined

    go x CPUnit = undefined

    go x y =
      do
        mr <- makePicker x y
        maybe (error $ "makeAllPicker fail") return mr
