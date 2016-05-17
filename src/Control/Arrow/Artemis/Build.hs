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
import Control.Arrow.Artemis.Modification
import qualified Data.Set as Set
import qualified Data.Foldable as Fd

buildArrow :: ArrowExp -> Q Exp
buildArrow aexp =
  do
    --runIO $ print aexp
    let c = head $ expBody aexp
        gPat = expInput aexp
        lPat = cmdInput c
    runIO $ putStrLn "---------------------------"
    -- p <- makeAllPicker gPat lPat
    concatCommand gPat (expBody aexp)

concatCommand :: CPat -> [Command] -> Q Exp
concatCommand cpat0 cmds =
  do
    spliter <- lookupMember "&&&"
    id' <- lookupMember "id"
    dot' <- lookupMember "."
    go cpat0 cmds spliter id' dot'
  where
    go cpat (x:xs) spliter id' dot' =
      do
        let lPat = cmdInput x
        ap <- makeAllPicker cpat lPat
        let core = AppE (makeLambda lPat $ cmdBody x) ap
            splited = InfixE (Just core) (VarE spliter) (Just (VarE id'))
            cpat' = CPPair (cmdOutput x) cpat
        tail <- go cpat' xs spliter id' dot'
        return $ InfixE (Just tail) (VarE dot') (Just splited)
    go cpat [] _ _ _ =
      do
        runIO $ print cpat
        Just pck <- makePicker cpat CPTail
        return pck

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
        return $ InfixE (Just (VarE f')) (VarE dot') (Just exp)

makePicker _ _ = return Nothing

-- Test: make picker for every member
makeAllPicker :: CPat -> CPat -> Q Exp
makeAllPicker pat0 pat = go pat0 pat
  where
    go x (CPPair y1 y2) =
      do
        r1 <- go x y1
        r2 <- go x y2
        return $ TupE [r1, r2]

    go x CPUnnamed = return $ VarE 'undefined

    go x CPUnit = return $ ConE '()

    go x y =
      do
        mr <- makePicker x y
        maybe (error $ "makeAllPicker fail") return mr
