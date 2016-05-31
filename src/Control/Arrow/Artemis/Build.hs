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
import Data.Witherable
import Control.Monad.State.Lazy

buildArrow :: ArrowExp -> Q Exp
buildArrow aexp =
  do
    let gPat = expInput aexp
    cmds <- precombineCommands (expBody aexp)
    -- runIO $ print cmds
    concatCommand gPat cmds


precombineCommands :: [Command] -> Q [Command]
precombineCommands xs0 =
  do
    parallel <- lookupMember "&&&"
    let (xs', cont) = runState (go parallel xs0) False
    if not cont then return xs' else precombineCommands xs'
  where
    go :: Name -> [Command] -> State Bool [Command]
    go parallel (x1:x2:xs)
        | not (hasIntersection x1 x2) =
          do
            put True
            let xnew = undefined --combine parallel x1 x2
            liftM (xnew:) $ go parallel xs
        | otherwise =
            liftM ((x1:).(x2:)) $ go parallel xs
    go _ xs =
        return xs

    hasIntersection _ _ = True

{-
    hasIntersection' x y =
        let
            xOut = Set.fromList $ Fd.toList (cmdOutput x)
            yIn = Set.fromList $ Fd.toList (cmdInput y)
          in
            not $ Set.null $ Set.intersection xOut yIn

    combine parallel x y =
        Command {
            cmdInput = CPPair (cmdInput x) (cmdInput y),
            cmdOutput = CPPair (cmdOutput x) (cmdOutput y),
            cmdBody = InfixE (Just (cmdBody x)) (VarE parallel) (Just (cmdBody y))
          }
-}

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
        (splited, cpat') <- single cpat x (cmdBody x) spliter id' dot'
        tail <- go cpat' xs spliter id' dot'
        return $ InfixE (Just tail) (VarE dot') (Just splited)

    go cpat [] _ _ _ =
      do
        -- runIO $ print cpat
        Just pck <- makePicker cpat CPTail
        return pck

    single cpat x (Right bodyExp) spliter id' dot' =
      do
        let lPat = setToCPat $ cmdInput x
        ap <- makeAllPicker cpat lPat
        let core = AppE (makeLambda lPat bodyExp) ap
            splited = InfixE (Just core) (VarE spliter) (Just (VarE id'))
            cpat' = CPPair (cmdOutput x) cpat
        return (splited, cpat')

    single cpat x (Left decls) splitter id' dot' =
      do
        runIO $ print x
        arr <- lookupMember "arr"
        let onlyContain name =
                if Set.member name (cmdInput x) then Just name else Nothing
            inPat = cpatToPat $ mapMaybe onlyContain cpat
            outExp = cpatToExp $ cmdOutput x
            core = AppE (VarE arr) $ LamE [inPat] $ LetE decls $ outExp
            splitted = InfixE (Just core) (VarE splitter) (Just (VarE id'))
            cpat' = CPPair (cmdOutput x) cpat
        return $ (splitted, cpat')

-- Lookup name, failing is error
lookupMember :: String -> Q Name
lookupMember s =
  do
    mx <- lookupValueName s
    maybe (fail $ "No function named " ++ s) return mx


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
        (Just _, Just _) -> fail "Internal error: Patter redundant"
        (Just r1, Nothing) -> append "fst" r1 >>= return . Just
        (Nothing, Just r2) -> append "snd" r2 >>= return . Just
        _ -> return Nothing
  where
    append s exp =
      do
        dot' <- lookupMember "."
        f' <- lookupMember s
        return $ InfixE (Just exp) (VarE dot') (Just (VarE f'))

makePicker _ _ = return Nothing

-- Test: make picker for every member
makeAllPicker :: CPat -> CPat -> Q Exp
makeAllPicker pat0 pat =
  do
    r <- go pat0 pat
    -- runIO $ putStrLn $ show pat0 ++ ", " ++ show pat ++ " -> " ++ pprint r
    return r
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
        maybe (fail "makeAllPicker fail") return mr
