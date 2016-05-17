{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module
    Control.Arrow.Artemis.Parse
      (
        parseArrowExp
      )
where

import Text.Parsec
import Text.Parsec.String
import Language.Haskell.TH
import qualified Language.Haskell.Meta as Meta

import Control.Arrow.Artemis.Type
import Control.Arrow.Artemis.Modification
import qualified Data.Set as Set
import qualified Data.Foldable as Fd

parseArrowExp ::
    String -> Either String ArrowExp
parseArrowExp s =
    either (Left . show) Right $
        runP arrowExp () "artemis"  s

arrowExp :: Parsec String () ArrowExp
arrowExp = --return $ ArrowExp {expInput = CPUnbound, expBody = []}
  do
    pat <- item
    spaces
    string "->"
    bodyStr <- many anyChar
    either unexpected return $ makeArrowExp pat bodyStr

item =
  do
    spaces
    do
        char '('
        spaces
        do
            -- ()
            char ')'
            return CPUnit
          <|> do
            -- some expression
            r <- cpat
            spaces
            char ')'
            return r
      <|> do
        unnamed
      <|> do
        ident

cpat =
  do
    spaces
    r1 <- item
    spaces
    do
        -- pair
        char ','
        spaces
        r2 <- item
        spaces
        do
            char ','
            unexpected "tuple length >= 3 (it's not allowed)"
          <|> do
            return $ CPPair r1 r2
     <|> do
        -- single element
        return r1

unnamed =
  do
    char '_'
    return CPUnnamed

ident =
  do
    h <- lower
    t <- many (alphaNum)
    return $ CPIdent (mkName (h:t))

makeArrowExp gPat bodyStr =
    let
        -- Parse haskell expression
        compBody = "\\" ++ show gPat ++ " ->" ++ bodyStr
        metaResult@(Right wholeExp) = Meta.parseExp compBody

        -- Decompose
        (patExp, bodyExp) = splitLambda wholeExp
        cpat = setToCPat $ listInputs (cpatToSet patExp) bodyExp
        resultAExp = ArrowExp {expInput = gPat, expBody = cmds cpat bodyExp}
      in
        case metaResult
          of
            Left err -> Left err
            Right _ -> Right resultAExp
  where
    cpatToSet = Set.fromList . Fd.toList

    cmds cpat (DoE xs) = decomposeBind (cpatToSet cpat) xs

    cmds cpat body = [Command {
         cmdInput = cpat,
         cmdOutput = CPTail,
         cmdBody = body
       }]

    decomposeBind varSet ((BindS pat exp):xs) =
        (Command {
            cmdInput = setToCPat $ listInputs varSet exp,
            cmdOutput = expToCPat pat,
            cmdBody = exp})
        : decomposeBind (varSet `Set.union` cpatToSet (expToCPat pat)) xs

    decomposeBind varSet ((NoBindS exp):xs) =
        (Command {
            cmdInput = setToCPat $ listInputs varSet exp,
            cmdOutput = if null xs then CPTail else CPUnnamed,
            cmdBody = exp})
        : decomposeBind varSet xs

    decomposeBind varSet [] = []
