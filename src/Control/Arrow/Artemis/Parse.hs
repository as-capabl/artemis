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

makeArrowExp pat body =
    let
        compBody = "\\" ++ show pat ++ " ->" ++ body
        metaResult = Meta.parseExp compBody
        cmds exp = [testcom exp] --stab
        testcom exp = Command {
            cmdInput = pat,
            cmdOutput = CPTail,
            cmdBody = exp
          }
      in
        case metaResult
          of
            Left err -> Left err
            Right exp -> Right $
                ArrowExp {expInput = pat, expBody = cmds exp}
