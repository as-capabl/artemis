{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module
    Control.Arrow.Artemis
      (
        proc,
        proc_fail
      )
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Control.Arrow.Artemis.Type as Ar
import qualified Control.Arrow.Artemis.Parse as Ar
import qualified Control.Arrow.Artemis.Build as Ar

proc :: QuasiQuoter
proc = QuasiQuoter {
    quoteExp = \s -> case Ar.parseArrowExp s
      of
        Left s -> error s
        Right exp -> Ar.buildArrow exp,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
  }

proc_fail = QuasiQuoter {
    quoteExp = \s -> quoteExp proc s >> return (VarE 'False) `recover` return (VarE 'True),
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
  }

