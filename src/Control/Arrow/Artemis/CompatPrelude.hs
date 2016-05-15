module
    Control.Arrow.Artemis.CompatPrelude
      (
        module Prelude,
        id,
        (.),
        fst,
        snd
      )
where

import Prelude hiding (id, (.), fst, snd)
import qualified Prelude
import Control.Category
import Control.Arrow


fst ::
    Arrow a => a (b1, b2) b1
fst = arr Prelude.fst

snd ::
    Arrow a => a (b1, b2) b2
snd = arr Prelude.snd

