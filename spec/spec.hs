{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}

module
    Main
where

import Test.Hspec

import Prelude ()
import Control.Arrow.Artemis.CompatPrelude
import Control.Category
import Control.Arrow
import Control.Applicative

import Control.Arrow.Artemis


--
-- Local definitions
--
instance
    Monad m => Functor (Kleisli m a)
  where
    fmap f x = x >>> arr f

instance
    Monad m => Applicative (Kleisli m a)
  where
    pure = arr . const
    f <*> g = f &&& g >>> arr (uncurry ($))


--
-- Test
--
main = hspec $
  do
    basics

basics =
  do
    describe "The new arrow notation" $ 
      do
        it "may looks like a normal haskell expression." $
          do
            let ar = [proc| (x, y) -> x |]
                -- By -XArrows : proc (x, _) -> returnA -< x

            r <- runKleisli ar (1, 2)
            r `shouldBe` (1::Int)

        it "can execute side effects by (<<<)" $
         --"Because formal arguments and expression bodies have arrow types."
          do
            let ar = [proc| x -> Kleisli (`shouldBe` 2) <<< x |]
                -- By -XArrows : proc x -> Kleisli (`shouldBe` 2) -< x

            runKleisli ar 2

        it "can do calculations between variables by Applicative I/F." $
         --"If the type doesn't have Applicative instance, you can still do it using WrappedArrow."
         --"But it's much easier to define Applicative instances as this example."
          do
            let ar = [proc| (x, y) -> liftA2 (+) x y |]
                -- By -XArrows : proc (x, y) -> returnA -< x + y

            r <- runKleisli ar (1, 2)
            r `shouldBe` (3::Int)

        it "can make vertical compositions by \'do\'" $
         --"Because formal arguments and expression bodies have arrow types."
          do
            let ar = [proc| (x, y) ->
                  do
                    z <- liftA2 (+) x y
                    Kleisli (`shouldBe` 3) <<< z
                  |]
                -- By -XArrows : proc (x, y) ->
                --   do
                --     z <- returnA -< x + y
                --     Kleisli (`shouldBe` 3) -< z

            runKleisli ar (1, 2)

{-
        it "doesn't allow tuples of size >= 3 due to the Cartesian class definition." $
          do
            [proc_fail| (x, y, z) -> x |] `shouldBe` True
-}
