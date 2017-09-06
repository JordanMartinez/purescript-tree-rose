module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(..), (:))
import Data.List.Partial (head)
import Data.Tree (Tree, mkTree, nodeChildren, nodeValue, scanTree, (|>))
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

sampleTree :: Tree Int
sampleTree = 
  1 |> 
      (2 |> Nil)
    : (3 |> Nil)
    : (4 |>
          (5 |> Nil)
        : (6 |> 
            (7 |> Nil) : Nil)
        : (8 |> Nil)
        : Nil
      )
    : Nil

main :: forall e. Eff (RunnerEffects e) Unit
main = run [consoleReporter] do 
  describe "Tree" do    

    it "mkTree" do
      let t = mkTree 10 Nil
      shouldEqual (nodeValue t) 10

    it "Functor" do
      let result = 
              2 |> 
                  (3 |> Nil)
                : (4 |> Nil)
                : (5 |>
                      (6 |> Nil)
                    : (7 |> 
                        (8 |> Nil) : Nil)
                    : (9 |> Nil)
                    : Nil
                  )
                : Nil
      shouldEqual (((+)1) <$> sampleTree) result

    it "scanTree" do
      let result = 
              1 |> 
                  (3 |> Nil)
                : (4 |> Nil)
                : (5 |>
                      (10 |> Nil)
                    : (11 |> 
                        (18 |> Nil) : Nil)
                    : (13 |> Nil)
                    : Nil
                  )
                : Nil
      shouldEqual (scanTree (\a b -> a + b) 0 sampleTree) result