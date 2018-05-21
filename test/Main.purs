module Test.Main where

import Prelude

import Control.Comonad.Cofree (head, (:<))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (log)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tree (Tree, mkTree, scanTree, showTree)
import Data.Tree.Zipper (down, findDownWhere, findFromRoot, findUp, flattenLocDepthFirst, fromTree, insertAfter, modifyValue, next, toTree, value)
import Debug.Trace (spy)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

sampleTree :: Tree Int
sampleTree = 
  1 :< 
      (2 :< Nil)
    : (3 :< Nil)
    : (4 :<
          (5 :< Nil)
        : (6 :< 
            (7 :< Nil) : Nil)
        : (8 :< Nil)
        : Nil
      )
    : Nil

main :: forall e. Eff (RunnerEffects e) Unit
main = run [consoleReporter] do 
  describe "Tree" do    

    it "mkTree" do
      let t = mkTree 10 Nil
      shouldEqual (head t) 10

    it "Functor" do
      let result = 
              2 :< 
                  (3 :< Nil)
                : (4 :< Nil)
                : (5 :<
                      (6 :< Nil)
                    : (7 :< 
                        (8 :< Nil) : Nil)
                    : (9 :< Nil)
                    : Nil
                  )
                : Nil
      shouldEqual (eq (((+)1) <$> sampleTree) result) true

    it "scanTree" do
      let result = 
              1 :< 
                  (3 :< Nil)
                : (4 :< Nil)
                : (5 :<
                      (10 :< Nil)
                    : (11 :< 
                        (18 :< Nil) : Nil)
                    : (13 :< Nil)
                    : Nil
                  )
                : Nil
      shouldEqual (eq (scanTree (\a b -> a + b) 0 sampleTree) result) true
  
  describe "Zipper" do

    let root = fromTree sampleTree

    it "Modify" do

      let root' = unsafePartial $ toTree $ modifyValue (\a -> 2 * a) (fromJust $ down root)      
      let root'' = unsafePartial $ toTree $ modifyValue (\a -> 2 * a) (fromJust $ down >=> next >=> next >=> down $ root) 
      let result =
              1 :< 
                  (4 :< Nil)
                : (3 :< Nil)
                : (4 :<
                      (5 :< Nil)
                    : (6 :< 
                        (7 :< Nil) : Nil)
                    : (8 :< Nil)
                    : Nil
                  )
                : Nil

      let result' =
              1 :< 
                  (2 :< Nil)
                : (3 :< Nil)
                : (4 :<
                      (10 :< Nil)
                    : (6 :< 
                        (7 :< Nil) : Nil)
                    : (8 :< Nil)
                    : Nil
                  )
                : Nil

      shouldEqual (eq root' result) true
      shouldEqual (eq root'' result') true
    
    it "Insert" do

      let root' = unsafePartial $ toTree $ insertAfter (mkTree 100 Nil) (fromJust $ down root)
      let root'' = unsafePartial $ toTree $ insertAfter (mkTree 100 Nil) (fromJust $ (down root) >>= next >>= next >>= down >>= next >>= down)             

      let result =
              1 :< 
                  (2 :< Nil)
                : (100 :< Nil)
                : (3 :< Nil)
                : (4 :<
                      (5 :< Nil)
                    : (6 :< 
                        (7 :< Nil) : Nil)
                    : (8 :< Nil)
                    : Nil
                  )
                : Nil

      let result' =
              1 :< 
                  (2 :< Nil)
                : (3 :< Nil)
                : (4 :<
                      (5 :< Nil)
                    : (6 :< 
                          (7 :< Nil) 
                        : (100 :< Nil)
                        : Nil)
                    : (8 :< Nil)
                    : Nil
                  )
                : Nil
      shouldEqual (eq root' result) true
      shouldEqual (eq root'' result') true

    it "Should findDownWhere with single node" do
      let tree = 1 :< Nil
      let loc = fromTree tree
      shouldEqual (Just 1) ((findDownWhere (_ == 1) loc) <#> value)

    it "Should findDownWhere with 2 nodes and 2 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 2) ((findDownWhere (_ == 2) loc) <#> value)

    it "Should findDownWhere with 3 nodes and 2 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 3) ((findDownWhere (_ == 3) loc) <#> value)

    it "Should findDownWhere with 4 nodes and 2 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 4) ((findDownWhere (_ == 4) loc) <#> value)

    it "Should findDownWhere with 5 nodes and 3 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< 
                          (5 :< Nil)
                        : Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 5) ((findDownWhere (_ == 5) loc) <#> value)

    it "Should findDownWhere with 6 nodes and 3 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< 
                          (5 :< Nil)
                        : (6 :< Nil)
                        : Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 6) ((findDownWhere (_ == 6) loc) <#> value)

    it "Should findDownWhere with 7 nodes and 4 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< 
                          (5 :< Nil)
                        : (6 :< 
                              (7 :< Nil) : Nil)
                        : Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 7) ((findDownWhere (_ == 7) loc) <#> value)

    it "Should findDownWhere with 8 nodes and 4 levels" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< 
                          (5 :< Nil)
                        : (6 :< 
                              (7 :< Nil) : Nil)
                        : (8 :< Nil)
                        : Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 8) ((findDownWhere (_ == 8) loc) <#> value)

    it "Should findDownWhere with 8 nodes and 4 levels with a step back" do
      let tree = 1 :< 
                      (2 :< Nil) 
                    : (3 :< Nil) 
                    : (4 :< 
                          (5 :< Nil)
                        : (6 :< 
                              (7 :< Nil) : Nil)
                        : (8 :< Nil)
                        : Nil) 
                      : Nil
      -- log $ showTree tree
      let loc = fromTree tree
      shouldEqual (Just 7) ((findDownWhere (_ == 7) loc) <#> value)

    it "Should find 7 from the sampleTree" do
      shouldEqual (Just 7) (findDownWhere (_ == 7) (fromTree sampleTree) <#> value)

    it "Should find 8 from the sampleTree (the bottom) and then find 1 (the top) with findUp" do
      let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
      shouldEqual (Just 1) (findUp 1 eight <#> value)

    it "Should find 8 from the sampleTree (the bottom) but then not find 7 because it would require a downward traversal" do
      let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
      shouldEqual Nothing (findUp 7 eight <#> value)

    it "Should find 8 from the sampleTree (the bottom) and then find 7 using findFromRoot" do
      let eight = unsafePartial $ fromJust $ findDownWhere (_ == 8) $ fromTree sampleTree
      shouldEqual (Just 7) (findFromRoot 7 eight <#> value)

    it "Should flatten the Tree into a list of locations following a depth first pattern" do
      let flat = map value $ flattenLocDepthFirst $ fromTree sampleTree
      -- log $ showTree sampleTree
      -- log $ show flat
      shouldEqual flat (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : Nil)



