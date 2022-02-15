module Data.Tree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.List (List(..), snoc, (:))
import Data.Monoid (power)
import Data.Traversable (Accum)

-- | A Rose, or multi-way tree, with values of type `a`. To access the
-- | root of the Tree's value, use `Control.Comonad.Cofree (head)`. To
-- | access the root's children, use `Control.Comonad.Cofree (tail)`
type Tree a = Cofree List a

-- | A type alias for the children of a Tree's root value.
type Forest a = List (Tree a)

-- | Create a `Tree` from a `Node` value of type `a` and a `Forest` of children.
mkTree :: forall a. a -> Forest a -> Tree a
mkTree = mkCofree

-- | Draw a 2D `String` representation of a `Tree String`.
drawTree :: Tree String -> String
drawTree = drawTree' 0

-- | Draw a 2D `String` representation of a `Tree String`,
-- | starting the indent at the given level
drawTree' :: Int -> Tree String -> String
drawTree' level t = do
  let
    treeRoot = (power "       " level) <> "|----> " <> (head t) <> "\n"
    treeChildren = drawForest' (level + 1) (tail t)
  treeRoot <> treeChildren

-- | Draw a 2D `String` representation of a `Forest String`,
drawForest :: Forest String -> String
drawForest = drawForest' 0

-- | Draw a 2D `String` representation of a `Forest String`,
-- | starting the indent at the given level
drawForest' :: Int -> Forest String -> String
drawForest' level forest = tailRec goForest { level: level, drawn: "", current: forest }
  where
  goForest
    :: { current :: Forest String, drawn :: String, level :: Int }
    -> Step { current :: Forest String, drawn :: String, level :: Int } String
  goForest { drawn: s, current: Nil } = Done s
  goForest { level: l, drawn: s, current: c : cs } = do
    let
      drawnTree = drawTree' l c
    Loop { level: l, drawn: s <> drawnTree, current: cs }

-- | Draw a 2D `String`  representation of a `Tree` composed of `Show`able
-- | elements.
showTree :: forall a. Show a => Tree a -> String
showTree tree = drawTree (show <$> tree)

-- | Draw a 2D `String`  representation of a `Forest` composed of `Show`able
-- | elements.
showForest :: forall a. Show a => Forest a -> String
showForest forest = drawForest ((\tree -> show <$> tree) <$> forest)

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent.
scanTree :: forall a b. (a -> b -> b) -> b -> Tree a -> Tree b
scanTree f b n = do
  let
    fb = f (head n) b
  fb :< (tailRec go { b: fb, current: (tail n), final: Nil })
  where
  go :: { final :: Forest b, current :: Forest a, b :: b } -> Step { final :: Forest b, current :: Forest a, b :: b } (Forest b)
  go { current: Nil, final: final } = Done final
  go { b: b', current: c : cs, final: final } = do
    let
      fb' = f (head c) b'
    Loop { b: b', current: cs, final: snoc final (fb' :< tailRec go { b: fb', current: (tail c), final: Nil }) }

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent, and returning a `Tree` of type `c`.
scanTreeAccum :: forall a b c. (a -> b -> Accum b c) -> b -> Tree a -> Tree c
scanTreeAccum f b n = do
  let
    fb = f (head n) b
  fb.value :< (tailRec go { b: fb.accum, current: (tail n), final: Nil })
  where
  go :: { final :: Forest c, current :: Forest a, b :: b } -> Step { final :: Forest c, current :: Forest a, b :: b } (Forest c)
  go { current: Nil, final: final } = Done final
  go { b: b', current: c : cs, final: final } = do
    let
      fb' = f (head c) b'
    Loop { b: b', current: cs, final: snoc final (fb'.value :< tailRec go { b: fb'.accum, current: (tail c), final: Nil }) }

-- | Set the value of a node.
setNodeValue :: forall a. a -> Tree a -> Tree a
setNodeValue a n = a :< (tail n)

-- | Modify the value of a node.
modifyNodeValue :: forall a. (a -> a) -> Tree a -> Tree a
modifyNodeValue f n = f (head n) :< tail n

-- | Append a child to a node.
appendChild :: forall a. Tree a -> Tree a -> Tree a
appendChild c n = head n :< snoc (tail n) c
