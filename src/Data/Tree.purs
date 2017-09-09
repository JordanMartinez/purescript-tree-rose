module Data.Tree where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree, tail, (:<))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Generic (class Generic)
import Data.List (List(..), snoc, (:))
import Data.Monoid (power)
import Data.Traversable (Accum)

-- | A Rose, or multi-way tree, with values of type `a`.
type Tree a = Cofree List a

type Forest a = List (Tree a)

-- | Create a `Tree` from a `Node` value of type `a` and a `Forest` of children.
mkTree :: forall a. a -> Forest a -> Tree a
mkTree = mkCofree

-- | Draw a 2D `String` representation of a `Tree String`.
drawTree :: Tree String -> String
drawTree t = tailRec go {level: 0, drawn: (head t) <> "\n", current: (tail t)}
  where
    go :: _ -> Step _ String
    go {level: l, drawn: s, current: Nil} = Done s
    go {level: l, drawn: s, current: c:cs } = 
      let drawn = (power "       " l) <> "|----> " <> (head c) <> "\n" in
      Loop {level: l, drawn: s <> drawn <> (tailRec go {level: l + 1, drawn: "", current: (tail c)})  , current: cs}

-- | Draw a 2D `String`  representation of a `Tree` composed of `Show`able
-- | elements.
showTree :: forall a. Show a => Tree a -> String
showTree = drawTree <<< (map show)

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent.
scanTree :: forall a b. (a -> b -> b) -> b -> Tree a -> Tree b
scanTree f b n = 
  let fb = f (head n) b 
  in fb :< (tailRec go {b: fb, current: (tail n), final: Nil})
  where
    go :: _ -> Step _ (Forest b)
    go {b: b', current: Nil, final: final} = Done final
    go {b: b', current: c:cs, final: final} = 
      let fb' = f (head c) b' 
      in Loop {b: b', current: cs, final: snoc final (fb' :< tailRec go {b: fb', current: (tail c), final: Nil})}

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent, and returning a `Tree` of type `c`.
scanTreeAccum :: forall a b c. (a -> b -> Accum b c) -> b -> Tree a -> Tree c
scanTreeAccum f b n = 
  let fb = f (head n) b 
  in fb.value :< (tailRec go {b: fb.accum , current: (tail n), final: Nil})
  where
    go :: _ -> Step _ (Forest c)
    go {b: b', current: Nil, final: final} = Done final
    go {b: b', current: c:cs, final: final} = 
      let fb' = f (head c) b' 
      in Loop {b: b', current: cs, final: snoc final (fb'.value :< tailRec go {b: fb'.accum, current: (tail c), final: Nil})}

-- | Set the value of a node.
setNodeValue :: forall a. a -> Tree a -> Tree a
setNodeValue a n = a :< (tail n)

-- | Modify the value of a node.
modifyNodeValue :: forall a. (a -> a) -> Tree a -> Tree a
modifyNodeValue f n = f (head n) :< tail n

-- | Append a child to a node.
appendChild :: forall a. Tree a  -> Tree a -> Tree a
appendChild c n = head n :< snoc (tail n) c
