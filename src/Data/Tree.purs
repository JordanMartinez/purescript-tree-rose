module Data.Tree where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), snoc, (:))
import Data.Monoid (power)
import Data.Traversable (class Traversable, sequenceDefault, traverse)

-- | A Rose, or multi-way tree, with values of type `a`.
newtype Tree a = Node { value :: a
                      , children :: Forest a 
                      }

-- | A `Forest` is a list of `Tree`s.
type Forest a = List (Tree a)

-- | Create a `Tree` from a `Node` value of type `a` and a `Forest` of children.
mkTree :: forall a. a -> Forest a -> Tree a
mkTree a cs = Node { value: a, children: cs }

infix 5 mkTree as |>

-- Instances

derive instance genericTree :: Generic a => Generic (Tree a)

instance eqTree :: Generic a => Eq (Tree a) where
  eq = gEq

instance showTree' :: Generic a => Show (Tree a) where
  show = gShow

instance functorTree :: Functor Tree where
  map f (Node {value:v, children: c}) = mkTree (f v) (tailRec go {current: c, result: Nil})
    where       
      go {current: Nil, result: r} = Done r
      go {current: c:cs, result: r} = 
        Loop {current: cs, result: snoc r (mkTree (f (nodeValue c)) (tailRec go {current: (nodeChildren c), result: Nil })) }

instance applyTree :: Apply Tree where
  apply (Node {value: f, children: fs}) tx@(Node {value: x, children: xs})
    = Node {value: (f x), children: ((map <<< map) f xs) <> (map ((flip apply) tx) fs)}

instance applicativeTree :: Applicative Tree where
  pure = (flip $ mkTree) Nil

instance bindTree :: Bind Tree where
  bind tx@(Node {value: x, children: xs}) f = 
    let (Node {value: b, children: bs}) = f x in
    Node {value: b, children: bs <> (map ((flip bind) f) xs)}

instance monadTree :: Monad Tree

instance foldableTree :: Foldable Tree where
  foldMap f (Node {value: x, children: xs}) = (f x) <> (foldMap (foldMap f) xs)
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance traversableTree :: Traversable Tree where
  traverse f (Node {value: x, children: xs}) =
    lift2 (\a b -> Node {value: a, children: b}) (f x) (traverse (traverse f) xs)

  sequence x = sequenceDefault x  

-- Other functions

-- | Draw a 2D `String` representation of a `Tree String`.
drawTree :: Tree String -> String
drawTree t = tailRec go {level: 0, drawn: (nodeValue t) <> "\n", current: (nodeChildren t)}
  where
    go :: _ -> Step _ String
    go {level: l, drawn: s, current: Nil} = Done s
    go {level: l, drawn: s, current: c:cs } = 
      let drawn = (power "       " l) <> "|----> " <> (nodeValue c) <> "\n" in
      Loop {level: l, drawn: s <> drawn <> (tailRec go {level: l + 1, drawn: "", current: (nodeChildren c)})  , current: cs}

-- | Draw a 2D `String`  representation of a `Tree` composed of `Show`able
-- | elements.
showTree :: forall a. Show a => Tree a -> String
showTree = drawTree <<< (map show)

-- | Scan a `Tree`, accumulating values of `b` there are constant across `Node`s
-- | that have the same parent.
scanTree :: forall a b. (a -> b -> b) -> b -> Tree a -> Tree b
scanTree f b n@(Node {value: x, children: xs}) = 
  let fb = f x b 
  in fb |> (tailRec go {b: fb, current: xs, final: Nil})
  where
    go :: _ -> Step _ (Forest b)
    go {b: b', current: Nil, final: final} = Done final
    go {b: b', current: c:cs, final: final} = 
      let fb' = f (nodeValue c) b' 
      in Loop {b: b', current: cs, final: snoc final (fb' |> tailRec go {b: fb', current: (nodeChildren c), final: Nil})}

-- Setters and getters
nodeValue :: forall a. Tree a -> a 
nodeValue (Node r) = r.value

nodeChildren :: forall a. Tree a -> Forest a
nodeChildren (Node r) = r.children

setNodeValue :: forall a. a -> Tree a -> Tree a
setNodeValue a (Node r) = Node r { value = a }

modifyNodeValue :: forall a. (a -> a) -> Tree a -> Tree a
modifyNodeValue f (Node r) = Node r { value = f r.value }

appendChild :: forall a. Tree a  -> Tree a -> Tree a
appendChild n (Node r) = Node r { children = snoc r.children n }

setChildren :: forall a. Forest a -> Tree a -> Tree a 
setChildren cs (Node r) = Node r { children = cs }
