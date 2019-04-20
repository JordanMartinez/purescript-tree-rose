module Data.Tree.Zipper where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (head, tail, (:<))
import Data.List (List(Nil), drop, reverse, take, (!!), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tree (Forest, Tree, mkTree, modifyNodeValue, setNodeValue)

-- | The `Loc` type describes the location of a `Node` inside a `Tree`. For this
-- | we store the current `Node`, the sibling nodes that appear before the current
-- | node, the sibling nodes that appear after the current node, and a `List` of
-- | `Loc`ations that store the parent node locations up to the root of the three.
-- |
-- | So, effectively, the `parents` field records the path travelled in the
-- | tree to reach the level of the current `Node` starting from the tree's root,
-- | and the `before` and `after` fields describe its location in the current
-- | level.
newtype Loc a = Loc { node :: Tree a
                    , before :: Forest a
                    , after :: Forest a
                    , parents :: List (Loc a)
                    }

derive newtype instance eqLoc :: Eq a => Eq (Loc a)

-- -- Cursor movement

-- | Move the cursor to the next sibling.
next :: forall a. Loc a -> Maybe (Loc a)
next (Loc r) =
  case r.after of
    Nil -> Nothing
    (c:cs) -> Just $ Loc r { node = c
                           , before = r.node : r.before
                           , after = cs
                           }

-- -- | Move the cursor to the previous sibling.
prev :: forall a. Loc a -> Maybe (Loc a)
prev (Loc r) =
  case r.before of
    Nil -> Nothing
    (c:cs) -> Just $ Loc r { node = c
                           , before = cs
                           , after = r.node : r.after
                           }

-- -- | Move the cursor to the first sibling.
first :: forall a. Loc a -> Loc a
first l@(Loc r) =
  case reverse r.before of
    Nil -> l
    c:cs -> Loc r { node = c
                  , before = Nil
                  , after = cs <> r.after
                  }

-- -- | Move the cursor to the last sibling.
last :: forall a. Loc a -> Loc a
last l@(Loc r) =
  case reverse r.after of
    Nil -> l
    c:cs -> Loc r { node = c
                  , before = cs <> (r.node : r.before)
                  , after = Nil
                  }

-- -- | Move the cursor to the parent `Node`.
up :: forall a. Loc a -> Maybe (Loc a)
up l@(Loc r) =
  case r.parents of
    Nil -> Nothing
    (p:ps) -> Just $ Loc { node: (value p) :< (siblings l)
                         , before: before p
                         , after: after p
                         , parents: ps
                         }

-- | Move the cursor to the root of the tree.
root :: forall a. Loc a -> Loc a
root l =
  case up l of
    Nothing -> l
    Just p -> root p

-- | Move the cursor to the first child of the current `Node`.
firstChild :: forall a. Loc a -> Maybe (Loc a)
firstChild n =
  case children n of
    Nil -> Nothing
    c:cs ->
      Just $ Loc { node: c
                 , before: Nil
                 , after: cs
                 , parents: n : (parents n)
                 }

-- | Move the cursor to the first child of the current `Node`.
down :: forall a. Loc a -> Maybe (Loc a)
down = firstChild

-- | Move the cursor to the last child of the current `Node`.
lastChild :: forall a. Loc a -> Maybe (Loc a)
lastChild n =
  case reverse (children n) of
    Nil -> Nothing
    c:cs ->
      Just $ Loc { node: c
                 , before: cs
                 , after: Nil
                 , parents: n : (parents n)
                 }

-- | Move the cursor to a specific sibling by it's index.
siblingAt :: forall a. Int -> Loc a -> Maybe (Loc a)
siblingAt i l@(Loc r) = do
  p@(Loc r') <- up l
  c <- (children p) !! i
  let before' = reverse $ take i (children p)
  let after' = drop (i+1) (children p)
  pure $ Loc { node: c
             , before: before'
             , after: after'
             , parents: r.parents
             }

-- | Move the cursor to a specific child of the current `Node` by it's index.
childAt :: forall a. Int -> Loc a -> Maybe (Loc a)
childAt i p = (firstChild p) >>= (siblingAt i)


-- | Retrieve the `Tree` representation, i.e., returns the root `Node` of the
-- | current tree.
toTree :: forall a. Loc a -> Tree a
toTree = node <<< root

-- | Get a `Loc`ation representation from a given `Tree`.
fromTree :: forall a. Tree a -> Loc a
fromTree n = Loc { node: n
                 , before: Nil
                 , after: Nil
                 , parents: Nil
                 }

-- | Set the `Node` at the current position.
setNode :: forall a. Tree a -> Loc a -> Loc a
setNode a (Loc r) = Loc r { node = a }

-- | Set the `Node` at the current position.
modifyNode :: forall a. (Tree a -> Tree a) -> Loc a -> Loc a
modifyNode f (Loc r) = Loc r { node = f r.node }

-- | Set the value of the current `Node`.
setValue :: forall a. a -> Loc a -> Loc a
setValue a l = setNode (setNodeValue a (node l)) l

-- | Modify the value of the current `Node`.
modifyValue :: forall a. (a -> a) -> Loc a -> Loc a
modifyValue f l = setNode (modifyNodeValue f (node l)) l

-- -- insert and delete nodes

-- | Insert a node after the current position, and move cursor to the new node.
insertAfter :: forall a. Tree a -> Loc a -> Loc a
insertAfter n (Loc r) = Loc r { node = n
                              , before = r.node : r.before
                              }

-- | Insert a node before the current position, and move cursor to the new node.
insertBefore :: forall a. Tree a -> Loc a -> Loc a
insertBefore n (Loc r) = Loc r { node = n
                               , after = r.node : r.after
                               }

-- | Insert a node as a child to  the current node, and move cursor to the new node.
insertChild :: forall a. Tree a -> Loc a -> Loc a
insertChild n l =
  case down l of
    Just c -> insertAfter n c
    Nothing -> Loc { node: n
                   , after: Nil
                   , before: Nil
                   , parents: l : (parents l)
                   }

-- | Delete the node in the current position.
delete :: forall a. Loc a -> Loc a
delete l@(Loc r) =
  case r.after of
    c:cs -> Loc r { node = c
                  , after = cs
                  }
    Nil ->
      case r.before of
        c:cs -> Loc r { node = c
                      , before = cs
                      }
        Nil ->
          case r.parents of
            Nil -> l
            c:cs -> Loc { node: mkTree (value c) Nil
                        , before: before c
                        , after: after c
                        , parents: parents c
                        }

-- Searches

 -- | Search down and to the right for the first occurence where the given predicate is true and return the Loc
findDownWhere :: ∀ a. (a -> Boolean) -> Loc a -> Maybe (Loc a)
findDownWhere predicate loc | predicate $ value loc = Just loc
findDownWhere predicate loc = lookNext <|> lookDown
  where
    lookNext = next loc >>= findDownWhere predicate
    lookDown = down loc >>= findDownWhere predicate

-- | Search for the first occurence of the value `a` downwards and to the right.
findDown :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findDown a = findDownWhere ( _ == a)

-- | Search to the left and up for the first occurence where the given predicate is true and return the Loc
findUpWhere :: ∀ a. (a -> Boolean) -> Loc a -> Maybe (Loc a)
findUpWhere predicate loc | predicate $ value loc = Just loc
findUpWhere predicate loc = lookPrev <|> lookUp
  where
    lookPrev  = prev loc  >>= findUpWhere predicate
    lookUp    = up loc    >>= findUpWhere predicate

-- | Search for the first occurence of the value `a` upwards and to the left,
findUp :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findUp a = findUpWhere (_ == a)

-- | Search from the root of the tree for the first occurrence where the given predicate is truen and return the Loc
findFromRootWhere :: ∀ a. (a -> Boolean) -> Loc a -> Maybe (Loc a)
findFromRootWhere predicate loc | predicate $ value loc = Just loc
findFromRootWhere predicate loc = findDownWhere predicate $ root loc

-- | Search for the first occurence of the value `a` starting from the root of
-- | the tree.
findFromRoot :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findFromRoot a = findFromRootWhere (_ == a)

-- | flattens the Tree into a List depth first.
flattenLocDepthFirst :: ∀ a. Loc a -> List (Loc a)
flattenLocDepthFirst loc = loc : (go loc)
  where
    go :: Loc a -> List (Loc a)
    go loc' =
      let
        downs = goDir loc' down
        nexts = goDir loc' next
      in
        downs <> nexts

    goDir :: Loc a -> (Loc a -> Maybe (Loc a)) -> List (Loc a)
    goDir loc' dirFn = case (dirFn loc') of
      Just l  -> l : go l
      Nothing -> Nil



-- Setters and Getters
node :: forall a. Loc a -> Tree a
node (Loc r) = r.node

value :: forall a. Loc a -> a
value = head <<< node

before :: forall a. Loc a -> Forest a
before (Loc r) = r.before

after :: forall a. Loc a -> Forest a
after (Loc r) = r.after

parents :: forall a. Loc a -> List (Loc a)
parents (Loc r) = r.parents

children :: forall a. Loc a -> Forest a
children = tail <<< node

siblings :: forall a. Loc a -> Forest a
siblings (Loc r) = (reverse r.before) <> (r.node : r.after)
