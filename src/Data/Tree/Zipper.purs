module Data.Tree.Zipper where

import Prelude

import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), drop, reverse, take, (!!), (:))
import Data.Maybe (Maybe(..))
import Data.Tree (Forest, Tree(..), mkTree, modifyNodeValue, nodeChildren, nodeValue, setNodeValue)

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


derive instance genericLoc :: Generic a => Generic (Loc a)

instance showLoc :: Generic a => Show (Loc a) where
  show = gShow

instance eqLoc :: Generic a => Eq (Loc a) where
  eq = gEq

-- Cursor movement

-- | Move the cursor to the next sibling.
next :: forall a. Loc a -> Maybe (Loc a)
next (Loc r) = 
  case r.after of
    Nil -> Nothing
    (c:cs) -> Just $ Loc { node: c
                         , before: r.node : r.before
                         , after: cs
                         , parents: r.parents
                         }

-- | Move the cursor to the previous sibling.
prev :: forall a. Loc a -> Maybe (Loc a)
prev (Loc r) =
  case r.before of
    Nil -> Nothing
    (c:cs) -> Just $ Loc { node: c 
                         , before: cs
                         , after: r.node : r.after
                         , parents: r.parents
                         }

-- | Move the cursor to the first sibling.
first :: forall a. Loc a -> Loc a
first l@(Loc r) = 
  case r.before of
    Nil -> l
    c:cs -> Loc $ { node: c
                  , before: Nil
                  , after: (reverse cs) <> r.after
                  , parents: r.parents
                  }

-- | Move the cursor to the last sibling.
last :: forall a. Loc a -> Loc a
last l@(Loc r) = 
  case reverse r.after of
    Nil -> l
    c:cs -> Loc $ { node: c
                  , before: cs <> r.before
                  , after: Nil
                  , parents: r.parents
                  }

-- | Move the cursor to the parent `Node`.
up :: forall a. Loc a -> Maybe (Loc a)
up l@(Loc r) = 
  case r.parents of
    Nil -> Nothing
    (p:ps) -> Just $ Loc { node: mkTree (value p) (siblings l)
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
firstChild p@(Loc r) = 
  case r.node of
    Node { value: _, children: Nil } -> Nothing
    Node { value: _, children: c:cs } -> 
      Just $ Loc { node: c
                 , before: Nil
                 , after: cs
                 , parents: p : r.parents
                 } 

-- | Move the cursor to the first child of the current `Node`.
down :: forall a. Loc a -> Maybe (Loc a)
down = firstChild

-- | Move the cursor to the last child of the current `Node`.
lastChild :: forall a. Loc a -> Maybe (Loc a)
lastChild p@(Loc r) =  last <$> down p

-- | Move the cursor to a specific sibling by it's index.
siblingAt :: forall a. Int -> Loc a -> Maybe (Loc a)
siblingAt i l@(Loc r) =
  case up l of
    Nothing -> Nothing
    Just p@(Loc r') -> 
      case (children p) !! i of
        Nothing -> Nothing
        Just c -> 
          let before = reverse $ take i (children p)
              after = drop (i+1) (children p)
          in Just $ Loc { node: c
                        , before: before 
                        , after: after
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
setNode a (Loc r) = Loc { node: a 
                        , before: r.before
                        , after: r.after
                        , parents: r.parents
                        }

-- | Set the `Node` at the current position.
modifyNode :: forall a. (Tree a -> Tree a) -> Loc a -> Loc a
modifyNode f (Loc r) = Loc { node: f r.node
                           , before: r.before
                           , after: r.after
                           , parents: r.parents
                           }

-- | Set the value of the current `Node`.
setValue :: forall a. a -> Loc a -> Loc a 
setValue a l = setNode (setNodeValue a (node l)) l

-- | Modify the value of the current `Node`.
modifyValue :: forall a. (a -> a) -> Loc a -> Loc a
modifyValue f l = setNode (modifyNodeValue f (node l)) l

-- insert and delete nodes

-- | Insert a node after the current position, and move cursor to the new node.
insertAfter :: forall a. Tree a -> Loc a -> Loc a
insertAfter n l = Loc { node: n 
                      , after: after l
                      , before: (node l) : (before l)
                      , parents: parents l
                      }

-- | Insert a node before the current position, and move cursor to the new node.
insertBefore :: forall a. Tree a -> Loc a -> Loc a
insertBefore n l = Loc { node: n 
                       , after: (node l) : (after l)
                       , before: before l
                       , parents: parents l
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
    c:cs -> Loc { node: c 
                , before: r.before
                , after: cs
                , parents: r.parents
                }
    Nil -> 
      case r.before of
        c:cs -> Loc { node: c
                    , before: cs
                    , after: r.after
                    , parents: r.parents
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

-- | Search for the first occurence of the value `a` downwards and to the right.
findDown :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findDown a l@(Loc r) = 
  if a == (value l) 
    then (Just l) 
    else 
      case next l of
        Just n -> findDown a n
        Nothing ->
          case down l of
            Just n' -> findDown a n'
            Nothing -> Nothing            

-- | Search for the first occurence of the value `a` upwards and to the left.
findUp :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findUp a l@(Loc r) = 
  if a == (value l) 
    then (Just l) 
    else 
      case prev l of
        Just n -> findUp a n
        Nothing ->
          case up l of
            Just n' -> findUp a n'
            Nothing -> Nothing            

-- | Search for the first occurence of the value `a` starting from the root of
-- | the tree.
findFromRoot :: forall a. Eq a => a -> Loc a -> Maybe (Loc a)
findFromRoot a = (findDown a) <<< root

-- Setters and Getters
node :: forall a. Loc a -> Tree a 
node (Loc r) = r.node

value :: forall a. Loc a -> a
value = nodeValue <<< node

before :: forall a. Loc a -> Forest a
before (Loc r) = r.before

after :: forall a. Loc a -> Forest a
after (Loc r) = r.after

parents :: forall a. Loc a -> List (Loc a)
parents (Loc r) = r.parents

children :: forall a. Loc a -> Forest a
children = nodeChildren <<< node

siblings :: forall a. Loc a -> Forest a 
siblings (Loc r) = (reverse r.before) <> (r.node : r.after) 
