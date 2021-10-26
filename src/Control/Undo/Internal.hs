{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections     #-}

-- | For internal use only. PVP compliance is forfeited if you import this module.
module Control.Undo.Internal where

import           Control.Monad.State
import           Data.Functor
import           Data.List
import           Data.Maybe

-- | A data structure describing all past, present, and future states that have been recorded.
data Undo u = Undo
  { cursor   :: u
  , children :: [Undo u]
  , parent   :: Maybe (Undo u)
  } deriving (Functor, Foldable, Traversable)

-- | Create an 'Undo' from a single state.
singleton :: u -> Undo u
singleton u = Undo u [] Nothing

-- | Add a new state to an 'Undo', navigating to it.
push :: u -> Undo u -> Undo u
push u node = let
  child = Undo u [] (Just node { children = child : children node })
  in child

-- | Get the present state from an 'Undo'.
current :: Undo u -> u
current = cursor

-- | Undo the current action. Can fail if there is no past state.
undo :: Undo u -> Maybe (Undo u)
undo = parent

-- | Like 'undo' but uses the current state if no past state exists.
undo' :: Undo u -> Undo u
undo' = fromMaybe <*> undo

-- | Recursively yield all previous states in this branch in reverse chronological order.
history :: Undo u -> [u]
history = unfoldr (\tree -> parent tree <&> \parent -> (cursor parent, parent))

-- | Go to a future state by index. Can fail if the index does not exist.
redo :: Int -> Undo u -> Maybe (Undo u)
redo idx node = children node !!? idx

-- | Go to the last seen future state. Can fail if the index does not exist.
redoFirst :: Undo u -> Maybe (Undo u)
redoFirst = redo 0

-- | Like 'redoFirst' but returns the current state if the lookup fails.
redoFirst' :: Undo u -> Undo u
redoFirst' = redo' 0

-- | Go to a future state by predicate. Can fail if the no state satisfies the predicate.
redoBy :: (u -> Bool) -> Undo u -> Maybe (Undo u)
redoBy p node = find (p . cursor) (children node)

-- | Like 'redo' but returns the current state if the lookup fails.
redo' :: Int -> Undo u -> Undo u
redo' idx = fromMaybe <*> redo idx

-- | Like 'redoBy' but returns the current state if no state satisfies the predicate.
redoBy' :: (u -> Bool) -> Undo u -> Undo u
redoBy' p = fromMaybe <*> redoBy p

-- | See all immediate future states.
redos :: Undo u -> [Undo u]
redos = children

-- | Safe indexing operation.
(!!?) :: [u] -> Int -> Maybe u
(!!?)= foldr (\x f k -> if k == 0 then Just x else f $! k - 1) (const Nothing)
