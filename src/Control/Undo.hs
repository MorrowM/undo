{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Control.Undo
-- Copyright   : © MorrowM, 2021
-- License     : MIT
-- Maintainer  : themorrowm@gmail.com
-- Stability   : stable
--
-- 'UndoM' is a monad that describes a branching state that can be navigated.
-- For example, it can be useful for a clipboard history tree, or navigating a filesystem while
-- remembering all the visited directories.
--
-- == Example usage: A simple shell for navigating directories.
--
-- > import           Control.Monad
-- > import           Control.Monad.IO.Class
-- > import           Control.Monad.Trans.Class
-- > import           Control.Undo
-- > import           Data.Functor
-- > import           Data.List
-- > import           Data.Maybe
-- > import           System.Console.Haskeline
-- > import           System.Directory
-- > import           System.IO
-- > import           Text.Read                 (readMaybe)
-- >
-- > prog :: UndoT FilePath (InputT IO) ()
-- > prog = forever $ do
-- >   cwd <- currentM
-- >   command <- lift $ getInputLine $ cwd <> " $ "
-- >   case words  <$> command of
-- >     Just ["cd", s] -> do
-- >       b <- liftIO $ doesDirectoryExist s
-- >       if b
-- >         then do
-- >           liftIO $ setCurrentDirectory s
-- >           pushM =<< liftIO getCurrentDirectory
-- >         else lift $ outputStrLn "error: directory does not exist"
-- >     Just ["pwd"] -> currentM >>= lift . outputStrLn
-- >     Just ["ls"] -> currentM >>= liftIO . listDirectory >>= lift . outputStr . unlines . sort
-- >     Just ["undo"] -> do
-- >       loc <- undoM
-- >       when (isNothing loc) $
-- >         lift . outputStr $  "error: already at earliest point"
-- >     Just ["listf"] -> do
-- >       vals <- redoListM
-- >       lift . outputStr . unlines $ [show i <> ". " <> s | (i, s) <- zip [1..] vals]
-- >     Just ["redo", i] -> case readMaybe i of
-- >       Nothing -> lift $ outputStrLn "error: invalid index"
-- >       Just idx -> do
-- >         loc <- redoM (idx - 1)
-- >         when (isNothing loc) $
-- >           lift . outputStr $  "error: failed to go redo to index " <> i
-- >     Just ["history"] -> historyM >>= lift . outputStr . unlines . reverse
-- >     _ -> lift $ outputStrLn "error: unknown command"
-- >
-- > main :: IO ()
-- > main = runInputT defaultSettings . runUndoT prog =<< getCurrentDirectory

module Control.Undo
  ( -- * Types
    UndoT
  , runUndoT
  , UndoM
  , runUndoM
  , -- * Navigation commands
    currentM
  , pushM
  , -- * Moving undowards
    undoM
  , undoM'
  , undoPeekM
  , historyM
  , -- * Moving redo
    redoM
  , redoM'
  , redoByM
  , redoByM'
  , redoPeekM
  , redoPeekByM
  , redosM
  , -- * Lower level operations
    Undo
  , getUndo
  , setUndo
  , modifyUndo
  ) where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Undo.Internal
import           Data.Functor
import           Data.List
import           Data.Maybe
import           System.IO
import           Text.Read              (readMaybe)

-- | A monad transformer that provides a navigable branching state.
newtype UndoT u m a = UndoT (StateT (Undo u) m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | A concrete monad that provides a navigable branching state.
type UndoM u = UndoT u Identity

-- | Run an `UndoT` layer when provided an initial state.
runUndoT :: Monad m => UndoT u m a -> u -> m a
runUndoT (UndoT st) u = evalStateT st (singleton u)

-- | Run the 'UndoM' monad when provided a initial state.
runUndoM :: UndoM u a -> u -> a
runUndoM u = runIdentity . runUndoT u

-- | Get the current state.
currentM :: Monad m => UndoT u m u
currentM = UndoT $ gets cursor

-- | Modify the current state.
modifyCurrentM :: Monad m => (u -> u) -> UndoT u m ()
modifyCurrentM f = UndoT $ modify (\tree -> tree { cursor = f (cursor tree) })

-- | Add a new state, navigating to it.
pushM :: Monad m => u -> UndoT u m ()
pushM u = UndoT $ modify (push u)

-- | Go to the previous state and yield it. Returns 'Nothing' if there is no previous state.
undoM :: Monad m => UndoT u m (Maybe u)
undoM = navigateWith undo

-- | Like 'undo' but uses the current state if no previous state exists.
undoM' :: Monad m => UndoT u m u
undoM' = UndoT (modify undo') *> currentM

-- | Yield the previous state. Returns 'Nothing' if none exists.
undoPeekM :: Monad m => UndoT u m (Maybe u)
undoPeekM = UndoT $ gets (fmap cursor . undo)

-- | Recursively yield all previous states in this branch in reverse chronological order.
historyM :: Monad m => UndoT u m [u]
historyM = history <$> getUndo

navigateWith :: Monad m => (Undo u -> Maybe (Undo u)) -> UndoT u m (Maybe u)
navigateWith f = UndoT $ do
  st <- get
  case f st of
    Nothing  -> pure Nothing
    Just st' -> Just (cursor st') <$ put st'

-- | Go to the next state after having gone 'undo'. Takes an index.
-- States are ordered in reverse creation order. So for example, if the
-- state @a@ is created and then the state moves 'undo' and then the state
-- @b@ is created and then the state moves 'undo', the state @a@ will have index @1@
-- and the state @b@ will have index @0@.
--
-- Yields 'Just' the new state if successful, 'Nothing' otherwise.
redoM :: Monad m => Int -> UndoT u m (Maybe u)
redoM = navigateWith . redo

-- | Like 'redo', but uses a predicate rather than an index. Selects the first
-- state that matches (ordered by ascending index).
redoByM :: Monad m => (u -> Bool) -> UndoT u m (Maybe u)
redoByM = navigateWith . redoBy

-- | Like 'redo', but yields the current state if unsuccessful.
redoM' ::  Monad m => Int -> UndoT u m u
redoM' idx = UndoT (modify (redo' idx)) *> currentM

-- | Like 'redoBy', but yields the current state if unsuccessful.
redoByM' ::  Monad m => (u -> Bool) -> UndoT u m u
redoByM' p = UndoT (modify (redoBy' p)) *> currentM

-- | View a future state without navigating to it.
redoPeekM :: Monad m => Int -> UndoT u m (Maybe u)
redoPeekM idx = UndoT $ gets (fmap cursor . redo idx)

-- | Like 'redoPeek', but uses a predicate.
redoPeekByM :: Monad m => (u -> Bool) -> UndoT u m (Maybe u)
redoPeekByM p = UndoT $ gets (fmap cursor . redoBy p)

-- | List all the possible states to move children to, one level deep.
redosM :: Monad m => UndoT u m [u]
redosM = UndoT $ gets (fmap cursor . children)

-- | Get the current 'Undo' from this node.
getUndo :: Monad m => UndoT u m (Undo u)
getUndo = UndoT get

-- | Set the current 'Undo' from this node.
setUndo :: Monad m => Undo u -> UndoT u m ()
setUndo = UndoT . put

-- | Modify the current 'Undo' from this node.
modifyUndo :: Monad m => (Undo u -> Undo u) -> UndoT u m ()
modifyUndo = UndoT . modify
