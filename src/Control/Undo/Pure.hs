-- |
-- Module      : Control.Undo.Pure
-- Copyright   : © MorrowM, 2021
-- License     : MIT
-- Maintainer  : themorrowm@gmail.com
-- Stability   : stable
--
-- Pure operations on a branching, navigable state.
module Control.Undo.Pure
  ( -- * The Undo tree
    Undo
  , singleton
  , push
  , current
  , -- * Undoing
    undo
  , undo'
  , history
  , -- * Redoing
    redo
  , redoFirst
  , redoFirst'
  , redoBy
  , redo'
  , redoBy'
  , redos
  ) where

import           Control.Undo.Internal


