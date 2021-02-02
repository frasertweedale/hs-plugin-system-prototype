{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Plugin where

import Control.Monad.State
import Control.Monad.Except

data Error = Error

-- synonyms for convenience
type Pure = Applicative
type CanRWState = MonadState Bool
type CanIO = MonadIO
type CanError = MonadError Error

class (CanRWState m, CanIO m) => Unconstrained m where
-- requires FlexibleInstances and UndecidableInstances
instance (CanRWState m, CanIO m) => Unconstrained m where


-- ConstraintKinds required
data Plugin ctx = Plugin
  { pluginName :: String
  , pluginHook :: forall m. (ctx m) => Int -> m Int
  }

-- QuantifiedConstraints required
relax
  :: (forall m. ctx' m => ctx m)
  => Plugin ctx -> Plugin ctx'
relax (Plugin n fs) = Plugin n fs

addPlugin
  :: (forall m. ctx' m => ctx m)
  => Plugin ctx -> [Plugin ctx'] -> [Plugin ctx']
addPlugin = (:) . relax

newPlugin :: (forall m. ctx m => Pure m) => String -> Plugin ctx
newPlugin n = Plugin n pure
