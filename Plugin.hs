{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Plugin
  ( Plugin(..)
  , addPlugin
  , PluginDict(..)

  , InputHook(..)
  , DisplayHook(..)

  , Pure
  , CanRWState
  , CanIO
  , Unconstrained
  ) where

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

newtype InputHook ctx =
  InputHook { getInputHook :: forall m. (ctx m) => Int -> m Int }

newtype DisplayHook ctx =
  DisplayHook { getDisplayHook :: forall m. (ctx m) => String -> m String }

-- ConstraintKinds required
data PluginDict ctx = PluginDict
  { pluginName :: String
  , inputHook :: InputHook ctx
  , displayHook :: DisplayHook ctx
  }

setInputHook :: InputHook ctx -> PluginDict ctx -> PluginDict ctx
setInputHook hook plug = plug { inputHook = hook }

setDisplayHook :: DisplayHook ctx -> PluginDict ctx -> PluginDict ctx
setDisplayHook hook plug = plug { displayHook = hook }

newPlugin :: (forall m. ctx m => Pure m) => String -> PluginDict ctx
newPlugin n = PluginDict n (InputHook pure) (DisplayHook pure)


class Hook t where
  setHook :: t -> PluginDict Unconstrained -> PluginDict Unconstrained

-- requires MonoLocalBinds (to silence warnings)
instance (forall m. Unconstrained m => ctx m) => Hook (InputHook ctx) where
  setHook (InputHook f) = setInputHook (InputHook f)

instance (forall m. Unconstrained m => ctx m) => Hook (DisplayHook ctx) where
  setHook (DisplayHook f) = setDisplayHook (DisplayHook f)

instance (Hook h1, Hook h2) => Hook (h1, h2) where
  setHook (h1, h2) = setHook h1 . setHook h2

instance Hook () where
  setHook _ = id

data Plugin hooks = Plugin String hooks

mkPluginDict :: (Hook hooks) => Plugin hooks -> PluginDict Unconstrained
mkPluginDict (Plugin name hook) = setHook hook (newPlugin name)

addPlugin
  :: (Hook hooks)
  => Plugin hooks -> [PluginDict Unconstrained] -> [PluginDict Unconstrained]
addPlugin = (:) . mkPluginDict
