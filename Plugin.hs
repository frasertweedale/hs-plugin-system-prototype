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

  , mkPluginDict
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
data PluginDict = PluginDict
  { pluginName :: String
  , inputHook :: InputHook Unconstrained
  , displayHook :: DisplayHook CanIO
  }

setInputHook :: InputHook Unconstrained -> PluginDict -> PluginDict
setInputHook hook plug = plug { inputHook = hook }

setDisplayHook :: DisplayHook CanIO -> PluginDict -> PluginDict
setDisplayHook hook plug = plug { displayHook = hook }

newPlugin :: String -> PluginDict
newPlugin n = PluginDict n (InputHook pure) (DisplayHook pure)


class Hook t where
  setHook :: t -> PluginDict -> PluginDict

-- requires MonoLocalBinds (to silence warnings)
instance (forall m. Unconstrained m => ctx m) => Hook (InputHook ctx) where
  setHook (InputHook f) = setInputHook (InputHook f)

instance (forall m. CanIO m => ctx m) => Hook (DisplayHook ctx) where
  setHook (DisplayHook f) = setDisplayHook (DisplayHook f)

instance (Hook h1, Hook h2) => Hook (h1, h2) where
  setHook (h1, h2) = setHook h1 . setHook h2

instance Hook () where
  setHook _ = id

data Plugin hooks = Plugin String hooks

mkPluginDict :: (Hook hooks) => Plugin hooks -> PluginDict
mkPluginDict (Plugin name hook) = setHook hook (newPlugin name)

addPlugin
  :: (Hook hooks)
  => Plugin hooks -> [PluginDict] -> [PluginDict]
addPlugin = (:) . mkPluginDict
