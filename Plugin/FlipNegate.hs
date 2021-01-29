module Plugin.FlipNegate where

import Control.Monad.State (modify)
import Plugin

plugin :: Plugin CanRWState
plugin = Plugin "FlipNegate" $ \i -> i <$ modify not
