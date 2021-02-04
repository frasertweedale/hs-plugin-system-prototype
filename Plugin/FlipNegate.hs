module Plugin.FlipNegate where

import Control.Monad.State (modify)
import Plugin

plugin :: Plugin (InputHook CanRWState)
plugin = Plugin "FlipNegate" (InputHook (\i -> i <$ modify not))
