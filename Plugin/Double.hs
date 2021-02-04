module Plugin.Double where

import Plugin

plugin :: Plugin (InputHook Pure)
plugin = Plugin "Double" (InputHook (pure . (*2)))
