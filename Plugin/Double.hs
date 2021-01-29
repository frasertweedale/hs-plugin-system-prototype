module Plugin.Double where

import Plugin

plugin :: Plugin Pure
plugin = Plugin "Double" $ pure . (*2)
