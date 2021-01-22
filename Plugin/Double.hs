module Plugin.Double where

import Plugin

plugin :: Plugin Pure
plugin = Plugin $ pure . (*2)
