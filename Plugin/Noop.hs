module Plugin.Noop where

import Plugin

plugin :: Plugin Pure
plugin = Plugin "Noop" $ pure
