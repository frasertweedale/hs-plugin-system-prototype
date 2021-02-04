module Plugin.Offset where

import Plugin

mkPlugin :: Int -> Plugin (InputHook Pure)
mkPlugin offset = Plugin "Offset" (InputHook (pure . (+ offset)))
