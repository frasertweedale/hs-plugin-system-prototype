module Plugin.Offset where

import Plugin

mkPlugin :: Int -> Plugin Pure
mkPlugin offset = Plugin "Offset" $ pure . (+ offset)
