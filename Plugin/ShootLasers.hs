module Plugin.ShootLasers where

import Control.Monad.IO.Class (liftIO)
import Plugin

plugin :: Plugin CanIO
plugin = Plugin $ \i -> i <$ liftIO (putStrLn "pew! pew!")
