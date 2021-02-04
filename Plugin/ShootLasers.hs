module Plugin.ShootLasers where

import Data.List (intersperse)
import Control.Monad.IO.Class (liftIO)
import Plugin

plugin :: Plugin (InputHook CanIO, DisplayHook Pure)
plugin = Plugin "ShootLasers"
  ( InputHook $ \i -> i <$ liftIO (putStrLn "pew! pew!")
  , DisplayHook $ pure . intersperse ' '
  )
