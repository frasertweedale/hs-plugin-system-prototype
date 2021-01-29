import System.IO (hFlush, stdout)
import Control.Monad.State

import Plugin
import qualified Plugin.Double
import qualified Plugin.FlipNegate
import qualified Plugin.ShootLasers
import qualified Plugin.Offset
import qualified Plugin.Noop

plugins :: [Plugin Unconstrained]
plugins =
  [ relax Plugin.Noop.plugin
  , relax (Plugin.Double.plugin :: Plugin Pure)
  , relax (Plugin.Offset.mkPlugin 100)
  , relax Plugin.FlipNegate.plugin
  , relax Plugin.ShootLasers.plugin
  ]

main :: IO ()
main = do
  -- read start values
  doNegate <- prompt "negate number? [True|False]"
  i <- prompt "number (Int)"

  (j, doNegate') <- flip runStateT doNegate $
    foldr (>=>) pure (fmap pluginHook plugins) i

  -- print result
  putStr "result: " *> print (if doNegate' then negate j else j)

  where
    prompt s = putStr (s <> ": ") *> hFlush stdout *> readLn
