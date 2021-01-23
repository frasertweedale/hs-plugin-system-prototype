import Control.Monad.State

import Plugin
import qualified Plugin.Double
import qualified Plugin.FlipNegate
import qualified Plugin.ShootLasers
import qualified Plugin.Offset

plugins :: [Plugin Unconstrained]
plugins =
  [ relax Plugin.FlipNegate.plugin
  , relax Plugin.ShootLasers.plugin
  , relax (Plugin.Double.plugin :: Plugin Pure)
  , relax (Plugin.Offset.mkPlugin 100)
  ]

main :: IO ()
main = do
  -- read start values
  doNegate <- putStr "negate number? [True|False]: " *> readLn
  i <- putStr "number (Int): " *> readLn

  (j, doNegate') <- flip runStateT doNegate $
    foldr (>=>) pure (fmap unPlugin plugins) i

  -- print result
  putStr "result: " *> print (if doNegate' then negate j else j)

  where
