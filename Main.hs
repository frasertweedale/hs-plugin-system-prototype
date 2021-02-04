module Main where

import System.IO (hFlush, stdout)
import Control.Monad.State

import Plugin
import qualified Plugin.Double
import qualified Plugin.FlipNegate
import qualified Plugin.ShootLasers
import qualified Plugin.Offset
import qualified Plugin.Noop

plugins :: [PluginDict]
plugins =
    addPlugin Plugin.Noop.plugin
  . addPlugin Plugin.Double.plugin
  . addPlugin (Plugin.Offset.mkPlugin 100)
  . addPlugin Plugin.FlipNegate.plugin
  . addPlugin Plugin.ShootLasers.plugin
  $ []

main :: IO ()
main = do
  -- read start values
  doNegate <- prompt "negate number? [True|False]"
  i <- prompt "number (Int)"

  (j, doNegate') <- flip runStateT doNegate $
    foldr (>=>) pure (fmap (getInputHook . inputHook) plugins) i

  -- print result
  let s = show (if doNegate' then negate j else j)
  s' <- foldr (>=>) pure (fmap (getDisplayHook . displayHook) plugins) s
  putStr "result: " *> putStrLn s'

  where
    prompt s = putStr (s <> ": ") *> hFlush stdout *> readLn
