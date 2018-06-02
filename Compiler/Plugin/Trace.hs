-- | A simple dummy plugin printing compilation start and end time.
module Compiler.Plugin.Trace (plugin) where

import GhcPlugins
import Data.Time.LocalTime
import Data.Time.Format
import Prelude hiding (mod)

plugin :: Plugin
plugin = defaultPlugin
       { installCoreToDos = install
       }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ passes = return ([pass1] ++ passes ++ [pass2])
   where pass1 = CoreDoPluginPass "trace-start" $ \mod -> do
                     t0 <- liftIO getZonedTime
                     putMsgS (fmt t0 ++ " start " ++ modName mod)
                     return mod
         pass2 = CoreDoPluginPass "trace-end" $ \mod -> do
                     t0 <- liftIO getZonedTime
                     putMsgS (fmt t0 ++ " end " ++ modName mod)
                     return mod
         modName mod = moduleNameString $ moduleName $ mg_module mod
         fmt t = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q" t

