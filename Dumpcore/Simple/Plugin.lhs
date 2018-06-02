> module Dumpcore.Simple.Plugin (plugin) where

> import GhcPlugins

GHC plugins must export a plugin which modifies the list
of compiler passes.

> plugin :: Plugin
> plugin = defaultPlugin
>        { installCoreToDos = install
>        }

> install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
> install args passes =
>   return (passes ++ [ CoreDoPluginPass "DumpCore-Simple" prettyprint ])

> prettyprint :: ModGuts -> CoreM ModGuts
> prettyprint mod = return mod
