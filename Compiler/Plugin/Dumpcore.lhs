| A GHC plugin dumping the optimizer output (Core program)
with a simplified syntax in a given directory.

Expected usage:

    -fplugin=Compiler.Plugin.Dumpcore
    -fplugin-opt=Compiler.Plugin.Dumpcore:dump-core-simple/

Output is named '$outdir/Module.Name.dump-core-simple'

> module Compiler.Plugin.Dumpcore (plugin) where

> import Compiler.Plugin.Dumpcore.Pretty
> import qualified Data.ByteString as B
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as TE
> import GhcPlugins
>   ( CoreM
>   , CoreToDo (..)
>   , CommandLineOption
>   , ModGuts (..)
>   , Plugin (..)
>   , defaultPlugin
>   , errorMsgS
>   , liftIO
>   , moduleName
>   , moduleNameString
>   , putMsgS
>   )
> import Prelude hiding (mod)
> import System.Directory
> import System.FilePath

GHC plugins must export a plugin which modifies the list
of compiler passes.

> plugin :: Plugin
> plugin = defaultPlugin
>        { installCoreToDos = install
>        }

Our compiler pass will insert itself at the very end of the
pipeline.

> install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
> install args passes =
>   case args of

The plugin takes a mandatory 'outdir' argument, specifying an output
directory.

>     outdir:_ -> do
>          let pass mod = do
>                  putMsgS "hello!"
>                  prettyprintTo outdir mod
>          return (passes ++ [ CoreDoPluginPass "DumpCore-Simple" pass ])

Missing directory argument causes the plugin to be a no-op.

>     _ -> do
>          errorMsgS "please specify an output directory for Compiler.Plugin.Dumpcore"
>          return passes

Dumped representation will be written to $outdir/Module.Name.dump-core-simple

> prettyprintTo :: FilePath -> ModGuts -> CoreM ModGuts
> prettyprintTo outdir mod = liftIO $ do
>     createDirectoryIfMissing True outdir
>     let modName = moduleNameString $ moduleName (mg_module mod)
>         fileName = outdir </> modName <.> "dump-stg-simple"
>         rendered = T.pack $ prettySimple modName (mg_binds mod)
>     B.writeFile fileName (TE.encodeUtf8 rendered)
>     return mod
