-| A GHC plugin dumping the optimizer output (Core program)
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
> install args passes = do

The plugin takes an 'outdir' argument, specifying an output
directory. Default value is "dump-simple".

>    outdir <- case args of
>               []  -> return $ Just "dump-simple"
>               [o] -> return $ Just o
>               _   -> errorMsgS "too many arguments in DumpCore" >> return Nothing
>    case outdir of
>       Nothing  -> return passes
>       Just dir -> let pass = CoreDoPluginPass "DumpCore-Simple"
>                            $ prettyprintTo dir
>                   in return (passes ++ [pass])

Dumped representation will be written to $outdir/Module.Name.dump-core-simple

> prettyprintTo :: FilePath -> ModGuts -> CoreM ModGuts
> prettyprintTo outdir mod = liftIO $ do
>     createDirectoryIfMissing True outdir
>     let modName = moduleNameString $ moduleName (mg_module mod)
>         fileName = outdir </> modName <.> "dump-core-simple"
>         rendered = T.pack $ prettySimple modName (mg_binds mod)
>     B.writeFile fileName (TE.encodeUtf8 rendered)
>     return mod
