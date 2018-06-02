-- | A GHC plugin dumping a simplified, more human-readable of
-- the STG intermediate representation.
--
-- Expected usage
--
--    -fplugin=Compiler.Plugin.DumpSTG
--    -fplugin-opt=Compiler.Plugin.DumpSTG:dump-stg-simple/
--
-- Output is named '$outdir/Module.Name.dump-stg-simple'
module Compiler.Plugin.DumpSTG (plugin) where

import Compiler.Plugin.DumpSTG.Pretty
import CoreToStg (coreToStg)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GhcPlugins
  ( CoreM
  , CoreToDo (..)
  , CommandLineOption
  , ModGuts (..)
  , Plugin (..)
  , defaultPlugin
  , errorMsgS
  , getDynFlags
  , liftIO
  , moduleName
  , moduleNameString
  )
import Prelude hiding (mod)
import System.Directory
import System.FilePath

-- | The compiler plugin
plugin :: Plugin
plugin = defaultPlugin
       { installCoreToDos = install
       }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args passes = do
    outdir <- case args of
               []  -> return $ Just "dump-simple"
               [o] -> return $ Just o
               _   -> errorMsgS "too many arguments in DumpSTG" >> return Nothing
    case outdir of
       Nothing  -> return passes
       Just dir -> let pass = CoreDoPluginPass "DumpSTG-Simple"
                            $ prettyprintTo dir
                   in return (passes ++ [pass])

prettyprintTo :: FilePath -> ModGuts -> CoreM ModGuts
prettyprintTo outdir mod = do
    liftIO $ createDirectoryIfMissing True outdir
    dynFlags <- getDynFlags
    let modName = moduleNameString $ moduleName (mg_module mod)
        fileName = outdir </> modName <.> "dump-stg-simple"
        stgProg = coreToStg dynFlags (mg_module mod) (mg_binds mod)
        rendered = T.pack $ prettySTG modName stgProg
    liftIO $ B.writeFile fileName (TE.encodeUtf8 rendered)
    return mod
