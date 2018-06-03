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
import CorePrep (corePrepPgm)
import CoreToStg (coreToStg)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GhcPlugins
  ( CoreM
  , CoreToDo (..)
  , CommandLineOption
  , ModGuts (..)
  , ModLocation (..)
  , Plugin (..)
  , defaultPlugin
  , errorMsgS
  , getDynFlags
  , getHscEnv
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
    ghcEnv <- getHscEnv
    let ModGuts { mg_module=gutsModule
                , mg_binds=gutsProg
                , mg_tcs=tycons
                } = mod
        modName = moduleNameString $ moduleName gutsModule
        fileName = outdir </> modName <.> "dump-stg-simple"
        dummyLoc = ModLocation Nothing "dummy.hi" "dummy.o"
    -- plugins only run during the Core2Core compiling phase.
    -- So we need to run the backend ourselves (see hscGenHardCode in GHC)

    -- Run final Core preparation: this is where eta-expansion happens.
    -- If we don't run it, the STG output will show top-level functions
    -- as "f = THUNK" instead of "f args = expr"
    -- beware: in GHC 8.4 corePrepPgm returns a tuple
    coreProg <- liftIO $ corePrepPgm ghcEnv gutsModule dummyLoc gutsProg tycons
    let stgProg = coreToStg dynFlags (mg_module mod) coreProg
        rendered = T.pack $ prettySTG modName stgProg
    liftIO $ B.writeFile fileName (TE.encodeUtf8 rendered)
    return mod
